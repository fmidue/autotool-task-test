{-# LANGUAGE LambdaCase #-}
module Main where

import GHC
import DynFlags
import OccName

import Data.List (find)

import Control.Exception (finally)
import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Extra (unlessM)

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.Directory (doesFileExist, removeFile, getAppUserDataDirectory, createDirectoryIfMissing)

import GHC.Paths (libdir)

import Unsafe.Coerce (unsafeCoerce)

import Haskell.Template.FileContents (testHelperContents, testHarnessContents)
import Digraph
import Test.HUnit (Counts(..))

{-
  GHC related code follows Stephen Diehl's "Dive into GHC" Overview.
  http://www.stephendiehl.com/posts/ghc_01.html
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [task, solution] -> runMain task solution Nothing
    (task:solution:"--type-holes":tys) -> runMain task solution $ Just tys

runMain :: FilePath -> FilePath -> Maybe [String] -> IO ()
runMain task solution typeHoles = do
  (template,tests) <- splitTask task typeHoles
  flip finally (mapM_ removeFile (template:tests)) $ do
    appData <- getAppUserDataDirectory "test-task"
    setupHelperAndHarness appData
    pkgEnvExists <- doesFileExist $ appData ++ "/pkg-env"
    let envFile =
          if pkgEnvExists
            then Just $ appData ++ "/pkg-env"
            else Nothing
    env <- setupEnv envFile
    -- test compile template
    -- don't load the first (primary) test module but load other hidden modules so that the template can import them
    (sflagTemplate,_) <- compileFiles env $ template : tail tests
    reportOutcome "template" sflagTemplate
    -- test compile solution and tests
    (sflagSolution,env) <- compileFiles env $ [appData ++ "/TestHelper", appData ++ "/TestHarness", solution] ++ tests
    reportOutcome "solution and tests" sflagSolution
    testRes <- testFiles env
    case testRes of
      Just err -> do
        putStrLn "testing solution failed:"
        putStrLn err
        exitFailure
      Nothing -> putStrLn "successfully tested solution"
    exitSuccess

setupHelperAndHarness :: FilePath -> IO ()
setupHelperAndHarness appData = do
  createDirectoryIfMissing True appData
  unlessM (doesFileExist $ appData ++ "/TestHelper.hs") $
    writeFile (appData ++ "/TestHelper.hs") testHelperContents
  unlessM (doesFileExist $ appData ++ "/TestHarness.hs") $
    writeFile (appData ++ "/TestHarness.hs") testHarnessContents

reportOutcome :: String -> SuccessFlag -> IO ()
reportOutcome target Succeeded =
  putStrLn $ target ++ " complilation successfull"
reportOutcome target Failed = do
  putStrLn "Error locations are relative to configuration boundaries"
  putStrLn $ target ++ " compilation failed"
  exitFailure

setupEnv :: Maybe FilePath -> IO HscEnv
setupEnv env = defaultErrorHandler defaultFatalMessager defaultFlushOut $
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    liftIO $ putStrLn $ "using ghc version: " ++ ghcNameVersion_projectVersion (ghcNameVersion dflags)
    setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                , ghcLink   = LinkInMemory
                                , packageEnv = env
                                }
    getSession

compileFiles :: HscEnv -> [FilePath] -> IO (SuccessFlag,HscEnv)
compileFiles env fs =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      setSession env
      mapM_ addTargetFile fs
      sflag <- load LoadAllTargets
      env <- getSession
      return (sflag,env)

addTargetFile ::  GhcMonad m => FilePath -> m ()
addTargetFile file = do
  target <- guessTarget file Nothing
  addTarget target

type TestFailure = String

testFiles :: HscEnv -> IO (Maybe TestFailure)
testFiles env = runGhc (Just libdir) $ do
  setSession env
  -- look for a TaskXX.main function
  -- (specifically we are looking for the name of the first import in the hidden Test module)
  modGraph <- getModuleGraph
  let
    modules = topSortModuleGraph True modGraph Nothing
    testMod = find (\case {AcyclicSCC m -> (moduleNameString . ms_mod_name) m == "Test" ; _ -> False}) modules
    mModName = unLoc . snd . last . ms_textual_imps . head . flattenSCC <$> testMod
    mMod = (\name -> find (\case { AcyclicSCC m -> ms_mod_name m == name ; _ -> False}) modules) =<< mModName
  taskModuleName <- case mMod of
    Just ~(AcyclicSCC modSum) -> do
      topLevelScope <- join <$> modInfoTopLevelScope <$$> getModuleInfo (ms_mod modSum)
      let mainName = mkOccName varName "main"
          containsMain = maybe False ((mainName `elem`) . map occName) topLevelScope
          isCodeWorldTask = mkModuleName "CodeWorld" `elem` map (unLoc . snd) (ms_textual_imps modSum)
      return $
        if containsMain && not isCodeWorldTask
          then Just $ ms_mod_name modSum
          else Nothing
    Nothing -> return Nothing
  -- compile test runner
  setContext $
    [ IIDecl $ simpleImportDecl (mkModuleName "Prelude")
    , IIDecl $ simpleImportDecl (mkModuleName "Test.HUnit.Base")
    , IIDecl $ simpleImportDecl (mkModuleName "Test")
    , IIDecl $ simpleImportDecl (mkModuleName "TestHarness")
    ] ++
    maybe [] (\m -> [ IIDecl $ simpleImportDecl m ]) taskModuleName
  -- run public test suite (TaskXX.main) if present
  case taskModuleName of
    Just m -> do
      hValue <- compileExpr $ moduleNameString m ++ ".main"
      liftIO $ do
        putStrLn "found public test suite\nrunning main:"
        unsafeCoerce hValue
    Nothing -> pure ()
  -- run internal test suite
  liftIO $ putStrLn "running hidden test suite:"
  hValue <- compileExpr "TestHarness.run Test.test"
  let (Counts {failures=n},s) = unsafeCoerce hValue
  liftIO $ if n > 0
    then return (Just $ s [])
    else return Nothing

splitTask :: FilePath -> Maybe [String] ->  IO (FilePath,[FilePath])
splitTask file typeHoles = do
  (template,tests) <- splitConfig <$> readFile file
  let fileBaseName = take (length file - 3) file
      templateFile = fileBaseName ++ "-template.hs"
      testFiles = [fileBaseName ++ "-tests"++ show n ++ ".hs" | (n,_) <- zip [1..] tests]
  writeFile templateFile $ insertHoledTypes template typeHoles
  mapM_ (uncurry writeFile) $ zip testFiles tests
  return (templateFile,testFiles)

insertHoledTypes :: String -> Maybe [String] -> String
insertHoledTypes template Nothing = template
insertHoledTypes template (Just tys) =
  template ++ "\n" ++
  unlines (map ("data " ++) tys)

splitConfig :: String -> (String, [String])
splitConfig x =
  -- dropWhile discards the YAML part of the config
  let ~(ls1,_sep:ls2) = break isSep . tail . dropWhile (not . isSep) $ lines x
  in (unlines ls1, splitTests ls2)

splitTests :: [String] -> [String]
splitTests x = case break isSep x of
  (ls1,[]) -> [unlines ls1]
  (ls1,_sep:ls2) -> unlines ls1 : splitTests ls2

isSep :: String -> Bool
isSep ('-':'-':'-':_) = True
isSep _ = False

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
