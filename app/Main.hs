module Main where

import GHC
import GHC.LanguageExtensions.Type
import DynFlags
import Outputable
import Pretty
import OccName

import Data.Functor (void)
import Data.List (break,find)

import Control.Exception (finally)
import Control.Monad (join, when)
import Control.Monad.IO.Class

import System.IO (stdout)
import System.Environment (getArgs, setEnv)
import System.Exit (exitSuccess, exitFailure)
import System.Directory (getHomeDirectory, doesFileExist, removeFile)

import GHC.Paths (libdir)

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace
{-
  GHC related code follows Stephen Diehl's "Dive into GHC" Overview.
  http://www.stephendiehl.com/posts/ghc_01.html
-}

main :: IO ()
main = do
  [task, solution] <- getArgs
  runMain task solution

runMain :: FilePath -> FilePath -> IO ()
runMain task solution = do
  (template,tests) <- splitTask task
  flip finally (mapM_ removeFile [template,tests]) $ do
    home <- getHomeDirectory
    let configDir = home ++ "/.test-task"
    pkgEnvExists <- doesFileExist $ configDir ++ "/pkg-env"
    let envFile =
          if pkgEnvExists
            then Just $ configDir ++ "/pkg-env"
            else Nothing
    env <- setupEnv envFile
    -- test compile template
    (sflagTemplate,_) <- compileFiles env [template]
    reportOutcome "template" sflagTemplate
    -- test compile solution and tests
    (sflagSolution,env) <- compileFiles env [configDir ++ "/TestHelper", configDir ++ "/TestHarness", solution, tests]
    reportOutcome "solution and tests" sflagSolution
    testRes <- testFiles env configDir
    case testRes of
      Just err -> do
        putStrLn "testing solution failed:"
        putStrLn err
        exitFailure
      Nothing -> putStrLn "successfully tested solution"
    exitSuccess

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

testFiles :: HscEnv -> FilePath -> IO (Maybe TestFailure)
testFiles env configDir = runGhc (Just libdir) $ do
  setSession env
  -- look for a Main.main function
  modules <- map ms_mod . mgModSummaries <$> getModuleGraph
  let mMod = find ((mkModuleName "Main" ==) . moduleName) modules
  hasMain <- case mMod of
    Just mod -> do
      topLevelScope <- join <$> modInfoTopLevelScope <$$> getModuleInfo mod
      let mainName = mkOccName varName "main"
      return $ maybe False ((mainName `elem`) . map occName) topLevelScope
    Nothing -> return False
  -- compile test runner
  setContext $
    [ IIDecl $ simpleImportDecl (mkModuleName "Prelude")
    , IIDecl $ simpleImportDecl (mkModuleName "Test.HUnit.Base")
    , IIDecl $ simpleImportDecl (mkModuleName "Test")
    , IIDecl $ simpleImportDecl (mkModuleName "TestHarness")
    ] ++
    [ IIDecl $ simpleImportDecl (mkModuleName "Main") | hasMain ]
  -- run public test suite (Main.main) if present
  when hasMain $ do
    traceM "!"
    hValue <- compileExpr "Main.main"
    liftIO $ do
      putStrLn "found public test suite\nrunning Main.main:"
      unsafeCoerce hValue
  -- run internal test suite
  hValue <- compileExpr $
    "let (Counts {failures=n},s) = TestHarness.run Test.test"
    ++ " in if n > 0 then return (Just $ s []) else (return Nothing :: IO (Maybe String))"
  liftIO (unsafeCoerce hValue)

splitTask :: FilePath -> IO (FilePath,FilePath)
splitTask file = do
  (template,tests) <- splitConfig <$> readFile file
  let templateFile = take (length file - 3) file ++ "-template.hs"
      testsFile = take (length file - 3) file ++ "-tests.hs"
  writeFile templateFile template
  writeFile testsFile tests
  return (templateFile,testsFile)

splitConfig :: String -> (String, String)
splitConfig x =
  let (ls1,ls2) = break isSep . tail . dropWhile (not . isSep) $ lines x
  in (unlines ls1,unlines $ tail ls2)
  where
    isSep ('-':'-':'-':_) = True
    isSep _ = False

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
