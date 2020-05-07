module Main where

import GHC
import GHC.LanguageExtensions.Type
import DynFlags
import Outputable
import Pretty

import Data.Functor (void)
import Data.List (break)

import Control.Exception (finally)
import Control.Monad (when)
import Control.Monad.IO.Class

import System.IO (stdout)
import System.Environment (getArgs, setEnv)
import System.Exit (exitSuccess, exitFailure)
import System.Directory (getHomeDirectory, doesFileExist, removeFile)

import GHC.Paths (libdir)

import Unsafe.Coerce (unsafeCoerce)

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
    -- test compile template
    (sflagTemplate,_) <- compileFiles configDir envFile [template]
    reportOutcome "template" sflagTemplate
    -- test compile solution and tests
    (sflagSolution,env) <- compileFiles configDir envFile [solution, tests]
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

compileFiles :: FilePath -> Maybe FilePath -> [FilePath] -> IO (SuccessFlag,HscEnv)
compileFiles configDir env fs =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                  , ghcLink   = LinkInMemory
                                  , packageEnv = env
                                  }

      addTargetFile $ configDir ++ "/TestHelper.hs"
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
  addTargetFile $ configDir ++ "/TestHarness.hs"
  load LoadAllTargets
  -- compile test runner
  setContext
    [ IIDecl $ simpleImportDecl (mkModuleName "Prelude")
    , IIDecl $ simpleImportDecl (mkModuleName "Test.HUnit.Base")
    , IIDecl $ simpleImportDecl (mkModuleName "Test")
    , IIDecl $ simpleImportDecl (mkModuleName "TestHarness")
    ]
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
