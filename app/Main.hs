module Main where

import GHC
import GHC.LanguageExtensions.Type
import DynFlags
import Outputable
import Pretty

import Data.Functor (void)
import Data.List (break)

import Control.Monad (when)
import Control.Monad.IO.Class

import System.IO (stdout)
import System.Environment (getArgs, setEnv)
import System.Exit (exitSuccess, exitFailure)
import System.Directory (getHomeDirectory, doesFileExist, removeFile)

import GHC.Paths (libdir)

{-
  All GHC related code follows Stephen Diehl's "Dive into GHC" Overview.
  http://www.stephendiehl.com/posts/ghc_01.html
-}

main :: IO ()
main = do
  [task] <- getArgs
  runMain task

runMain :: FilePath -> IO ()
runMain task = do
  (template,tests) <- splitTask task
  home <- getHomeDirectory
  let configDir = home ++ "/.test-task"
  pkgEnvExists <- doesFileExist $ configDir ++ "/pkg-env"
  let envFile =
        if pkgEnvExists
          then Just $ configDir ++ "/pkg-env"
          else Nothing
  sflag <- doGhcStuff configDir envFile [template, tests]
  mapM_ removeFile [template,tests]
  case sflag of
    Succeeded -> do
      putStrLn "Test complilation successfull"
      exitSuccess
    Failed -> do
      putStrLn "Error locations are relative to configuration boundaries"
      putStrLn "Test compilation failed"
      exitFailure

doGhcStuff :: FilePath -> Maybe FilePath -> [FilePath] -> IO SuccessFlag
doGhcStuff configDir env fs =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                  , ghcLink   = LinkInMemory
                                  , packageEnv = env
                                  }

      addTargetFile $ configDir ++ "/TestHelper.hs"
      mapM_ addTargetFile fs
      load LoadAllTargets

addTargetFile ::  GhcMonad m => FilePath -> m ()
addTargetFile file = do
  target <- guessTarget file Nothing
  addTarget target

splitTask :: FilePath -> IO (FilePath,FilePath)
splitTask file = do
  (template,tests) <- splitConfig <$> readFile file
  let templateFile = take ((length file) - 3) file ++ "-template.hs"
      testsFile = take ((length file) - 3) file ++ "-tests.hs"
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
