module Main where

import System.Directory
import System.Process

main :: IO ()
main = do
  putStrLn "copying current stack environment to config directory"
  appData <- getAppUserDataDirectory "test-task"
  dir <- getCurrentDirectory
  path <- readCreateProcess (proc "stack" ["path", "--ghc-package-path"]){cwd = Just dir} ""
  createDirectoryIfMissing True appData
  writeFile (appData ++ "/GHC_PACKAGE_PATH") (head $ lines path)
