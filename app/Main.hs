module Main where

import SingleFile (Result(..))
import qualified SingleFile (runMain)
import qualified BatchMode (runMain)


import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

{-
  GHC related code follows Stephen Diehl's "Dive into GHC" Overview.
  http://www.stephendiehl.com/posts/ghc_01.html
-}

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
    -- single file mode
    [task, solution] -> SingleFile.runMain task solution Nothing
    (task:solution:"--type-holes":tys) -> SingleFile.runMain task solution $ Just tys
    -- batch mode
    ["--batch-mode", taskFolder, soluitionFolder] -> BatchMode.runMain taskFolder soluitionFolder Nothing
    ("--batch-mode": taskFolder: soluitionFolder: "--type-holes":tys) -> BatchMode.runMain taskFolder soluitionFolder $ Just tys
    _ -> showUsage >> pure Failure
  case result of
    Success -> exitSuccess
    Failure -> exitFailure

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: test-task <path/to/taskConfig> <path/to/solution> [--type-holes <types>]"
  putStrLn "       test-task --batch-mode <path/to/tasks> <path/to/solutions> [--type-holes <types>]"
