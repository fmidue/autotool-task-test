module Main where

import qualified SingleFile
import qualified BatchMode


import System.Environment (getArgs)
import System.Exit (exitSuccess)

{-
  GHC related code follows Stephen Diehl's "Dive into GHC" Overview.
  http://www.stephendiehl.com/posts/ghc_01.html
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- single file mode
    [task, solution] -> SingleFile.runMain task solution Nothing
    (task:solution:"--type-holes":tys) -> SingleFile.runMain task solution $ Just tys
    -- batch mode
    ["--batch-mode", taskFolder, soluitionFolder] -> BatchMode.runMain taskFolder soluitionFolder Nothing
    ("--batch-mode": taskFolder: soluitionFolder: "--type-holes":tys) -> BatchMode.runMain taskFolder soluitionFolder $ Just tys
    _ -> showUsage
  exitSuccess

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: test-task <path/to/taskConfig> <path/to/solution> [--type-holes <types>]"
  putStrLn "       test-task --batch-mode <path/to/tasks> <path/to/solutions> [--type-holes <types>]"
