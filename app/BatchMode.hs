module BatchMode where

import SingleFile

import Control.Monad (forM_)

import Data.Char (toLower)
import Data.Foldable (find)
import Data.List (isInfixOf)

import System.Directory (listDirectory)

runMain :: FilePath -> FilePath -> Maybe [String] -> IO ()
runMain taskFolder solutionFolder tys = do
  tasks <- listDirectory taskFolder
  solutions <- listDirectory solutionFolder
  forM_ (findConfigs tasks solutions) $ \(solution, mTask) -> do
    let solutionPath = solutionFolder ++ "/" ++ solution
    case mTask of
      Just task -> do
        let taskPath = taskFolder ++ "/" ++ task
        putStrLn $ "config: " ++ taskPath ++ " solution: " ++ solutionPath
        SingleFile.runMain taskPath solutionPath tys
      Nothing -> putStrLn $ "could not find task config for " ++ solutionPath
    putStrLn ""

findConfigs :: [FilePath] -> [FilePath] -> [(FilePath,Maybe FilePath)]
findConfigs tasks = map $ \s -> (s, findConfig s tasks)

findConfig :: FilePath -> [FilePath] -> Maybe FilePath
findConfig solution = find $ \t -> map toLower t `isInfixOf` map toLower solution
