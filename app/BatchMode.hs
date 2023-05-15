module BatchMode where

import SingleFile

import Control.Monad (forM)

import Data.Char (toLower)
import Data.Foldable (find)
import Data.List (isInfixOf)

import System.Directory (listDirectory)
import System.FilePath (dropExtension)

runMain :: FilePath -> FilePath -> Maybe [String] -> IO Result
runMain taskFolder solutionFolder tys = do
  tasks <- listDirectory taskFolder
  solutions <- listDirectory solutionFolder
  mconcat <$> forM (findConfigs tasks solutions) (\(solution, mTask) ->
    let solutionPath = solutionFolder ++ "/" ++ solution in
    case mTask of
      Just task -> do
        let taskPath = taskFolder ++ "/" ++ task
        putStrLn $ "config: " ++ taskPath ++ " solution: " ++ solutionPath
        SingleFile.runMain taskPath solutionPath tys
      Nothing -> do
        putStrLn $ "could not find task config for " ++ solutionPath
        pure Failure
    <* putStrLn "")

findConfigs :: [FilePath] -> [FilePath] -> [(FilePath,Maybe FilePath)]
findConfigs tasks = map $ \s -> (s, findConfig s tasks)

findConfig :: FilePath -> [FilePath] -> Maybe FilePath
findConfig solution = find $ \t -> map toLower (dropExtension t) `isInfixOf` map toLower solution
