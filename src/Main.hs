module Main (main) where

import qualified Main2021
import System.FilePath (dropExtension)

main :: IO ()
main = do
  let (answer, puzzle) = Main2021.getPuzzle
  rows <- readInput puzzle
  printAnswer (answer rows) puzzle

readInput :: FilePath -> IO [String]
readInput fp = do
  x <- readFile fp
  return (filter (not . null) . lines $ x)

printAnswer :: String -> FilePath -> IO ()
printAnswer x fp = putStrLn $ dropExtension fp <> ": " <> x
