module Main (main) where

import Data.List (intercalate, intersperse, nub, partition, sort)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShowId)
import System.FilePath (dropExtension,(</>))
import Text.Read (Lexeme (String))

import qualified Main2021

main :: IO ()
main = do 
    let (answer,puzzle) = Main2021.getPuzzle
    rows <- readInput puzzle
    printAnswer (answer rows) puzzle

readInput :: FilePath -> IO [String]
readInput fp = do
  x <- readFile fp
  return (filter (not . null) . lines $ x)

printAnswer :: String -> FilePath -> IO ()
printAnswer x fp = putStrLn $ dropExtension fp <> ": " <> x
