module Main2021 (getPuzzle) where

import Data.List (intercalate, intersperse, nub, partition, sort)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShowId)
import System.FilePath (dropExtension, (</>))
import Text.Read (Lexeme (String))

getPuzzle :: ([String] -> String, FilePath)
getPuzzle = from puzzle3
  where
    from (a, b) = (a, show 2021 </> b)

puzzle3 :: ([String] -> String, FilePath)
puzzle3 = (fun, "puzzle_03.txt")
  where
    -- Assuming all rows have same length
    fun rows = show powerConsumption
      where
        powerConsumption = gammaRate * epsilonRate
        gammaRate = fromBinary $ commons (<)
        epsilonRate = fromBinary $ commons (>)
        commons :: (Int -> Int -> Bool) -> String
        commons oper = map (commonBits oper . nthBits rows) [1 .. (length (head rows))]
        fromBinary :: String -> Int
        fromBinary xs = case xs of
          [c] -> read xs
          _ -> read [last xs] + 2 * fromBinary (init xs)
        nthBits xs n
          | n == 1 = map head xs
          | otherwise = nthBits (map tail xs) (n -1)
        commonBits :: (Int -> Int -> Bool) -> String -> Char
        commonBits oper bits =
          if (length a) `oper` (length b)
            then '0'
            else '1'
          where
            (a, b) = span (== '0') . sort $ bits

data Direction = Forward Int | Up Int | Down Int
  deriving (Show, Eq)

data Position = Position
  { depth :: Int,
    hor :: Int,
    aim :: Int
  }

puzzle2 :: ([String] -> String, FilePath)
puzzle2 = (fun, "puzzle_02.txt")
  where
    fun rows = show . afterwards . fromStart . map parse $ rows

    parse :: String -> Direction
    parse str = case str of
      'u' : 'p' : num -> Up (read num)
      'd' : 'o' : 'w' : 'n' : num -> Down (read num)
      'f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : num -> Forward (read num)
      _ ->
        error
          "parse error"

    changePos :: Position -> Direction -> Position
    changePos pos dir = case dir of
      Forward distance ->
        Position
          { hor = hor pos + distance,
            depth = depth pos + (aim pos * distance),
            aim = aim pos
          }
      Up distance ->
        Position
          { hor = hor pos,
            depth = depth pos,
            aim = aim pos - distance
          }
      Down distance ->
        Position
          { hor = hor pos,
            depth = depth pos,
            aim = aim pos + distance
          }
    fromStart :: [Direction] -> Position
    fromStart = foldl changePos (Position 0 0 0)
    afterwards pos = hor pos * depth pos

puzzle1 :: ([String] -> String, FilePath)
puzzle1 = (fun, "puzzle_01.txt")
  where
    -- fun rows = show . length . filter id . increases . map toInt $ rows
    fun rows = show . length . filter id . increases . sumOf3 . map toInt $ rows
    toInt :: String -> Int
    toInt = read
    increases :: [Int] -> [Bool]
    increases [] = []
    increases [a] = []
    increases (a : b : rest) = (a < b) : increases (b : rest)
    sumOf3 :: [Int] -> [Int]
    sumOf3 xs = case xs of
      (a : b : c : rest) -> a + b + c : sumOf3 (b : c : rest)
      _ -> []
