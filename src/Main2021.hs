module Main2021
  ( getPuzzle,
    puzzle5,
    puzzle6,
    puzzle7
  )
where

import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))

getPuzzle :: ([String] -> String, FilePath)
getPuzzle = from puzzle7
  where
    from (a, b) = (a, show (2021 :: Int) </> b)
puzzle7 :: ([String] -> String, FilePath)
puzzle7 = (fun, "puzzle_07.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . showIt . calculate $ input
      where
        input :: [Int]
        input = read $ "[" <> head rows <> "]"
        calculate list = minimum . map fuel $ [minimum list..maximum list]
        fuel :: Int -> Int
        fuel pos = sum . map fuelConsuption $ input
          where 
            _fuelConsuption' i = abs (i - pos)
            fuelConsuption i = sum [1..abs (i - pos)]
        showIt x = [show x]



data Timers = Timers
  { timerDay :: !Int,
    timer8 :: !Integer,
    timer7 :: !Integer,
    timer6 :: !Integer,
    timer5 :: !Integer,
    timer4 :: !Integer,
    timer3 :: !Integer,
    timer2 :: !Integer,
    timer1 :: !Integer,
    timer0 :: !Integer
  }
  deriving (Show)

puzzle6 :: ([String] -> String, FilePath)
puzzle6 = (fun, "puzzle_06.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . showIt . toDay 256 $ initial
      where
        initial :: Timers
        initial =
          Timers
            { timerDay = 0,
              timer8 = amount 8,
              timer7 = amount 7,
              timer6 = amount 6,
              timer5 = amount 5,
              timer4 = amount 4,
              timer3 = amount 3,
              timer2 = amount 2,
              timer1 = amount 1,
              timer0 = amount 0
            }
          where
            timers = read $ "[" <> head rows <> "]"
            amount :: Integer -> Integer
            amount i = toInteger . length . filter (== i) $ timers
    nextDay :: Timers -> Timers
    nextDay x =
      Timers
        { timerDay = 1 + timerDay x,
          timer8 = timer0 x,
          timer7 = timer8 x,
          timer6 = timer7 x + timer0 x,
          timer5 = timer6 x,
          timer4 = timer5 x,
          timer3 = timer4 x,
          timer2 = timer3 x,
          timer1 = timer2 x,
          timer0 = timer1 x
        }
    toDay :: Integer -> Timers -> Timers
    toDay 0 tn = tn
    toDay n tn = nextDay . toDay (n -1) $ tn
    showIt :: Timers -> [String]
    showIt t =
      [ show t,
        show $ sum [timer0 t,timer1 t, timer2 t, timer3 t, timer4 t, timer5 t, timer6 t, timer7 t, timer8 t]
      ]

type Field = Map Point Int

type LineSegment = (Point, Point)

type Point = (Int, Int)

puzzle5 :: ([String] -> String, FilePath)
puzzle5 = (fun, "puzzle_05.txt")
  where
    fun :: [String] -> String
    fun rows = "\n\n" <> (showField . foldr intoField mempty . concatMap (pointsOf False . parseRow) $ rows)
    _showField2 :: Map Point Int -> String
    _showField2 m = intercalate "\n" . map showRow' $ [0 .. rows]
      where
        showRow' :: Int -> String
        showRow' i = map (showCell i) [0 .. cols]
        showCell i j = case Map.lookup (j, i) m of
          Nothing -> '.'
          Just n -> if n <= 9 then chr (ord '0' + n) else chr (ord 'A' + n)
        cols = maximum . map (fst . fst) . Map.toList $ m
        rows = maximum . map (fst . fst) . Map.toList $ m
    showField :: Map Point Int -> String
    showField = show . length . filter (\x -> snd x >= 2) . Map.toList
    parseRow :: String -> LineSegment
    parseRow row = ((read a, read b), (read c, read d))
      where
        (a, restA) = span (/= ',') row
        (b, restB) = span (/= ' ') . drop 1 $ restA
        (c, restC) = span (/= ',') . drop 4 $ restB
        d = tail restC
    _showRow :: Bool -> LineSegment -> [String]
    _showRow b ls =
      [ showLinesegment ls,
        intercalate ">" . map showPoint . pointsOf b $ ls
      ]
      where
        showLinesegment ls' = showPoint (fst ls') <> " -> " <> showPoint (snd ls')
        showPoint p = show (fst p) <> "," <> show (snd p)
    pointsOf :: Bool -> LineSegment -> [(Int, Int)]
    pointsOf onlyHorAndVer ((a, b), (c, d))
      | -- vertical
        minX == maxX =
        zip (repeat minX) [minY .. maxY]
      | -- horizontal
        minY == maxY =
        zip [minX .. maxX] $ repeat minY
      | abs (a - c) /= abs (b - d) = error "not diagonal"
      | onlyHorAndVer = []
      | otherwise =
        zip ((if a < c then id else reverse) [minX .. maxX]) ((if b < d then id else reverse) [minY .. maxY])
      where
        minX = min a c
        minY = min b d
        maxX = max a c
        maxY = max b d
    intoField :: Point -> Field -> Field
    intoField key = Map.insertWith (+) key 1
