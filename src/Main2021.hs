module Main2021
  ( getPuzzle
  )
where

import System.FilePath ((</>))
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Data.Char (chr, ord)

getPuzzle :: ([String] -> String, FilePath)
getPuzzle = from puzzle5
  where
    from (a, b) = (a, show (2021 :: Int) </> b)


type Field = Map Point Int
type LineSegment = (Point,Point)
type Point = (Int,Int)
puzzle5 :: ([String] -> String, FilePath)
puzzle5 = (fun, "puzzle_05.txt")
  where
    fun :: [String] -> String
    fun rows = "\n\n"<>(showField . mies . concatMap (pointsOf False . parseRow) $ rows)
    _showField2 :: Map Point Int -> String
    _showField2 m = intercalate "\n" . map showRow' $ [0..rows]
      where showRow' :: Int -> String
            showRow' i = map (showCell i) [0..cols]
            showCell i j = case Map.lookup (j,i) m of
              Nothing -> '.'
              Just n -> if n <= 9 then chr (ord '0' + n) else chr (ord 'A' + n)
            cols = maximum . map (fst . fst) . Map.toList $ m
            rows = maximum . map (fst . fst) . Map.toList $ m
    showField :: Map Point Int -> String
    showField = show . length . filter (\x-> snd x >= 2) . Map.toList
    parseRow :: String -> LineSegment
    parseRow row =  ((read a,read b), (read c, read d))
      where
        (a,restA) = span (/= ',') row
        (b,restB) = span (/= ' ') . drop 1 $ restA
        (c,restC) = span (/= ',') . drop 4 $ restB
        d = tail restC
    _showRow :: Bool -> LineSegment -> [String]
    _showRow b ls = [ showLinesegment ls
                 , intercalate ">" . map showPoint . pointsOf b $ ls ]  
      where
        showLinesegment ls' = showPoint (fst ls') <>" -> "<>showPoint (snd ls')
        showPoint p = show (fst p)<>","<>show (snd p)
    pointsOf :: Bool -> LineSegment -> [(Int,Int)]
    pointsOf onlyHorAndVer ((a,b),(c,d)) 
      | -- vertical
        minX == maxX = zip (repeat minX) [minY .. maxY]
      | -- horizontal
        minY == maxY = zip [minX .. maxX] $ repeat minY
      | abs (a-c) /= abs (b-d) = error "not diagonal" 
      | onlyHorAndVer = []
      | otherwise = 
          zip ((if a < c then id else reverse)[minX .. maxX]) ((if b<d then id else reverse)[minY .. maxY])
      where minX = min a c
            minY = min b d
            maxX = max a c
            maxY = max b d 
    intoField :: Point -> Field -> Field
    intoField key = Map.insertWith  (+) key 1
    initField :: Field
    initField = mempty
    mies :: [Point] -> Field
    mies = foldr intoField initField
    