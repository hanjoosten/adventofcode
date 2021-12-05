module Main2021 (getPuzzle) where

import Data.List (intercalate, intersperse, nub, partition, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe, fromJust)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShowId)
import System.FilePath (dropExtension, (</>))
import Text.Read (Lexeme (String))

getPuzzle :: ([String] -> String, FilePath)
getPuzzle = from puzzle4
  where
    from (a, b) = (a, show 2021 </> b)

data Board = Board
  { boardNr :: !Int,
    cells :: ![Cell]
  }
  deriving (Show)

data Cell = Cell
  { row :: !Int,
    col :: !Int,
    value :: !Int,
    drawn :: !Bool
  }
   
instance Show Cell where
  show c =
    if drawn c then mempty else "("<>show(row c)<>","<>show(col c)<>":"<>(if drawn c then "X" else show (value c))<>")"

data GameState = GameState
  { nrsStillToCome :: ![Int],
    nrsDrawn :: ![Int],
    boards :: ![Board],
    winner :: Maybe Board
  }
  deriving (Show)

puzzle4 :: ([String] -> String, FilePath)
puzzle4 = (fun, "puzzle_04.txt")
  where
    (nrOfRows,nrOfCols) = (5,5)
    fun = show . score . playBingo . initGame 
    score :: GameState -> Int
    score gs = (sum . mapMaybe valueWhenNotDrawn . cells . fromJust . winner $ gs) 
             * (head . nrsDrawn $ gs)
      where  valueWhenNotDrawn :: Cell -> Maybe Int
             valueWhenNotDrawn c = if drawn c then Nothing else Just (value c)
             
    playBingo :: GameState -> GameState
    playBingo = go
      where go :: GameState -> GameState
            go gs = case winner gs of
                      Nothing -> go (drawNr gs)
                      Just w -> gs
    initGame :: [String] -> GameState
    initGame [] = error "Empty input file!"
    initGame (firstRow : tl) =
      GameState
        { nrsStillToCome = read $ "[" <> firstRow <> "]",
          nrsDrawn = [],
          boards = readBoards 1 tl,
          winner = Nothing
        }
      where
        readBoards :: Int -> [String] -> [Board]
        readBoards i xs
          | null xs = mempty
          | otherwise = buildBoard i firsts : readBoards (i + 1) rest
          where
            (firsts, rest) = splitAt nrOfRows . filter (not . null) $ xs
            buildBoard :: Int -> [String] -> Board
            buildBoard i xs =
              Board
                { boardNr = i,
                  cells = concat . zipWith readRow [1 .. ] $ xs
                }
            readRow :: Int -> String -> [Cell]
            readRow rowNum = zipWith toCell [1 .. ] . map read . words
              where
                toCell colNum val = Cell rowNum colNum val False
    drawNr :: GameState -> GameState
    drawNr gs = (\x -> trace (showIt gs x)x)  $
      case gs of
        GameState {nrsStillToCome = []} -> error "Start tracing :(" 
        GameState {winner = Just _} -> gs
        GameState
          { nrsStillToCome = nrNow : restNrs,
            nrsDrawn = drawns,
            boards = bs,
            winner = Nothing
          } ->
          GameState
            { nrsStillToCome = restNrs,
              nrsDrawn = nrNow:drawns,
              boards = newBoards,
              winner = case filter isWinner newBoards of
                          [] -> Nothing
                          [b] -> Just b
                          xs -> error $ "There are "<>show (length xs)<>" winning boards!"
            }
              where
                newBoards = map (doDrawn nrNow) bs
    isWinner :: Board -> Bool
    isWinner b = any (`rowCompleted` b) [1..nrOfRows] ||
                 any (`colsCompleted` b) [1..nrOfCols]
    doDrawn :: Int -> Board -> Board
    doDrawn nr b = b{ cells = map doCell (cells b)}
      where doCell c = c{drawn = drawn c || value c == nr}
    rowCompleted :: Int -> Board -> Bool
    rowCompleted rNr xx = all drawn . filter (inRow rNr) . cells $ xx
      where inRow i c = i == row c
    colsCompleted :: Int -> Board -> Bool
    colsCompleted rNr = all drawn . filter (inCol rNr) . cells 
      where inCol i c = i == col c
    showIt gs x = intercalate "\n   " $
                 [ "drawing "<>(show . head . nrsStillToCome $ gs)
                 , "   "<>(show . nrsStillToCome $ x)
                 ] ++
                 (map (\row -> ("  "<>) . concatMap show . cells $ row) . boards $ x)
                 
puzzle3 :: ([String] -> String, FilePath)
puzzle3 = (fun, "puzzle_03.txt")
  where
    -- Assuming all rows have same length
    fun rows = show lifeSupportRating
      where
        lifeSupportRating = oxygenGeneratorRating * co2ScrubberRating
        oxygenGeneratorRating = rating (>)
        co2ScrubberRating = rating (<=)
        rating :: (Int -> Int -> Bool) -> Int
        rating oper =
          fromBinary
            . head
            . remainderMatrix oper [1 .. length (head rows)]
            $ rows

        remainderMatrix :: (Int -> Int -> Bool) -> [Int] -> [String] -> [String]
        remainderMatrix _ [] rows = rows
        remainderMatrix oper (index : rest) rows
          | length rows == 1 = rows
          | countAtPlace '0' `oper` countAtPlace '1' =
            remainderMatrix oper rest . remaining '0' $ rows
          | otherwise = remainderMatrix oper rest . remaining '1' $ rows
          where
            countAtPlace :: Char -> Int
            countAtPlace bit = length . filter (\row -> bit == ((!! max 0 (index - 1)) row)) $ rows
            remaining :: Char -> [String] -> [String]
            remaining bit = filter (hasBitAtIndex bit)
            hasBitAtIndex :: Char -> String -> Bool
            hasBitAtIndex bit row = bit == ((!! max 0 (index - 1)) row)

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
