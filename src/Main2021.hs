{-# LANGUAGE FlexibleInstances #-}

module Main2021
  ( getPuzzle,
    puzzle5,
    puzzle6,
    puzzle7,
    puzzle8,
    puzzle9,
    puzzle10,
    puzzle11
  )
where

import Data.Char (chr, ord)
import Data.List (intercalate, sort, union, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import System.FilePath ((</>))

data Part = Part1 | Part2 -- Om onderscheid te maken in de delen van de dagpuzzel.
  deriving (Eq)

getPuzzle :: ([String] -> String, FilePath)
getPuzzle = from puzzle11
  where
    from (a, b) = (a, show (2021 :: Int) </> b)

type Value11 = (Int, Bool)

instance Value (Int, Bool) where
  toVal i = (i, False)
  showVal (i, b)
    | b = "0"
    | i > 9 = "#"
    | otherwise = show i

mapBoth :: (a -> x) -> (b -> y) -> (a, b) -> (x, y)
mapBoth funcA funcB (x, y) =
  (funcA x, funcB y)

puzzle11 :: ([String] -> String, FilePath)
puzzle11 = (fun, "puzzle_11.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . showIt . calculate Part2 $ input
      where
        input :: Area Value11
        input = inputArea rows
        calculate :: Part -> Area Value11 -> (Int, Area Value11)
        calculate part = go 0 0
          where
            go :: Int -> Int -> Area Value11 -> (Int, Area Value11)
            go flashes dayNr area =
              case part of
                Part1 ->
                  if dayNr == 100
                    then (flashes, area)
                    else next
                Part2 ->
                  if traceDayflashes dayFlashes == 100
                    then (flashes + dayFlashes, nextDayArea)
                    else next
              where
                traceDayflashes i = trace ("Amount of flashes after day " <> show (dayNr + 1) <> ": " <> show i) i
                next = (if part == Part1 then traceNext else id) $ go (flashes + dayFlashes) (dayNr + 1) nextDayArea
                traceNext :: (Int, Area Value11) -> (Int, Area Value11)
                traceNext x =
                  trace
                    ( unlines $
                        [ "dayNr: " <> show dayNr,
                          "flashes before: " <> show flashes <> ", dayFlashes: " <> show dayFlashes <> " makes " <> show (flashes + dayFlashes) <> " flashes after this day."
                        ]
                          ++ showIt (flashes, area)
                    )
                    x
                (dayFlashes, nextDayArea) = doAllFlashes . addEnergy $ area
                addEnergy :: Area Value11 -> Area Value11
                addEnergy = Map.map (mapBoth (+ 1) id)
                doAllFlashes = go' 0
                  where
                    go' :: Int -> Area Value11 -> (Int, Area Value11)
                    go' flashesSofarThisDay a =
                      if null newFlashes'
                        then (flashesSofarThisDay, fmap resetFlashed a)
                        else go' (flashesSofarThisDay + length newFlashes') (doNewFlashes a)
                      where
                        resetFlashed :: Value11 -> Value11
                        resetFlashed (n, flashed) = if flashed then (0, False) else (n, flashed)
                        newFlashes' :: Area Value11
                        newFlashes' =
                          -- (\x-> trace ("NewFlashes: "<>show x) x ) $
                          Map.filter (\(energy, flashed) -> not flashed && energy > 9) a
                        doNewFlashes :: Area Value11 -> Area Value11
                        doNewFlashes = updateNeighbours . Map.keysSet $ newFlashes'
                        updateNeighbours :: Set (Int, Int) -> Area Value11 -> Area Value11
                        updateNeighbours s area'' =
                          -- (\x-> trace (intercalate "\n"$"updateNeighbours: ":showArea x) x ) $
                          foldr doNeighbours area'' s
                        doNeighbours :: (Int, Int) -> Area Value11 -> Area Value11
                        doNeighbours k area'' = resetSelf $ foldr addOne area'' (neighborsOf k)
                          where
                            resetSelf :: Area Value11 -> Area Value11
                            resetSelf = Map.update (\_ -> Just (0, True)) k
                        addOne :: (Int, Int) -> Area Value11 -> Area Value11
                        addOne k = Map.adjust (mapBoth (+ 1) id) k
                        neighborsOf :: (Int, Int) -> [(Int, Int)]
                        neighborsOf (x, y) =
                          [ (p, q) | p <- [x -1 .. x + 1], q <- [y -1 .. y + 1], not (p == x && q == y)
                          ]
        showIt :: (Int, Area Value11) -> [String]
        showIt (flashes, area) =
          ("Resulting flashes: " <> show flashes) : showArea area

data Result
  = Valid
      { reverseParsed :: String
      }
  | Corrupted
      { orig :: String,
        reverseParsed :: String,
        expected :: Maybe Char,
        found :: Char
      }
  | Unfinished
      { reverseParsed :: String,
        restHeap :: String
      }
  deriving (Show)

puzzle10 :: ([String] -> String, FilePath)
puzzle10 = (fun, "puzzle_10.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . showIt . calculate Part2 $ input
      where
        input :: [String]
        input = rows
        calculate :: Part -> [String] -> Int
        calculate part = case part of
          Part1 -> sum . pointList
          Part2 -> mid . sort . pointList
          where
            mid :: [Int] -> Int
            mid [m] = m
            mid (_ : b : c) = mid (init (b : c))
            mid _ = error "No mid!"
            pointList = mapMaybe (foo . doRow)
            foo :: Result -> Maybe Int
            foo res = case (part, res) of
              (Part1, Corrupted {}) -> Just $ getpoints Part1 (found res)
              (Part2, Unfinished {}) -> Just score
                where
                  score = foldl (\n c -> 5 * n + getpoints Part2 c) 0 . restHeap $ res
              _ -> Nothing
        doRow :: String -> Result
        doRow str = go "" "" str
          where
            go :: String -> String -> String -> Result
            go parsed heap unparsed =
              case (heap, unparsed) of
                ([], []) -> Valid parsed
                ([], c : rest) ->
                  case filter (\p -> fst p == c) matches of
                    [] -> Corrupted str parsed Nothing c
                    [(_, expect)] -> go (c : parsed) (expect : heap) rest
                    err -> error $ "doubles found in matches: " <> show err
                (_ : _, []) -> Unfinished parsed heap
                (h : tl, c : rest) ->
                  if h == c
                    then go (c : parsed) tl rest
                    else case filter (\p -> fst p == c) matches of
                      [] -> Corrupted str parsed (Just h) c
                      [(_, expect)] -> go (c : parsed) (expect : heap) rest
                      err -> error $ "doubles found in matches: " <> show err
        showIt :: Int -> [String]
        showIt i = [show i]
    matches = map match quads
      where
        match (a, b, _, _) = (a, b)
    getpoints :: Part -> Char -> Int
    getpoints part c = snd . head . filter (\(x, _) -> c == x) . map (point part) $ quads
    point Part1 (_, b, c', _) = (b, c')
    point Part2 (_, b, _, d) = (b, d)
    quads =
      [ ('(', ')', 3, 1),
        ('[', ']', 57, 2),
        ('{', '}', 1197, 3),
        ('<', '>', 25137, 4)
      ]

type Area a = Map (Int, Int) a

class Value a where
  toVal :: Int -> a
  showVal :: a -> String

instance Value Int where
  toVal = id
  showVal = show

inputArea :: Value a => [String] -> Area a
inputArea rows = Map.fromList . concat $ parsedRows
  where
    parsedRows :: Value a => [[((Int, Int), a)]]
    parsedRows = zipWith parseRow [0 ..] rows
    parseRow :: Value a => Int -> String -> [((Int, Int), a)]
    parseRow rowNr =
      zipWith (mkKeyVal rowNr) [0 ..] . map (read . pure)
    mkKeyVal :: Value a => Int -> Int -> Int -> ((Int, Int), a)
    mkKeyVal row col i = ((row, col), toVal i)

showArea :: Value a => Area a -> [String]
showArea = lines . go Nothing . Map.assocs
  where
    go :: Value a => Maybe Int -> [((Int, Int), a)] -> String
    go _ [] = []
    go lineNr (((row, _), val) : tl) = str <> go lineNr' tl
      where
        (lineNr', str) =
          case lineNr of
            Nothing -> (Just row, "\n" <> showVal val)
            Just l ->
              ( Just row,
                if l == row
                  then showVal val
                  else "\n" <> showVal val
              )

puzzle9 :: ([String] -> String, FilePath)
puzzle9 = (fun, "puzzle_09.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . showIt . calculate . inputArea $ rows
      where
        calculate :: Area Int -> Int
        calculate area = _part2
          where
            _part1 = length . map ((+ 1) . snd) . Map.toList $ lowPoints
            _part2 = product . take 3 . reverse . sort . map (length . Set.toList . getBasin . fst) . Map.toList $ lowPoints
            lowPoints = Map.filterWithKey isLowPoint area
            isLowPoint :: (Int, Int) -> Int -> Bool
            isLowPoint point _ =
              case Map.lookup point area of
                Just val -> val < minimum (neighboursVals point)
                Nothing -> error "The map wasn't constructed well. "
            neighboursVals = mapMaybe (`Map.lookup` area) . Set.toList . neighbourKeys
            neighbourKeys :: Point -> Set Point
            neighbourKeys (row, col) = Set.fromList [(row + 1, col), (row -1, col), (row, col + 1), (row, col -1)]
            getBasin :: Point -> Set Point
            getBasin lowPoint = go mempty (Set.singleton lowPoint)
              where
                go :: Set Point -> Set Point -> Set Point
                go a set
                  | null set = a
                  | otherwise = go (a `Set.union` newNeighbors) newNeighbors
                  where
                    newNeighbors :: Set Point
                    newNeighbors =
                      Set.unions (map validNeighbors . Set.toList $ set)
                        `Set.difference` a
                    validNeighbors :: Point -> Set Point
                    validNeighbors = Set.filter isValid . neighbourKeys
                    isValid :: Point -> Bool
                    isValid p = case Map.lookup p area of
                      Nothing -> False
                      Just n -> n < 9
        showIt :: Int -> [String]
        showIt x = [show x]

data Pattern = Pattern
  { zero :: String,
    one :: String,
    two :: String,
    three :: String,
    four :: String,
    five :: String,
    six :: String,
    seven :: String,
    eight :: String,
    nine :: String
  }
  deriving (Show)

puzzle8 :: ([String] -> String, FilePath)
puzzle8 = (fun, "puzzle_08.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . showIt . calculate $ input
      where
        input :: [(Pattern, [String])]
        input = map parse rows
          where
            parse :: String -> (Pattern, [String])
            parse str =
              ( wordsToPattern . map sort . words $ str1,
                map sort . words $ tail str2
              )
              where
                (str1, str2) = span (/= '|') str
        calculate :: [(Pattern, [String])] -> Int
        calculate = _part2
          where
            _part1, _part2 :: [(Pattern, [String])] -> Int
            _part1 = length . filter foo . concatMap snd
              where
                foo :: String -> Bool
                foo str = length str `elem` [2, 4, 3, 7]
            _part2 = sum . map doRow
              where
                doRow :: (Pattern, [String]) -> Int
                doRow (pat, digits) = read . concatMap (show . digit2value pat) $ digits

        showIt :: Int -> [String]
        showIt x = [show x]

        wordsToPattern :: [String] -> Pattern
        wordsToPattern digits =
          Pattern d0 d1 d2 d3 d4 d5 d6 d7 d8 d9
          where
            ofLength n = filter (\digit -> length digit == n) digits
            len = head . ofLength
            d1 = len 2
            d4 = len 4
            d7 = len 3
            d8 = len 7
            fives = ofLength 5
            sixes = ofLength 6
            uni n a b = length (a `union` b) == n
            d6 = head . filter (uni 7 d1) $ sixes
            d9 = head . filter (uni 6 d5) $ sixes \\ [d6]
            d0 = head $ sixes \\ [d6, d9]
            d2 = head . filter (uni 7 d4) $ fives
            d3 = head . filter (uni 5 d7) $ fives
            d5 = head $ fives \\ [d2, d3]
        digit2value :: Pattern -> String -> Int
        digit2value pat str
          | str == zero pat = 0
          | str == one pat = 1
          | str == two pat = 2
          | str == three pat = 3
          | str == four pat = 4
          | str == five pat = 5
          | str == six pat = 6
          | str == seven pat = 7
          | str == eight pat = 8
          | str == nine pat = 9
          | otherwise =
            error $
              "digit not in pattern. " <> show str <> "\n"
                <> show pat

puzzle7 :: ([String] -> String, FilePath)
puzzle7 = (fun, "puzzle_07.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . showIt . calculate $ input
      where
        input :: [Int]
        input = read $ "[" <> head rows <> "]"
        calculate list = minimum . map fuel $ [minimum list .. maximum list]
        fuel :: Int -> Int
        fuel pos = sum . map fuelConsuption $ input
          where
            _fuelConsuption' i = abs (i - pos)
            fuelConsuption i = sum [1 .. abs (i - pos)]
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
        show $ sum [timer0 t, timer1 t, timer2 t, timer3 t, timer4 t, timer5 t, timer6 t, timer7 t, timer8 t]
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
