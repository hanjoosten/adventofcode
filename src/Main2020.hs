module Main2020 (main) where

import Data.List (intercalate, intersperse, nub, partition, sort)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShowId)
import System.FilePath (dropExtension,(</>))
import Text.Read (Lexeme (String))

main :: IO ()
main = do
  content <- readInput (snd puzzle)
  printAnswer (fst puzzle content) (snd puzzle)
  where
    puzzle = from 2020 puzzle24
      where from year (a,b) = (a,show year </> b) 
data Direction = EAST | SOUTHEAST | SOUTHWEST | WEST | NORTHWEST | NORTHEAST
  deriving (Eq, Show, Bounded, Enum)

data TileColour = Black | White
  deriving (Eq, Show, Bounded)

type Coordinate = (Int, Int)
data Tile = Tile
      { coordinaten :: Coordinate
      , colour:: TileColour
      }
puzzle24 :: ([String] -> String, FilePath)
puzzle24 = (fun, "puzzle_24.txt")
  where
    fun rows =
      --show. length . reduce . sort . map (show . placement . parse) $ rows
      concat $ "\n" : (intersperse "\n" . concatMap (niceShow . blackTilesAtDay) $ [0 .. 100])
      where
        niceShow :: (Int,Set.Set Coordinate) -> [String]
        niceShow (i,cs) = 
          if i `elem` [1..10] ++ map (* 10) [1..10]
            then ["Day "<>show i<>": "<>show (length cs)] 
            else mempty
        blackTilesAtDay :: Int -> (Int, Set.Set Coordinate)
        blackTilesAtDay 0 = (0, Set.fromList . reduce . sort . map (placement . parse) $ rows)
        blackTilesAtDay n = (n, nextDayBlackTiles n . snd . blackTilesAtDay $ n - 1)
    nextDayBlackTiles :: Int -> Set.Set Coordinate -> Set.Set Coordinate
    nextDayBlackTiles day blackNow = blackNext
      where
        showIt = concatMap (\x -> "\n "++show day++"  "<>x ) $
           ["blackNow:"<>show blackNow
           ,"allNow: "<> (show . length $ allNow)
           ] ++
           showWhitesToBlack allNow
        showWhitesToBlack :: Set.Set Coordinate -> [String]
        showWhitesToBlack = concatMap foo
          where foo c = ["    "<>show c<>": Nu: "<>
                             (if isBlackNow c then "Zwart" else "Wit")
                          <>". Aantal zwarte buren: "<> show(nrOfBlackNeighbors c  )
                          <>" Next: "<>if willbeBlack c then "Zwart" else "Wit" 
                        ]
        blackNext :: Set.Set Coordinate
        blackNext = Set.filter willbeBlack allNow
        willbeBlack c = (isBlackNow c && nrOfBlackNeighbors c `elem` [1,2]) ||
                     (isWhiteNow c && nrOfBlackNeighbors c == 2)
        allNow :: Set.Set Coordinate
        allNow = (Set.unions . map neighbors . Set.toList $ blackNow ) `Set.union` blackNow
        isBlackNow c = c `elem` blackNow
        isWhiteNow = not . isBlackNow
        nrOfBlackNeighbors = length . filter isBlackNow . Set.toList . neighbors

        neighbors :: Coordinate -> Set.Set Coordinate
        neighbors c = Set.fromList . map (plus c . vector) $ [EAST ..]


    parse :: String -> [Direction]
    parse [] = []
    parse ('e' : x) = EAST : parse x
    parse ('s' : 'e' : x) = SOUTHEAST : parse x
    parse ('s' : 'w' : x) = SOUTHWEST : parse x
    parse ('w' : x) = WEST : parse x
    parse ('n' : 'w' : x) = NORTHWEST : parse x
    parse ('n' : 'e' : x) = NORTHEAST : parse x
    parse x = error $ "unparsable: " <> x
    vector :: Direction -> Coordinate
    vector d = case d of
      EAST -> (1, 0)
      SOUTHEAST -> (1, -1)
      SOUTHWEST -> (0, -1)
      WEST -> (-1, 0)
      NORTHWEST -> (-1, 1)
      NORTHEAST -> (0, 1)
    brief :: Direction -> String
    brief d = case d of
      EAST -> "e"
      SOUTHEAST -> "se"
      SOUTHWEST -> "sw"
      WEST -> "w"
      NORTHWEST -> "nw"
      NORTHEAST -> "ne"

    plus :: Coordinate -> Coordinate -> Coordinate
    plus (a, b) (c, d) = (a + c, b + d)
    placement :: [Direction] -> Coordinate
    placement = foldr (plus . vector) (0, 0)
    reduce :: Eq a => [a] -> [a]
    reduce [] = []
    reduce [a] = [a]
    reduce (a : b : c) = if a == b then reduce c else a : reduce (b : c)
    showIntermediates :: Coordinate -> [Direction] -> String
    showIntermediates x ds = case ds of
      [] -> show x
      di : dis -> show x <> brief di <> showIntermediates newSpot dis
        where
          newSpot = plus x $ vector di

puzzle9 :: ([String] -> String, FilePath)
puzzle9 = (fun, "puzzle_9.txt")
  where
    fun rows = show (head aap + last aap)
      where
        aap :: [Int]
        aap = sort a
          where
            (a, _, _) = aap'
        aap' =
          traceShowId $
            head
              [ (map (number rows) [a .. b], a, b)
                | a <- [1 .. 500],
                  b <- [a + 1 .. length rows],
                  b - a < 20,
                  (sum . map (number rows) $ [a .. b]) == answer1 rows
              ]

    numbers :: [String] -> [(Int, Int)]
    numbers = zip [1 ..] . map read
    preambleLength = 25
    preamble :: [String] -> Int -> Maybe [(Int, Int)]
    preamble rows x
      | x <= preambleLength = Nothing
      | x > length rows = Nothing
      | otherwise = Just . reverse . take preambleLength . reverse . take (x -1) $ numbers rows
    number rows x = case lookup x (numbers rows) of
      Nothing -> error $ "number " <> show x <> " bestaat niet!"
      Just n -> n
    notValid :: [String] -> (Int, Int) -> Bool
    notValid rows = not . isValid rows . fst
    isValid :: [String] -> Int -> Bool
    isValid rows = isJust . isValid' rows
    isValid' :: [String] -> Int -> Maybe ((Int, Int), (Int, Int))
    isValid' rows x = case preamble rows x of
      Nothing -> Nothing
      Just xs -> case [ (a, b)
                        | a <- xs,
                          b <- xs,
                          fst a < fst b,
                          snd a + snd b == number rows x
                      ] of
        [] -> Nothing
        h : _ -> Just h
    answer1 rows = snd . head . filter (notValid rows) . drop preambleLength $ numbers rows
      where
        showIt x = trace aap x
          where
            aap = intercalate "\n" $ map foo (numbers rows)
            foo (i, n) =
              show i <> ", "
                <> show n
                <> ", "
                <> show (number rows i)
                <> ", "
                <> show (isValid' rows i)
                <> ", "
                <> mies (isValid' rows i)
                <> "\n"
                <> show (preamble rows i)
              where
                mies :: Maybe ((Int, Int), (Int, Int)) -> String
                mies = show . fmap teun
                teun :: ((Int, Int), (Int, Int)) -> String
                teun ((a, b), (c, d)) = show ((a - i, b), (c - i, d))

puzzle3 :: ([String] -> String, FilePath)
puzzle3 = (fun, "puzzle_3.txt")
  where
    fun rows =
      --show . length . filter isTree $ zip [1..] rows
      show . product . map doSlope $ slopes
      where
        slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        doSlope :: (Int, Int) -> Int
        doSlope (right, down) =
          showIt $
            length . filter isTree $ zip3 [1 ..] (map snd rows') (repeat right)
          where
            rows' = filter validRow . zip [0 ..] $ rows
            validRow (i, _) = i `rem` down == 0
            showIt x =
              trace
                ( show right <> ", "
                    <> show down
                    <> ", "
                    <> (show . take 10 . map fst $ rows')
                    <> ": "
                    <> show x
                )
                x
    isTree :: (Int, String, Int) -> Bool
    isTree (rowNum, row, right) = traceFirsts $
      case theSpot of
        '#' -> True
        '.' -> False
        c -> error $ "Found " <> [c] <> " in the input file. This is unexpected."
      where
        traceFirsts = if rowNum <= 5 then trace showOnTrace else id
          where
            showOnTrace = rowPlus <> "\n" <> spaces (toColNum rowNum - 1) <> "^" <> mies
            rowPlus = take (minimum [toColNum rowNum, length row]) . cycle $ row
            spaces n = replicate n ' '
            mies = theSpot : show (toColNum rowNum)
        theSpot = last . take (toColNum rowNum) . cycle $ row
        toColNum :: Int -> Int
        toColNum i = 1 + right * (i - 1)

puzzle2 :: IO ()
puzzle2 = do
  passwords <- readInput
  printAnswer $ getAnswer passwords
  where
    readInput :: IO [String]
    readInput = do
      x <- readFile "puzzle_2.txt"
      return (filter (not . null) . lines $ x)
    getAnswer :: [String] -> Int
    getAnswer = length . filter isValidPassword
    printAnswer :: Int -> IO ()
    printAnswer x = print $ "Dag 2: " <> show x
    isValidPassword :: String -> Bool
    isValidPassword str = trace aap isValid2
      where
        aap =
          str
            ++ if isValid2
              then " OK."
              else
                " "
                  <> show
                    [ show min,
                      show max,
                      show $ charAtPosition min,
                      show $ charAtPosition max
                      -- , show . partition (==givenLetter) $ password
                    ]
        isValid1 = count >= min && count <= max
        isValid2 =
          charAtPosition min == Just givenLetter && charAtPosition max /= Just givenLetter
            || charAtPosition min /= Just givenLetter && charAtPosition max == Just givenLetter
        (lowest, rest) = span (/= '-') str
        min, max :: Int
        min = read lowest
        (highest, rest') = span (/= ' ') (tail rest)
        max = read highest
        givenLetter = head . tail $ rest'
        password :: String
        password = drop 4 rest'
        count :: Int
        count = length . fst . partition (`elem` [givenLetter]) $ password
        charAtPosition :: Int -> Maybe Char
        charAtPosition x = case drop (x -1) password of
          [] -> Nothing
          c : s -> Just c

puzzle1 :: IO ()
puzzle1 = do
  numbers <- readInput
  printAnswer $ getAnswer numbers
  where
    readInput :: IO [Int]
    readInput = do
      x <- readFile "puzzle_1.txt"
      return (map read . lines $ x)
    getAnswer :: [Int] -> [(Int, Int, Int, Int)]
    getAnswer xs = [(a, b, c, a * b * c) | a <- xs, b <- xs, a <= b, c <- xs, b <= c, a + b + c == 2020]
    printAnswer :: Show a => a -> IO ()
    printAnswer a = putStrLn ("Day 1 " <> show a)

readInput :: FilePath -> IO [String]
readInput fp = do
  x <- readFile fp
  return (filter (not . null) . lines $ x)

printAnswer :: String -> FilePath -> IO ()
printAnswer x fp = putStrLn $ dropExtension fp <> ": " <> x
