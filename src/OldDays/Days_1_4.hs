module OldDays.Days_1_4
  ( 
    puzzle1,
    puzzle2,
    puzzle3,
    puzzle4,
  )
where

import Data.List (sort)
import GHC.Exts (sortWith)


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
 show c = "(" <> show (row c) <> "," <> show (col c) <> ":" <> show (value c) <> ")"

newtype GameState = GameState
  { boards :: [BoardState]
  }
  deriving (Show)
data BoardState = BoardState
   { nrsstate :: !NumberState,
     board :: !Board
   }
  deriving (Show)
data NumberState = NumberState
   { nrsStillToCome :: ![Int],
     nrsDrawn :: ![Int]
   }
  deriving (Show)
puzzle4 :: ([String] -> String, FilePath)
puzzle4 = (fun, "puzzle_04.txt")
  where
    (nrOfRows, nrOfCols) = (5, 5)
    fun rows = resultOfPlay . map playBoard . boards $ initGame rows
    resultOfPlay :: [BoardState] -> String
    resultOfPlay gs = "\n"<>showWinner <> showLooser
       where
         showWinner,showLooser :: String
         showWinner = "Winner: "<>showIt winner
         showLooser = "Looser: "<>showIt looser
         sorted ::  [BoardState] ->  [BoardState]
         sorted = sortWith (length . nrsDrawn . nrsstate )
         winner = head (sorted gs)
         looser = last (sorted gs)
         showIt :: BoardState -> String
         showIt bs = unlines . map ("  "<>) $
            [show . boardNr . board $ bs
            ,"Score = "<>(show . score $ bs)<>" (sumOfUnmarked: "<>show (sumOfUnmarked bs)<>"* lastcalled: "<>show (lastCalled bs)<>") "
            ,"   "<>show (nrsstate bs)
            ,"   "<>show (board bs)]
         score :: BoardState -> Int
         score bs = sumOfUnmarked bs * lastCalled bs
         sumOfUnmarked = sum . map value . filter (not . drawn) . cells . board
         lastCalled = head . nrsDrawn . nrsstate
    playBoard :: BoardState -> BoardState
    playBoard x =
      if isWinner (board x)
        then x
        else case next (nrsstate x) of
               Nothing -> error "No more numbers to draw"
               Just ns -> playBoard BoardState { nrsstate = ns
                                               , board = doDrawn (nrNow ns) (board x)}
    nrNow :: NumberState -> Int
    nrNow = head . nrsDrawn
    next :: NumberState -> Maybe NumberState
    next (NumberState x ds) = case x of
                               [] -> Nothing
                               (h:tl) -> Just $ NumberState tl (h:ds)

    initGame :: [String] -> GameState
    initGame [] = error "Empty input file!"
    initGame (firstRow : tl) =
      GameState . map (initBoard initNrs) $ readBoards 1 tl
      where
        initNrs = NumberState
          { nrsStillToCome = read $ "[" <> firstRow <> "]",
          nrsDrawn = []}
        initBoard :: NumberState -> Board -> BoardState
        initBoard = BoardState
        readBoards :: Int -> [String] -> [Board]
        readBoards i xs
          | null xs = mempty
          | otherwise = buildBoard i firsts : readBoards (i + 1) rest
          where
            (firsts, rest) = splitAt nrOfRows . filter (not . null) $ xs
            buildBoard :: Int -> [String] -> Board
            buildBoard nr xs' =
              Board
                { boardNr = nr,
                  cells = concat . zipWith readRow [1 ..] $ xs'
                }
            readRow :: Int -> String -> [Cell]
            readRow rowNum = zipWith toCell [1 ..] . map read . words
              where
                toCell colNum val = Cell rowNum colNum val False
    -- drawNr :: GameState -> GameState
    -- drawNr gs = (\x -> trace (showIt gs x) x) $
    --   case gs of
    --     GameState {nrsStillToCome = []} -> error "Start tracing :("
    --     GameState {winner = Just _,
    --                looser = Just _} -> gs
    --     GameState
    --       { winner = Nothing
    --       } ->
    --         GameState
    --           { nrsStillToCome = nrsStillToCome',
    --             nrsDrawn = nrsDrawn',
    --             boards = boards',
    --             winner = case filter isWinner boards' of
    --               [] -> Nothing
    --               [b] -> Just (BoardSnapshot b (head nrNow))
    --               xs -> error $ "There are " <> show (length xs) <> " winning boards!",
    --             looser = Nothing
    --           }
    --     GameState
    --       { looser = Nothing
    --       , boards = _ : _ 
    --       } ->
    --         GameState
    --           { nrsStillToCome = nrsStillToCome',
    --             nrsDrawn = nrsDrawn',
    --             boards = boards',
    --             winner = winner gs,
    --             looser = case filter (not .isWinner) boards' of
    --               [] -> Nothing
    --               [b] -> let endGame = playBingo GameState 
    --                                              {nrsStillToCome = nrsStillToCome gs,
    --                                               nrsDrawn = nrsDrawn gs,
    --                                               boards = [b],
    --                                               winner = Nothing,
    --                                               looser = Nothing
    --                                              }
    --                      in winner endGame
    --               xs -> error $ "There are " <> show (length xs) <> " winning boards!"
    --           }
    --     GameState { looser = Nothing 
    --               , boards = [] } -> undefined
    --  where (nrNow,nrsStillToCome') = splitAt 1 . nrsStillToCome $ gs
    --        nrsDrawn' = nrNow <> nrsDrawn gs
    --        boards' = map (doDrawn $ head nrNow) (boards gs)


    isWinner :: Board -> Bool
    isWinner b =
      any (`rowCompleted` b) [1 .. nrOfRows]
        || any (`colsCompleted` b) [1 .. nrOfCols]
    doDrawn :: Int -> Board -> Board
    doDrawn nr b = b {cells = map doCell (cells b)}
      where
        doCell c = c {drawn = drawn c || value c == nr}
    rowCompleted :: Int -> Board -> Bool
    rowCompleted rNr xx = all drawn . filter (inRow rNr) . cells $ xx
      where
        inRow i c = i == row c
    colsCompleted :: Int -> Board -> Bool
    colsCompleted rNr = all drawn . filter (inCol rNr) . cells
      where
        inCol i c = i == col c
puzzle3 :: ([String] -> String, FilePath)
puzzle3 = (fun, "puzzle_03.txt")
  where
    -- Assuming all rows have same length
    fun rows = show lifeSupportRating <>" "<>show powerConsumption
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
        remainderMatrix _ [] xs = xs
        remainderMatrix oper (index : rest) xs
          | length xs == 1 = xs
          | countAtPlace '0' `oper` countAtPlace '1' =
            remainderMatrix oper rest . remaining '0' $ xs
          | otherwise = remainderMatrix oper rest . remaining '1' $ xs
          where
            countAtPlace :: Char -> Int
            countAtPlace bit = length . filter (\x -> bit == (!! max 0 (index - 1)) x) $ xs
            remaining :: Char -> [String] -> [String]
            remaining bit = filter (hasBitAtIndex bit)
            hasBitAtIndex :: Char -> String -> Bool
            hasBitAtIndex bit x = bit == (!! max 0 (index - 1)) x

        powerConsumption = gammaRate * epsilonRate
        gammaRate = fromBinary $ commons (<)
        epsilonRate = fromBinary $ commons (>)
        commons :: (Int -> Int -> Bool) -> String
        commons oper = map (commonBits oper . nthBits rows) [1 .. (length (head rows))]
        fromBinary :: String -> Int
        fromBinary xs = case xs of
          [_] -> read xs
          _ -> read [last xs] + 2 * fromBinary (init xs)
        nthBits xs n
          | n == 1 = map head xs
          | otherwise = nthBits (map tail xs) (n -1)
        commonBits :: (Int -> Int -> Bool) -> String -> Char
        commonBits oper bits =
          if length a `oper` length b
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
    increases [_] = []
    increases (a : b : rest) = (a < b) : increases (b : rest)
    sumOf3 :: [Int] -> [Int]
    sumOf3 xs = case xs of
      (a : b : c : rest) -> a + b + c : sumOf3 (b : c : rest)
      _ -> []
