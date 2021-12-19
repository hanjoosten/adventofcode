{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Main2021
  ( getPuzzle,
    puzzle5,
    puzzle6,
    puzzle7,
    puzzle8,
    puzzle9,
    puzzle10,
    puzzle11,
    puzzle12,
    puzzle13,
    puzzle14,
    puzzle15,
    puzzle16
  )
where

import Data.Char (chr, isLower, isUpper, ord)
import Data.Either
import Data.List (intercalate, sort, union, (\\), nub)
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
getPuzzle = from puzzle16
  where
    from (a, b) = (a, show (2021 :: Int) </> b)

data Packet = Packet
  { header :: PacketHeader
  , payLoad :: PayLoad
  , pRest :: Bits
  } deriving Show
data PayLoad =
   Literal Int
 | Operator
     { packets :: [Packet]
     }
  deriving Show
newtype Bits = Bits String
instance Show Bits where
  show (Bits b) = "Bits "<>b<>" (length = "<>show (length b)<>")"
data Mode = 
       LiteralPackage
     | NumberOfPackets Int
     | NumberOfBitsOfSubpackets Int
     deriving Show
data PacketHeader = PacketHeader
  { version :: Int,
    typeId :: Int,
    modus :: Mode
  } deriving Show
puzzle16 :: ([String] -> String, FilePath)
puzzle16 = (fun, "puzzle_16.txt")
  where
    toBits :: String -> Bits
    toBits = Bits . concatMap char2Bits
      where char2Bits :: Char -> String
            char2Bits c = case c of
              '0' -> "0000"
              '1' -> "0001"
              '2' -> "0010"
              '3' -> "0011"
              '4' -> "0100"
              '5' -> "0101"
              '6' -> "0110"
              '7' -> "0111"
              '8' -> "1000"
              '9' -> "1001"
              'A' -> "1010"
              'B' -> "1011"
              'C' -> "1100"
              'D' -> "1101"
              'E' -> "1110"
              'F' -> "1111"
              _ -> error $ "No bits for "<>[c]
    bitstringOf :: Bits -> String
    bitstringOf (Bits bits) = bits
    bitsToInt :: Bits -> Int
    bitsToInt (Bits bits) = case reverse bits of -- Not very efficient, but OK for tiny strings.
      [] -> error "No Bits!"
      [_] -> read bits
      (h:tl) -> read [h] + 2 * bitsToInt (Bits $ reverse tl)
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . calculate $ input
      where
        input :: Packet
        input = readPacket . toBits . head $ rows
        calculate :: Packet -> [String]
        calculate p = lines . show . expressionValue Part2 $ p
    expressionValue part = 
      case part of
        Part1 -> sumOfVersions
        Part2 -> calculationPart2 
    calculationPart2 :: Packet -> Int
    calculationPart2 p = 
        case typeId . header $ p of
          0 -> sum subexprs
          1 -> product subexprs
          2 -> minimum subexprs
          3 -> maximum subexprs
          4 -> literal p
          5 -> if first > second then 1 else 0
          6 -> if first < second then 1 else 0
          7 -> if first == second then 1 else 0
          err -> error $ "Invalid typeId: "<>show err  
      where literal lit = case payLoad lit of
              Literal n -> n
              Operator _ -> error "This is not a literal package."
            subexprs = map calculationPart2 . packets . payLoad $ p
            first:second:_ = subexprs
    sumOfVersions :: Packet -> Int
    sumOfVersions p =
        (version . header $ p)
      + case payLoad p of
          Literal _ -> 0
          Operator{packets = ps} -> sum . map sumOfVersions $ ps
    readPacket :: Bits -> Packet
    readPacket bits = 
       Packet { header = packetHeader
              , payLoad = thePayload
              , pRest = pRest'
              }
      where (packetHeader,bodyBits) = readHeader bits
            readHeader :: Bits -> (PacketHeader,Bits)
            readHeader bs = (PacketHeader
                                    {version = bitsToInt version'
                                    ,typeId = bitsToInt typeId'
                                    ,modus = modus'
                                    }, case modus' of
                                         LiteralPackage -> afterTypeId
                                         NumberOfPackets _ -> afterModusBits
                                         NumberOfBitsOfSubpackets _ -> afterModusBits)
              where (version',afterVersion) = both Bits . splitAt 3 . bitstringOf $ bs
                    (typeId',afterTypeId) = both Bits . splitAt 3 . bitstringOf $ afterVersion
                    Bits (modusBit:afterModusBit) = afterTypeId
                    (nextModusBits,afterModusBits) = both Bits . splitAt (if modusBit == '0' then 15 else 11) $ afterModusBit
                    modus'  
                      | bitsToInt typeId' == 4 = LiteralPackage
                      | modusBit == '0' = NumberOfBitsOfSubpackets (bitsToInt nextModusBits)
                      | otherwise = NumberOfPackets (bitsToInt nextModusBits)
            (thePayload,pRest') = readBody

            readBody :: (PayLoad, Bits)
            readBody = case modus packetHeader of
                LiteralPackage -> readLiteral bodyBits
                NumberOfPackets n -> (Operator ps, remainingBits)
                  where (ps,remainingBits) = readNpackets n bodyBits
                NumberOfBitsOfSubpackets n -> (Operator ps, remainingBits)
                  where (ps,remainingBits) = (consumeNbits (readPacket bitsToConsume),leftOvers)
                         where (bitsToConsume,leftOvers) = both Bits . splitAt n . bitstringOf $ bodyBits
              where readNpackets :: Int -> Bits -> ([Packet],Bits)
                    readNpackets 0 xs = ([],xs)
                    readNpackets i leftovers = (p:ps,r)
                      where
                        (ps,r) = readNpackets (i-1) (pRest p)
                        p = readPacket leftovers
                    consumeNbits :: Packet -> [Packet]
                    consumeNbits p = p : doRest p
                        
                    doRest p
                      | length (bitstringOf . pRest $ p) < 6 = []
                      | otherwise = consumeNbits . readPacket . pRest $ p

            readLiteral :: Bits -> (PayLoad,Bits)
            readLiteral bs = ( Literal $ bitsToInt a
                             , b)
              where
                (a,b) = both Bits . readLiteral' "" . bitstringOf $ bs
                readLiteral' :: String -> String -> (String, String)
                readLiteral' ws str = case head as of
                      '1' -> readLiteral' (ws <>tail as) as'
                      '0' -> (ws <>tail as,as')
                      _ -> error $ "Error at readLiteral' "<>ws<>" "<>str
                  where (as, as') = splitAt 5 str

-- | Apply a single function to both components of a pair.
--
-- > both succ (1,2) == (2,3)
both :: (a -> b) -> (a, a) -> (b, b)
both f (x,y) = (f x, f y)

puzzle15 :: ([String] -> String, FilePath)
puzzle15 = (fun, "puzzle_15.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . calculate $ input Part2
      where
        input :: Part -> Map (Int,Int) Int
        input part = case part of
          Part1 -> Map.fromList . concatMap parseRow . zip [0..] $ rows
          Part2 -> times5 (input Part1)
          where times5 :: Map (Int, Int) Int -> Map (Int, Int) Int
                times5 baseTile = Map.unions [ tile a b | a <- [0..4],b <- [0..4]]
                  where
                    tile :: Int -> Int -> Map (Int, Int) Int
                    tile a b = mkNewMap
                      where mkNewMap :: Map (Int,Int) Int
                            mkNewMap = Map.fromList .map foo . Map.keys $ baseTile
                              where
                                foo :: (Int,Int) -> ((Int,Int),Int)
                                foo (x,y) = ((x+(width+1)*a,y+(hight+1)*b), newVal (fromJust $ Map.lookup (x,y) baseTile))
                                (width,hight) = fst . fromJust . Map.lookupMax $ baseTile
                            newVal x = if val > 9 then val - 9 else val
                              where val = sum [x,a,b]
                parseRow :: (Int,String) -> [((Int,Int),Int)]
                parseRow (rowNr,row) = zipWith (curry foo) [0..] . map (read .(: [])) $ row
                 where foo ::(Int,Int) -> ((Int,Int),Int)
                       foo (col,val) = ((col,rowNr),val)
        calculate :: Map (Int, Int) Int -> [String]
        calculate m = lines . show $ 
             dijkstra step (fst . fromJust . Map.lookupMax $ m) (0, fst . fromJust . Map.lookupMin $ m)
           where step :: (Int,(Int,Int)) -> [(Int,(Int,Int))]
                 step (cost,node) =
                      [ (cost + edgeCost, child)
                      | (edgeCost , child) <- fromMaybe [] $ Map.lookup node (graph m)
                      ]

        graph m = Map.fromList . map froms . Map.keys $ m
          where froms :: (Int,Int) -> ((Int, Int), [(Int,(Int, Int))])
                froms (row,col) -- all outgoing edges with their cost from a given node
                  = ((row,col), mapMaybe doEdge [(0,1),(1,0),(-1,0),(0,-1)])
                  where doEdge (a,b) = case Map.lookup dest m of
                          Nothing -> Nothing
                          Just n -> Just (n,dest)
                          where dest = (row + a,col+b)
dijkstra
    :: (Ord cost , Ord node)
    => ((cost , node) -> [(cost , node)]) -- ^ Where we can go from a node and the cost of that
    -> node                               -- ^ Where we want to get to
    -> (cost , node)                      -- ^ The start position
    -> Maybe (cost , node)                -- ^ Maybe the answer. Maybe it doesn't exist
dijkstra next target start = search mempty (Set.singleton start)
    where
        search visited toBeVisited = case Set.minView toBeVisited of
            Nothing -> Nothing
            Just ((cost , vertex) , withoutVertex)
                | vertex == target            -> Just (cost , vertex)
                | vertex `Set.member` visited -> search visited withoutVertex
                | otherwise                   -> search visitedWithNode withNext
                where
                    visitedWithNode = Set.insert vertex visited
                    withNext = foldr Set.insert withoutVertex $ next (cost , vertex)


type Rule = ((Char,Char),Char)
data StepResult = StepResult
       {dayNr :: Int
       ,pairCounts :: Map (Char,Char) Int
       ,charCounts :: Map Char Int
       }
puzzle14 :: ([String] -> String, FilePath)
puzzle14 = (fun, "puzzle_14.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . calculate Part2 $ input
      where
        input :: [Either String Rule]
        input = map parseRow rows
          where parseRow row = case span (/= ' ') row of
                  (first,[]) -> Left first
                  (first,rest) -> Right ((head first,head (tail first)), rest !! 4)
        calculate :: Part -> [Either String Rule] -> [String]
        calculate part xs = [showIt  $ go (case part of
                                           Part1 -> 10
                                           Part2 -> 40) (initial . head . lefts $ xs)]
          where ruleMap = Map.fromList . rights $ xs
                initial :: String -> StepResult
                initial templ = StepResult {
                    dayNr=0,
                    pairCounts=countKeys mempty . pairs $ templ ,
                    charCounts=countKeys mempty . map (, 1) $ templ
                    }
                  where
                    pairs :: [a] -> [((a, a),Int)]
                    pairs str = case str of
                      (a:b:rest) -> ((a,b),1) : pairs (b:rest)
                      _ -> []
                countKeys :: Ord k => Map k Int -> [(k,Int)] -> Map k Int
                countKeys m str =
                  case str of
                    [] -> m
                    ((key,amount):tl) ->
                        countKeys (Map.insert key amount' m) tl
                        where amount' = case Map.lookup key m of
                                Nothing -> amount
                                Just n -> n+amount
                go :: Int -> StepResult -> StepResult
                go n m
                  | n == 0 = m
                  | otherwise = go (n-1) (doStep m)
                doStep :: StepResult -> StepResult
                doStep sr = StepResult {
                    dayNr=1+dayNr sr,
                    pairCounts= countKeys mempty . concatMap newPairs . Map.toList $ pairCounts sr,
                    charCounts= countKeys
                                   (charCounts sr)
                                   . concatMap newChars . Map.toList $ pairCounts sr
                    }
                    where newPairs :: ((Char, Char), Int) -> [((Char, Char), Int)]
                          newPairs ((a,b),amount) = case Map.lookup (a, b) ruleMap of
                            Nothing -> [((a,b),amount)]
                            Just c -> [((a,c),amount),((c,b),amount)]
                          newChars :: ((Char,Char), Int) -> [(Char, Int)]
                          newChars (key,amount) = case Map.lookup key ruleMap of
                            Nothing -> []
                            Just c -> [(c,amount)]

                showIt :: StepResult -> String
                showIt sr = unlines $
                  ("After day "<>show (dayNr sr)<>":")
                  :
                  [ show a<>" - "<>show b<>" = "<>show (a-b)]
                  where a = maximum . Map.elems . charCounts $ sr
                        b = minimum . Map.elems . charCounts $ sr

type Edge = (String, String)

type Path = [String]

data Fold
  = Horizontal Int
  | Vertical Int
  deriving (Show)

puzzle13 :: ([String] -> String, FilePath)
puzzle13 = (fun, "puzzle_13.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . calculate Part2 $ input
      where
        input :: [Either (Int, Int) Fold]
        input = map parseRow rows
          where
            parseRow row =
              case span (/= ',') row of
                (foldLine, []) -> case ((!! 11) foldLine, read . drop 13 $ foldLine) of
                  ('x', line) -> Right $ Vertical line
                  ('y', line) -> Right $ Horizontal line
                  _ -> error $ "No valid row: `" <> row <> "`"
                (x, y) -> Left (read x, read . tail $ y)
        calculate :: Part -> [Either (Int, Int) Fold] -> [String]
        calculate part  xs = case part of
          Part1 -> dotCount . foldedMap' (head . rights $ xs). lefts $ xs
          Part2 -> drawMap . foldAll (rights xs) . lefts $ xs

        foldAll :: [Fold] -> [(Int,Int)] -> [(Int,Int)]
        foldAll folds xs = foldl foldedMap xs folds
        foldedMap :: [(Int,Int)] -> Fold -> [(Int,Int)]
        foldedMap xs fold = nub . map doFold $ xs
          where
            doFold :: (Int,Int) -> (Int,Int)
            doFold (a,b) = case fold of
              Horizontal n -> (a,foo n b)
              Vertical n -> (foo n a,b)
            foo n x = if n > x then x else x - 2*(x - n)
        foldedMap' :: Fold -> [(Int,Int)] -> [(Int,Int)]
        foldedMap' f xs = foldedMap xs f
        dotCount, drawMap :: [(Int,Int)] -> [String]
        dotCount dots = ["There are "<>(show . length) dots<>" dots visible."]
        drawMap dots = map drawRow [(minimum . map snd) dots..(maximum . map snd) dots]
          where
            drawRow rowNr = map drawPoint [(minimum . map fst) dots..(maximum . map fst) dots]
              where
                drawPoint colNr =
                  if (colNr,rowNr) `elem` dots
                    then '#'
                    else '.'

        _showRow :: Either (Int, Int) Fold -> String
        _showRow (Left (a, b)) = show a <> "," <> show b
        _showRow (Right foldLine) =
          "fold along " <> case foldLine of
            Horizontal n -> "y" <> "=" <> show n
            Vertical n -> "x" <> "=" <> show n

puzzle12 :: ([String] -> String, FilePath)
puzzle12 = (fun, "puzzle_12.txt")
  where
    fun :: [String] -> String
    fun rows = ("\n\n" <>) . intercalate "\n" . showIt . calculate Part2 $ input
      where
        input :: [Edge]
        input = map toEdge rows
          where
            toEdge str = (a, tail b)
              where
                (a, b) = span (/= '-') str
        calculate :: Part -> [Edge] -> [Path]
        calculate part edges = map reverse $ go ["start"] "start"
          where
            go :: Path -> String -> [Path]
            go walked current =
              if current == "end"
                then [walked]
                else case possibleNextCaves of
                  [] -> []
                  _ -> concat [go (nextCave : walked) nextCave | nextCave <- possibleNextCaves]
              where
                possibleNextCaves :: [String]
                possibleNextCaves = mapMaybe isPossible edges
                isPossible :: Edge -> Maybe String
                isPossible (s, t)
                  | s == current && validTarget t = Just t
                  | t == current && validTarget s = Just s
                  | otherwise = Nothing
                validTarget "start" = False
                validTarget t =
                  (isUpper . head) t
                    || t `notElem` walked
                    || part == Part2 && noDoublesYet
                noDoublesYet = length visitedSmallCaves == length (Set.fromList visitedSmallCaves)
                visitedSmallCaves = filter (isLower . head) walked
        showIt :: [Path] -> [String]
        showIt paths =
          --  (map _showPath $ paths) ++
          [show (length paths)]
    _showPath :: Path -> String
    _showPath = intercalate " -> "

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
            go flashes dayNr' area =
              case part of
                Part1 ->
                  if dayNr' == 100
                    then (flashes, area)
                    else next
                Part2 ->
                  if traceDayflashes dayFlashes == 100
                    then (flashes + dayFlashes, nextDayArea)
                    else next
              where
                traceDayflashes i = trace ("Amount of flashes after day " <> show (dayNr' + 1) <> ": " <> show i) i
                next = (if part == Part1 then traceNext else id) $ go (flashes + dayFlashes) (dayNr' + 1) nextDayArea
                traceNext :: (Int, Area Value11) -> (Int, Area Value11)
                traceNext x =
                  trace
                    ( unlines $
                        [ "dayNr: " <> show dayNr',
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
