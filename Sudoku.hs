module Main where

import Control.Exception.Base(assert)
import Data.Array
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Square = Known Int | Possibilities (Set Int) deriving (Eq)
allPossibilities :: Set Int
allPossibilities = Set.fromList [1..9]

unknown = Possibilities allPossibilities

type Index = Int
type Possibility = Int
type Board = Array Index Square

data RowCol = RowCol Int Int deriving (Eq, Show)

rowLeft :: RowCol -> [RowCol]
rowLeft (RowCol r c) = let
  colsLeft = [0..(c - 1)]
  in map (RowCol r) colsLeft

rowRight :: RowCol -> [RowCol]
rowRight (RowCol r c) = let
  colsRight = [(c + 1)..8]
  in map (RowCol r) colsRight

columnUp :: RowCol -> [RowCol]
columnUp (RowCol r c) = let
  rowsUp = [0..(r - 1)]
  in map (\newRow -> RowCol newRow c) rowsUp

columnDown :: RowCol -> [RowCol]
columnDown (RowCol r c) = let
  rowsDown = [(r + 1)..8]
  in map (\newRow -> RowCol newRow c) rowsDown

boxMates :: RowCol -> [RowCol]
boxMates (RowCol r c) = let
  highest = 3 * (r `div` 3)
  boxRows = [highest, highest + 1, highest + 2]
  leftmost = 3 * (c `div` 3)
  boxColumns = [leftmost, leftmost + 1, leftmost + 2]
  -- normally I hate list comprehension but
  in [ RowCol r1 c1 | r1<-boxRows, c1<-boxColumns, (r1, c1) /= (r, c) ]

-- this will have four dupes and I don't care
allIndexNeighbors :: Index -> [Index]
allIndexNeighbors i = filter (/= i) $ concat
  [ indicesForRow (rowForIndex i)
  , indicesForColumn (columnForIndex i)
  , indicesForBox (boxForIndex i)]

eliminatePossibility :: Possibility -> Square -> Square
eliminatePossibility i rc = case rc of
  Known k -> if (k == i)
               then error ("I tried to eliminate " ++ (show i) ++
                           " from its own square. Why?")
               else rc
  Possibilities s -> let
    newPossibilities = Set.delete i s
    in assert (not $ Set.null newPossibilities) $ Possibilities newPossibilities

rowColIndex :: RowCol -> Int
rowColIndex (RowCol r c) = (9 * r) + c

indexRowCol :: Int -> RowCol
indexRowCol i = assert ((i >= 0) && (i < 81)) (RowCol (i `div` 9) (i `mod` 9))

rowForIndex :: Index -> Int
rowForIndex i = i `div` 9

columnForIndex :: Index -> Int
columnForIndex i = i `mod` 9

showSquare :: Square -> String
showSquare (Known i) = show i
showSquare (Possibilities s) = concat $ map show $ Set.toList s

instance Show Square where
  show = showSquare

showSquarePadded :: Int -> Square -> String
showSquarePadded l sq = let
  contents = showSquare sq
  padding = take (l - (length contents)) $ repeat ' '
  in contents ++ padding

-- this ends up with 90 character lines but I am still thinking about it
showRow :: [Square] -> String
showRow xs = intercalate " " (map (showSquarePadded 9) xs)

splitEvery :: Int -> [a] -> [[a]]
splitEvery i [] = []
splitEvery i xs = (take i xs):(splitEvery i (drop i xs))

showBoard :: Board -> [String]
showBoard b = map showRow (splitEvery 9 (elems b))

readDigitOrCrash :: Char -> Int
readDigitOrCrash c = let
  i = (read [c] :: Int)
  in assert ((i > 0) && (i < 10)) i

charToSquare :: Char -> Square
charToSquare ' ' = unknown
charToSquare '0' = unknown
charToSquare '.' = unknown
charToSquare c = Known (readDigitOrCrash c)

-- https://www.websudoku.com/?level=4&set_id=3381457023
--testPuzzleRows = [" 3   1 9 ",
--                  " 982     ",
--                  " 5 8  7  ",
--                  " 24 8    ",
--                  "  3   5  ",
--                  "    3 17 ",
--                  "  7  9 1 ",
--                  "     648 ",
--                  " 4 5   6 "]
testPuzzleRows2 = ["  46     ",
                   "  32 9   ",
                   " 12 4   8",
                   "      9 4",
                   "4   8   6",
                   "1 9      ",
                   "7   3 21 ",
                   "   7 28  ",
                   "     15  "]

testPuzzleRows = "004300209005009001070060043006002087190007400050083000600000105003508690042910300"

testBoard = parseBoard testPuzzleRows
testProgress = tryToSolve testBoard

parseRows :: [String] -> Board
parseRows rows = parseBoard $ concat rows

parseBoard :: String -> Board
parseBoard oneBigString = let
  squares = map charToSquare oneBigString
  pairs:: [(Int, Square)]
  pairs = zip [0..80] squares
  in array (0,80) pairs


fixSinglePossibility :: Square -> Square
fixSinglePossibility sq = case sq of
  (Possibilities s) -> if (Set.size s == 1) then Known (Set.findMin s) else sq
  _                 -> sq

reduceFromKnownSquare :: Board -> Int -> Board
reduceFromKnownSquare b i = let
  newBoard = b//[(i, fixSinglePossibility (b!i))]
  neighbors = allIndexNeighbors i
  in case newBoard!i of
    Known knownValue -> eliminateFromIndices newBoard knownValue neighbors
    _                -> newBoard

eliminateFromIndices :: Board -> Possibility -> [Index] -> Board
eliminateFromIndices board possibility indices = let
  makeUpdate :: Index -> (Index, Square)
  makeUpdate i = (i, eliminatePossibility possibility (board!i))
  allUpdates = map makeUpdate indices
  in board//allUpdates

initialReduceFromKnown :: Board -> Board
initialReduceFromKnown b = foldl reduceFromKnownSquare b [0..80]

iterateUntilStable :: Eq a => (a -> a) -> a -> (Int, a)
iterateUntilStable f x = iterateUntilStable0 f x 1

indicesForRow :: Int -> [Int]
indicesForRow r = [(9 * r)..(9 * r + 8)]

indicesForColumn :: Int -> [Int]
indicesForColumn c = map (\r -> r * 9 + c) [0..8]

indicesForBox :: Int -> [Int]
indicesForBox b = let
  upperLeft = 27 * (b `div` 3) + 3 * (b `mod` 3)
  in [ 9 * x + y + upperLeft | x<-[0..2], y<-[0..2] ]
-- 0 -> 0-2,9-11,18-20

indicesForHouse :: HouseID -> [Int]
indicesForHouse (HouseID Row r) = indicesForRow r
indicesForHouse (HouseID Column c) = indicesForColumn c
indicesForHouse (HouseID Box b) = indicesForBox b

allHouses :: [HouseID]
allHouses = [ (HouseID hType x) | hType<-[Row, Column, Box], x<-[0..8]]

boxForIndex :: Index -> Int
boxForIndex i = let
  RowCol r c = indexRowCol i
  in 3 * (r `div` 3) + (c `div` 3)

allIndexLists :: [[Int]]
allIndexLists = [ f list | f<-[indicesForRow, indicesForColumn, indicesForBox], list<-[0..8]]

type DigitsToIndices = Map Possibility (Set Index)
type IndexSetUnions = Map (Set Index) (Set Possibility)

data HouseType = Row | Column | Box deriving (Eq, Show)
data NineSquareData = NineSquareData { digitsToIndices :: DigitsToIndices
                                     , indexSetUnions :: IndexSetUnions
                                     , house :: HouseID } deriving (Eq, Show)

data HouseID = HouseID HouseType Int deriving (Eq, Show)

addPossibilityToTable :: DigitsToIndices -> (Int, Int) -> DigitsToIndices
addPossibilityToTable t (index, possibility) = Map.insertWith Set.union possibility (Set.singleton index) t

-- updateIndexSetUnions :: IndexSetUnions -> Set Possibility -> Index -> IndexSetUnions
-- updateIndexSetUnions t possibilities index = Map.insertWith Set.union possibilities (Set.singleton index) t

-- get rid of this when we get containers 0.5.11 or later
powerSet :: Ord a => Set a -> Set (Set a)
powerSet set = let
  min = Set.findMin set
  withoutMin = powerSet $ Set.deleteMin set
  withMin = Set.map (Set.insert min) withoutMin
  fancySubsets = Set.union withoutMin withMin
  in if Set.null set then (Set.singleton Set.empty) else fancySubsets

addPossibilitySetToTables :: NineSquareData -> Int -> Set Int -> NineSquareData
addPossibilitySetToTables (NineSquareData oldPTable oldIndexSets house) index possibilities = let
  dPairs = zip (repeat index) (Set.toList possibilities)
  newDigits = foldl addPossibilityToTable oldPTable dPairs
  newIndexSets = updateIndexSetUnions oldIndexSets index possibilities
  in NineSquareData newDigits newIndexSets house

updateIndexSetUnions :: IndexSetUnions -> Int -> Set Int -> IndexSetUnions
updateIndexSetUnions oldISU index possibilities = let
  oldPairs :: [(Set Index, Set Possibility)]
  oldPairs = Map.toList oldISU
  newSingletonPair = (Set.singleton index, possibilities)
  makeNewSet :: (Set Index, Set Possibility) -> (Set Index, Set Possibility)
  makeNewSet (oldI, oldP) = (Set.insert index oldI, Set.union possibilities oldP)
  newPairs = map makeNewSet oldPairs
  newISU = Map.fromList (newSingletonPair:newPairs)
  in Map.unionWith (error "duplicate key") oldISU newISU
 
addToTableFromBoardSquare :: Board -> NineSquareData -> Index -> NineSquareData
addToTableFromBoardSquare board table index = case board!index of
  Possibilities s -> addPossibilitySetToTables table index s
  _               -> table

makeTableFromBoardSquares :: Board -> [Index] -> NineSquareData
makeTableFromBoardSquares board indices = let
  emptyNSD = NineSquareData Map.empty Map.empty undefined
  in foldl (addToTableFromBoardSquare board) emptyNSD indices

makeTableFromHouse :: Board -> HouseID -> NineSquareData
makeTableFromHouse board house = let
  indices = indicesForHouse house
  emptyNSD = NineSquareData Map.empty Map.empty house
  in foldl (addToTableFromBoardSquare board) emptyNSD indices

iterateUntilStable0 f x i = let
  output = f x
  in if (output == x) then (i, x) else (iterateUntilStable0 f output (i + 1))

updateBoardUsingIndexSet :: HouseID -> Board -> (Set Index, Set Possibility) -> Board
updateBoardUsingIndexSet house board (indices, possibilities) = let
  indicesToUpdate = filter (\i -> Set.notMember i indices) $ indicesForHouse house
  eliminate :: Board -> Possibility -> Board
  eliminate b p = eliminateFromIndices b p indicesToUpdate
  eliminateAll = foldl eliminate board (Set.toList possibilities)
  canEliminate = Set.size indices == Set.size possibilities
  in if canEliminate then eliminateAll else board

updateBoardUsingEntry :: HouseID -> Board -> (Possibility, Set Index) -> Board
updateBoardUsingEntry house b (possibility, set) = case (Set.size set) of
  0 -> error "How did I get a set of size zero here?"
  1 -> reduceFromKnownSquare (b//[(Set.findMin set, Known possibility)]) (Set.findMin set)
  _ -> reduceLockedCandidates b (possibility, set) house

reduceLockedCandidates :: Board -> (Possibility, Set Index) -> HouseID -> Board
reduceLockedCandidates b (possibility, set) house = case (findLockedCandidatesFromHouse house $ Set.toList set) of
  Just house -> eliminatePossibilityFromLockedCandidates b possibility set house
  Nothing    -> b

eliminatePossibilityFromLockedCandidates :: Board -> Possibility -> Set Index -> HouseID -> Board
eliminatePossibilityFromLockedCandidates b p s h = let
  indicesToUpdate = filter (\i -> Set.notMember i s) $ indicesForHouse h
  in eliminateFromIndices b p indicesToUpdate

findLockedCandidatesFromHouse :: HouseID -> [Index] -> Maybe HouseID
findLockedCandidatesFromHouse (HouseID hType _) indices = case hType of
  Box -> indicesAreSameRowOrColumn indices
  _   -> indicesAreSameBox indices
  
updateBoardUsingTable :: Board -> NineSquareData -> Board
updateBoardUsingTable board (NineSquareData dTable indexSets h) = let
  afterHiddenSingles = foldl (updateBoardUsingEntry h) board (Map.toList dTable)
  afterIndexSets = foldl (updateBoardUsingIndexSet h) afterHiddenSingles (Map.toList indexSets)
  in afterIndexSets

updateBoardUsingIndices :: Board -> [Index] -> Board
updateBoardUsingIndices board indices = updateBoardUsingTable board (makeTableFromBoardSquares board indices)

updateBoardUsingAllHiddenSingles :: Board -> Board
updateBoardUsingAllHiddenSingles board = foldl updateBoardUsingIndices board allIndexLists

updateBoardUsingHouse :: Board -> HouseID -> Board
updateBoardUsingHouse board house = updateBoardUsingTable board (makeTableFromHouse board house)

updateBoardUsingAllHouses :: Board -> Board
updateBoardUsingAllHouses board = foldl updateBoardUsingHouse board allHouses

tryToSolve :: Board -> Board
tryToSolve = snd . (iterateUntilStable updateBoardUsingAllHouses) . initialReduceFromKnown

indicesAreSameRow :: [Int] -> Maybe HouseID
indicesAreSameRow [] = error "indicesAreSameRow on empty list"
indicesAreSameRow [i] = Just $ HouseID Row (i `div` 9)
indicesAreSameRow (x:y:xs) = if (x `div` 9 == y `div` 9) then indicesAreSameRow (y:xs) else Nothing

indicesAreSameColumn :: [Int] -> Maybe HouseID
indicesAreSameColumn [] = error "indicesAreSameColumn on empty list"
indicesAreSameColumn [i] = Just $ HouseID Column (i `mod` 9)
indicesAreSameColumn (x:y:xs) = if (x `mod` 9 == y `mod` 9) then indicesAreSameColumn (y:xs) else Nothing

indicesAreSameBox :: [Int] -> Maybe HouseID
indicesAreSameBox [] = error "indicesAreSameBox on empty list"
indicesAreSameBox indices = let
  (x:xs) = map boxForIndex indices
  in if (all (== x) xs) then Just (HouseID Box x) else Nothing

indicesAreSameRowOrColumn :: [Int] -> Maybe HouseID
indicesAreSameRowOrColumn indices = case indicesAreSameRow indices of
  Just r -> Just r
  Nothing -> indicesAreSameColumn indices
