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
allNeighbors :: RowCol -> [RowCol]
allNeighbors rc = concat [rowLeft rc, rowRight rc, columnUp rc, columnDown rc,
                          boxMates rc]

eliminatePossibility :: Int -> Square -> Square
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
charToSquare c = Known (readDigitOrCrash c)

-- https://www.websudoku.com/?level=4&set_id=3381457023
testPuzzleRows = [" 3   1 9 ",
                  " 982     ",
                  " 5 8  7  ",
                  " 24 8    ",
                  "  3   5  ",
                  "    3 17 ",
                  "  7  9 1 ",
                  "     648 ",
                  " 4 5   6 "]

testBoard = parseRows testPuzzleRows
testProgress = snd $ iterateUntilStable reduceFromAllSquares testBoard

parseRows :: [String] -> Board
parseRows rows = let
  oneBigString = concat rows
  squares = map charToSquare oneBigString
  pairs:: [(Int, Square)]
  pairs = zip [0..80] squares
  in array (0,80) pairs

fixSinglePossibility :: Square -> Square
fixSinglePossibility sq = case sq of
  (Possibilities s) -> if (Set.size s == 1) then Known (Set.findMin s) else sq
  _                 -> sq

reduceFromSquare :: Board -> Int -> Board
reduceFromSquare b i = let
  newBoard = b//[(i, fixSinglePossibility (b!i))]
  neighbors = map rowColIndex $ allNeighbors $ indexRowCol i
  makeUpdate :: Int -> Int -> (Int, Square)
  makeUpdate v j = (j, eliminatePossibility v (newBoard!j))
  allUpdates v = map (makeUpdate v) neighbors
  in case newBoard!i of
    Known knownValue -> newBoard//(allUpdates knownValue)
    _                -> newBoard

reduceFromAllSquares :: Board -> Board
reduceFromAllSquares b = foldl reduceFromSquare b [0..80]

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

boxForIndex :: Index -> Int
boxForIndex i = let
  RowCol r c = indexRowCol i
  in (r `div` 3) + (c `div` 3)

allIndexLists :: [[Int]]
allIndexLists = [ f list | f<-[indicesForRow, indicesForColumn, indicesForBox], list<-[0..8]]

type DigitsToIndices = Map Possibility (Set Index)
type DigitSetsToIndices = Map (Set Possibility) (Set Index)

data NineSquareData = NineSquareData { digitsToIndices :: DigitsToIndices
                                     , digitSetsToIndices :: DigitSetsToIndices } deriving (Eq, Show)

addPossibilityToTable :: DigitsToIndices -> (Int, Int) -> DigitsToIndices
addPossibilityToTable t (index, possibility) = Map.insertWith Set.union possibility (Set.singleton index) t

updateDigitSetsToIndices :: DigitSetsToIndices -> Set Possibility -> Index -> DigitSetsToIndices
updateDigitSetsToIndices t possibilities index = Map.insertWith Set.union possibilities (Set.singleton index) t

addPossibilitySetToTables :: NineSquareData -> Int -> Set Int -> NineSquareData
addPossibilitySetToTables (NineSquareData oldPTable oldSetTable) index possibilities = let
  dPairs = zip (repeat index) (Set.toList possibilities)
  newDigits = foldl addPossibilityToTable oldPTable dPairs
  newSets = updateDigitSetsToIndices oldSetTable possibilities index
  in NineSquareData newDigits newSets
 
addToTableFromBoardSquare :: Board -> NineSquareData -> Index -> NineSquareData
addToTableFromBoardSquare board table index = case board!index of
  Possibilities s -> addPossibilitySetToTables table index s
  _               -> table

makeTableFromBoardSquares :: Board -> [Index] -> NineSquareData
makeTableFromBoardSquares board indices = let
  emptyNSD = NineSquareData Map.empty Map.empty
  in foldl (addToTableFromBoardSquare board) emptyNSD indices

iterateUntilStable0 f x i = let
  output = f x
  in if (output == x) then (i, x) else (iterateUntilStable0 f output (i + 1))

updateBoardUsingEntry :: Board -> (Int, Set Int) -> Board
updateBoardUsingEntry b (possibility, set) = case (Set.size set) of
  0 -> error "How did I get a set of size zero here?"
  1 -> reduceFromSquare (b//[(Set.findMin set, Known possibility)]) (Set.findMin set)
  _ -> b

updateBoardUsingTable :: Board -> NineSquareData -> Board
updateBoardUsingTable board (NineSquareData dTable _) = foldl updateBoardUsingEntry board (Map.toList dTable)

updateBoardUsingIndices :: Board -> [Index] -> Board
updateBoardUsingIndices board indices = updateBoardUsingTable board (makeTableFromBoardSquares board indices)

updateBoardUsingAllHiddenSingles :: Board -> Board
updateBoardUsingAllHiddenSingles board = foldl updateBoardUsingIndices board allIndexLists

performAllOperations :: Board -> Board
performAllOperations = reduceFromAllSquares . updateBoardUsingAllHiddenSingles

indicesAreSameRow :: [Int] -> Maybe Int
indicesAreSameRow [] = error "indicesAreSameRow on empty list"
indicesAreSameRow [i] = Just (i `div` 9)
indicesAreSameRow (x:y:xs) = if (x `div` 9 == y `div` 9) then indicesAreSameRow (y:xs) else Nothing

indicesAreSameColumn :: [Int] -> Maybe Int
indicesAreSameColumn [] = error "indicesAreSameColumn on empty list"
indicesAreSameColumn [i] = Just (i `mod` 9)
indicesAreSameColumn (x:y:xs) = if (x `mod` 9 == y `mod` 9) then indicesAreSameColumn (y:xs) else Nothing

indicesAreSameBox :: [Int] -> Maybe Int
indicesAreSameBox [] = error "indicesAreSameBox on empty list"
indicesAreSameBox indices = let
  (x:xs) = map boxForIndex xs
  in if (all (== x) xs) then Just x else Nothing


