module KenKen where

import Control.Exception.Base(assert)
import Data.Array (Array, (!), (//))
import qualified Data.Array as Array
import Data.List (intercalate, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified KenKenDecoder as KenKenDecoder

data Square = Known Int | Possibilities (Set Int) deriving (Eq, Show)
allPossibilities :: Set Int
allPossibilities = Set.fromList [1..9]

unknown = Possibilities allPossibilities

type Index = Int
type Possibility = Int
type Board = Array Index Square
data BoardKK = BoardKK { getBoard :: Board, getMathGroups :: [MathGroup] } deriving (Eq, Show)

isSolved :: Board -> Bool
isSolved b = let
  isKnown (Known i) = True
  isKnown _ = False
  in all isKnown $ Array.elems b

isSolvedKK :: BoardKK -> Bool
isSolvedKK (BoardKK b _) = isSolved b

isSatisfiable :: Board -> Bool
isSatisfiable b = let
  isEmpty (Possibilities s) = Set.null s
  isEmpty _ = False
  in not $ any isEmpty $ Array.elems b

-- this will have four dupes and I don't care
allIndexNeighbors :: Index -> [Index]
allIndexNeighbors i = filter (/= i) $ concat
  [ indicesForRow (rowForIndex i)
  , indicesForColumn (columnForIndex i)]

eliminatePossibility :: Possibility -> Square -> Square
eliminatePossibility i rc = case rc of
  Known k -> if (k == i)
               then error ("I tried to eliminate " ++ (show i) ++
                           " from its own square. Why?")
               else rc
  Possibilities s -> let
    newPossibilities = Set.delete i s
    in Possibilities newPossibilities


rowForIndex :: Index -> Int
rowForIndex i = i `div` 9

columnForIndex :: Index -> Int
columnForIndex i = i `mod` 9

showSquare :: Square -> String
showSquare (Known i) = show i
showSquare (Possibilities s) = concat $ map show $ Set.toList s

-- instance Show Square where
--   show = showSquare

showSquarePadded :: Int -> Square -> String
showSquarePadded l sq = let
  contents = showSquare sq
  padding = take (l - (length contents)) $ repeat ' '
  in contents ++ padding

-- this ends up with 90 character lines but I am still thinking about it
showRow :: [Int] -> [Square] -> String
showRow paddings squares = let
  pairs = zip squares paddings
  showPair (s, p) = showSquarePadded p s
  in concat $ map showPair pairs

makePostPadding :: Board -> [Int]
-- the last column gets no post-padding. every other column gets 1 plus its
-- widest column.
makePostPadding b = let
  squareWidth sq = case sq of
    Possibilities s -> Set.size s
    _               -> 1
  columnSquares c = [b!i | i<-(indicesForColumn c)]
  columnWidth :: Int -> Int
  columnWidth c = maximum (map squareWidth $ columnSquares c)
  columnPadding :: Int -> Int
  columnPadding 8 = 0
  columnPadding c = 1 + (columnWidth c)
  in map columnPadding [0..8]

splitEvery :: Int -> [a] -> [[a]]
splitEvery i [] = []
splitEvery i xs = (take i xs):(splitEvery i (drop i xs))

showBoard :: Board -> IO ()
showBoard b = putStrLn $ intercalate "\n" $ map (showRow (makePostPadding b))
                                                (splitEvery 9 (Array.elems b))

showBoardKK :: BoardKK -> IO ()
showBoardKK (BoardKK b gs) = do
  showBoard b
  putStrLn $ show gs
  
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

testBoard = BoardKK (parseBoard testPuzzleRows) []
testProgress = tryToSolve testBoard

parseRows :: [String] -> Board
parseRows rows = parseBoard $ concat rows

parseBoard :: String -> Board
parseBoard oneBigString = let
  squares = map charToSquare oneBigString
  pairs:: [(Int, Square)]
  pairs = zip [0..80] squares
  in case (length oneBigString) of
    81 -> Array.array (0,80) pairs
    _  -> error "Invalid board input"

fixSinglePossibility :: Square -> Square
fixSinglePossibility sq = case sq of
  (Possibilities s) -> if (Set.size s == 1) then Known (Set.findMin s) else sq
  _                 -> sq

-- So first we check if there is only one possibility in a square.
-- if there is we replace that with a Known.
-- and then we eliminate that number from all of this square's neighbors
-- if there is more than one possibility I think this function does nothing
-- KENKEN: maybe we'll reduce the mathematical neighbors here too, maybe not
reduceFromKnownSquare :: BoardKK -> Int -> BoardKK
reduceFromKnownSquare (BoardKK b gs) i = let
  newBoard = b//[(i, fixSinglePossibility (b!i))]
  neighbors = allIndexNeighbors i
  fixedNeighbors k = eliminateFromIndices newBoard k neighbors
  fixedMathGroups k = fixMathGroupsFromKnown (BoardKK (fixedNeighbors k) gs) i k
  in case newBoard!i of
    Known knownValue -> fixedMathGroups knownValue
    _                -> BoardKK newBoard gs

groupContainsIndex ::  Index -> MathGroup -> Bool
groupContainsIndex i (MathGroup _ _ is) = Set.member i is

fixMathGroupsFromKnown :: BoardKK -> Index -> Possibility -> BoardKK
fixMathGroupsFromKnown (BoardKK board mgs) i v = let
  (relevantMGs, otherMGs) = partition (groupContainsIndex i) mgs
  onlyRelevantMG = if (length relevantMGs == 1) then head relevantMGs else error ("why is there more than one MG for an index? " ++ show (length relevantMGs, relevantMGs))
  newMGIfNecessary = reduceGroupWithIndexValueKnown i v onlyRelevantMG
  updatesIfNecessary = [reduceGroupWithIndexValue2 i v onlyRelevantMG]
  newBoardIfNecessary = board//updatesIfNecessary
  indexCount = Set.size $ getIndices onlyRelevantMG
  (nextBoard, nextMGs)
    | null relevantMGs = (board, mgs)
    | indexCount == 0  = error "why are there no indices in this MG"
    | indexCount == 1  = (board, otherMGs) -- just get rid of the current one
    | indexCount == 2  = (newBoardIfNecessary, otherMGs)
    | otherwise        = (board, newMGIfNecessary:otherMGs)
  in BoardKK nextBoard nextMGs  

eliminateFromIndices :: Board -> Possibility -> [Index] -> Board
eliminateFromIndices board possibility indices = let
  makeUpdate :: Index -> (Index, Square)
  makeUpdate i = (i, eliminatePossibility possibility (board!i))
  allUpdates = map makeUpdate indices
  in board//allUpdates

initialReduceFromKnown :: BoardKK -> BoardKK
initialReduceFromKnown b = foldl reduceFromKnownSquare b [0..80]

iterateUntilStable :: Eq a => (a -> a) -> a -> (Int, a)
iterateUntilStable f x = iterateUntilStable0 f x 1

indicesForRow :: Int -> [Int]
indicesForRow r = [(9 * r)..(9 * r + 8)]

indicesForColumn :: Int -> [Int]
indicesForColumn c = map (\r -> r * 9 + c) [0..8]

indicesForHouse :: HouseID -> [Int]
indicesForHouse (HouseID Row r) = indicesForRow r
indicesForHouse (HouseID Column c) = indicesForColumn c

allHouses :: [HouseID]
allHouses = [ (HouseID hType x) | hType<-[Row, Column], x<-[0..8]]

boxForIndex :: Index -> Int
boxForIndex i = 3 * ((rowForIndex i) `div` 3) + ((columnForIndex i) `div` 3)

allIndexLists :: [[Int]]
allIndexLists = [ f list | f<-[indicesForRow, indicesForColumn],
                           list<-[0..8]]

type DigitsToIndices = Map Possibility (Set Index)
type IndexSetUnions = Map (Set Index) (Set Possibility)

data HouseType = Row | Column deriving (Eq, Show)
-- ok DigitsToIndices is like "for each digit, which indices can it be in"
-- that's how you know "all the 2s in this column are in the same box, which means
-- they can't be in another column in that box"
-- IndexSetUnions is like "for each possible subset of indices, what is the
-- union of the possibilities for that set." that's how you realize that three
-- given indices have a total set of possibilities of size three, which means
-- no other index in the house can have any of those values.
-- KENKEN: I think we can only use DigitsToIndices when the value is of size 1.
-- I think we can still use IndexSetUnions?
data NineSquareData = NineSquareData { digitsToIndices :: DigitsToIndices
                                     , indexSetUnions :: IndexSetUnions
                                     , house :: HouseID } deriving (Eq, Show)

data HouseID = HouseID HouseType Int deriving (Eq, Show)

addPossibilityToTable :: DigitsToIndices -> (Int, Int) -> DigitsToIndices
addPossibilityToTable t (index, possibility) =
  Map.insertWith Set.union possibility (Set.singleton index) t

addPossibilitySetToTables :: NineSquareData -> Int -> Set Int -> NineSquareData
addPossibilitySetToTables (NineSquareData oldPTable oldIndexSets house)
                          index possibilities = let
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
  makeNewSet (oldI, oldP) = (Set.insert index oldI,
                             Set.union possibilities oldP)
  newPairs = map makeNewSet oldPairs
  newISU = Map.fromList (newSingletonPair:newPairs)
  in Map.unionWith (error "duplicate key") oldISU newISU
 
addToTableFromBoardSquare :: Board -> NineSquareData -> Index -> NineSquareData
addToTableFromBoardSquare board table index = case board!index of
--  Known k         -> addPossibilitySetToTables table index (Set.singleton k)
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

-- ok so this says "if indices [x,y] combined only have digits [a,b] then no other
-- index can have a or b".
updateBoardUsingIndexSet :: HouseID -> Board -> (Set Index, Set Possibility)
                            -> Board
updateBoardUsingIndexSet house board (indices, possibilities) = let
  indicesToUpdate = filter (\i -> Set.notMember i indices)
                    $ indicesForHouse house
  eliminate :: Board -> Possibility -> Board
  eliminate b p = eliminateFromIndices b p indicesToUpdate
  eliminateAll = foldl eliminate board (Set.toList possibilities)
  canEliminate = Set.size indices == Set.size possibilities
  in if canEliminate then eliminateAll else board

-- this does not seem to handle the case where there is exactly one 2 in a row
-- reduceLockedCandidates :: HouseID -> Board -> (Possibility, Set Index) -> Board
-- reduceLockedCandidates house b (possibility, set) = let
--  answer = case (findLockedCandidatesFromHouse house $ Set.toList set) of
--    Just house -> eliminatePossibilityFromLockedCandidates b possibility set
--                  house
--    Nothing    -> b
--  in assert ((Set.size set) > 0) answer

-- I think the idea is "if all the 2s in this row are in the same box, then we
-- can eliminate all the other 2s in that box, because the 2 from that box HAS
-- to be in this row". (Replace row with column and also flip row/column with box.)
-- KENKEN: I feel like this logic might be entirely irrelevant to Kenken?
eliminatePossibilityFromLockedCandidates :: Board -> Possibility -> Set Index ->
                                            HouseID -> Board
eliminatePossibilityFromLockedCandidates b p s h = let
  indicesToUpdate = filter (\i -> Set.notMember i s) $ indicesForHouse h
  in eliminateFromIndices b p indicesToUpdate

-- okay so first we do something with "locked candidates"
-- KENKEN: which I think is totally irrelevant?
-- actually when do we hit the case where there is only one 5 in a row.
-- then we do something with "index sets"

updateBoardUsingTable :: Board -> NineSquareData -> Board
updateBoardUsingTable board (NineSquareData dTable indexSets h) = let
  afterLockedCandidates = board -- we don't do locked stuff
  afterIndexSets = foldl (updateBoardUsingIndexSet h) afterLockedCandidates
                   (Map.toList indexSets)
  in afterIndexSets

-- this just calls reduceFromKnownSquare on every square in a house, which does
-- nothing with squares that have >1 possibility
reduceKnownsInHouse :: BoardKK -> HouseID -> BoardKK
reduceKnownsInHouse board house =
  foldl reduceFromKnownSquare board (indicesForHouse house)

updateBoardUsingHouse :: BoardKK -> HouseID -> BoardKK
updateBoardUsingHouse board house = let
  (BoardKK board2 gs2) = reduceKnownsInHouse board house
  board3 = updateBoardUsingTable board2 (makeTableFromHouse board2 house)
  in BoardKK board3 gs2

updateBoardUsingAllHouses :: BoardKK -> BoardKK
updateBoardUsingAllHouses board = foldl updateBoardUsingHouse board allHouses

updateBoardUsingMathGroup :: BoardKK -> MathGroup -> BoardKK
-- possibilitiesForMathGroup :: Board -> MathGroup -> Map Index (Set Possibility)
updateBoardUsingMathGroup (BoardKK board mgs) mg = let
  possMap = possibilitiesForMathGroup board mg
  asPairs = Map.toList possMap
  allUpdates = map (\(i,set) -> (i, Possibilities set)) asPairs
  newBoard = board//allUpdates
  in BoardKK newBoard mgs

updateBoardUsingAllMathGroups :: BoardKK -> BoardKK
updateBoardUsingAllMathGroups bkk = let
  BoardKK _ mgs = bkk
  in foldl updateBoardUsingMathGroup bkk mgs

updateBoardUsingAllLogic :: BoardKK -> BoardKK
updateBoardUsingAllLogic = updateBoardUsingAllHouses . updateBoardUsingAllMathGroups

tryToSolve :: BoardKK -> BoardKK
tryToSolve = snd . (iterateUntilStable updateBoardUsingAllLogic)
             . initialReduceFromKnown

indicesAreSameRow :: [Int] -> Maybe HouseID
indicesAreSameRow [] = error "indicesAreSameRow on empty list"
indicesAreSameRow [i] = Just $ HouseID Row (i `div` 9)
indicesAreSameRow (x:y:xs) = if (rowForIndex x == rowForIndex y)
                             then indicesAreSameRow (y:xs) else Nothing

indicesAreSameColumn :: [Int] -> Maybe HouseID
indicesAreSameColumn [] = error "indicesAreSameColumn on empty list"
indicesAreSameColumn [i] = Just $ HouseID Column (i `mod` 9)
indicesAreSameColumn (x:y:xs) = if (columnForIndex x == columnForIndex y)
                                then indicesAreSameColumn (y:xs) else Nothing

indicesAreSameRowOrColumn :: [Int] -> Maybe HouseID
indicesAreSameRowOrColumn indices = case indicesAreSameRow indices of
  Just r -> Just r
  Nothing -> indicesAreSameColumn indices

possibilities :: Square -> Set Possibility
possibilities (Known _) = error "You did it wrong"
possibilities (Possibilities s) = s

makeGuessBoards :: BoardKK -> [BoardKK]
makeGuessBoards (BoardKK board gs) = let
  isKnown (Known i) = True
  isKnown _ = False
  firstUnknown = head $ filter (not . isKnown . snd) $ Array.assocs board
  guesses = Set.toList $ possibilities $ snd firstUnknown
  index = fst firstUnknown
  in map (\g -> BoardKK(board//[(index, Known g)]) gs) guesses

solveWithGuesses :: BoardKK -> BoardKK
solveWithGuesses board = let
  BoardKK board2 gs2 = tryToSolve board
  guessBoards = makeGuessBoards (BoardKK board2 gs2)
  results = map solveWithGuesses guessBoards
  successes = filter isSolvedKK $ results
  result = if null successes then head results else head successes
  in if (isSolved board2 || (not . isSatisfiable) board2)
     then (BoardKK board2 []) else result

applyGuessWithMathGroup3 :: Index ->  Board -> MathGroup -> Possibility -> Map Index (Set Possibility)
applyGuessWithMathGroup3 i board mg v = let
  board2 = board//[(i, Known v)]
  BoardKK board3 _ = reduceFromKnownSquare (BoardKK board2 []) i
  mg2Maybe = reduceGroupWithIndexValue i v mg
  in case mg2Maybe of
    Nothing  -> Map.empty
    Just mg2 -> possibilitiesForMathGroup board3 mg2

applyGuessWithMathGroup2 :: Index ->  Board -> MathGroup -> Possibility -> Map Index (Set Possibility)
applyGuessWithMathGroup2 i board mg v = let
  board2 = board//[(i, Known v)]
  BoardKK board3 _ = reduceFromKnownSquare (BoardKK board2 []) i
  lastIndex :: Index
  lastSquare :: Square
  (lastIndex, lastSquare) = reduceGroupWithIndexValue2 i v mg
  in case (board3!lastIndex, lastSquare) of
    (Possibilities s1, Possibilities s2) -> Map.singleton lastIndex (Set.intersection s1 s2)
    _                                    -> error "Why do we have a Known here"

possibilitiesForMathGroup :: Board -> MathGroup -> Map Index (Set Possibility)
possibilitiesForMathGroup board mg = let
  MathGroup op total groupIndices = mg
  i = Set.findMin groupIndices
  guesses :: [Possibility]
  guesses = case board!i of
    Possibilities s -> Set.toList s
    Known k         -> error "we should not have a Known here"
  applyFunc
    | Set.size groupIndices > 2 = applyGuessWithMathGroup3
    | Set.size groupIndices == 2 = applyGuessWithMathGroup2
    | otherwise = error "too few indices for possibilitiesForMathGroup"
  guessOutcomes :: [(Possibility, Map Index (Set Possibility))]
  guessOutcomes = map (\v -> (v, applyFunc i board mg v)) guesses
  -- we need to know which guessOutcomes had no possibilities for the last square
  legitOutcome :: (Possibility, Map Index (Set Possibility)) -> Bool
  legitOutcome (_, m) = hasPossibilities m
  legitOutcomes = filter legitOutcome guessOutcomes
  legitMaps = map snd legitOutcomes
  legitGuesses = Set.fromList $ map fst legitOutcomes
  unionMap = Map.unionsWith Set.union legitMaps
  guessMap = Map.insert i legitGuesses unionMap
  givenMap = Map.singleton i (Set.singleton total)
  isGiven = op == '1'
  in if isGiven then givenMap else guessMap

hasPossibilities :: (Ord a, Ord b) => Map a (Set b) -> Bool
hasPossibilities m = case Map.elems m of
  [] -> False
  sets -> all (not . Set.null) sets
  
-- narrowDownMathGroup :: Board -> MathGroup -> [(Index, Square)]
-- narrowDownMathGroup b mg = let
-- let's assume all indices in the group are still unknown
-- but maybe that's wrong?
-- I think the logic is
-- figure out all the guesses for the first square
-- if there's only one square left, return the possibilities for the last one,
-- make boards for them, including reducing the neighbors and updating the math group
-- 
-- 

data MathGroup = MathGroup { getOperation :: Char
                           , getTotal :: Int
                           , getIndices :: Set Index } deriving (Eq, Ord, Show)
mathGroupFromList :: Char -> Int -> [Index] -> MathGroup
mathGroupFromList c t is = MathGroup c t (Set.fromList is)

-- this is only for groups of size 3 or bigger which means no
-- subtraction or division
reduceGroupWithIndexValue ::  Index -> Possibility -> MathGroup -> Maybe MathGroup
reduceGroupWithIndexValue i value (MathGroup op total s) = let
  newSet = if Set.member i s then Set.delete i s else error "can't delete that"
  newTotal = case op of
    '+' -> Just (total - value)
    '*' -> if ((total `mod` value) == 0) then Just (total `div` value) else Nothing
    _   -> error ("I did not expect operation " ++ show op)
  in case newTotal of
    Nothing -> Nothing
    Just t  -> Just $ MathGroup op t newSet

reduceGroupWithIndexValueKnown ::  Index -> Possibility -> MathGroup -> MathGroup
reduceGroupWithIndexValueKnown i value mg = case (reduceGroupWithIndexValue i value mg) of
  Nothing -> error "You fucked up reduceGroupWithIndexValueKnown"
  Just mg -> mg

reduceGroupWithIndexValue2 :: Index -> Possibility -> MathGroup -> (Index, Square)
reduceGroupWithIndexValue2 i value (MathGroup op total s) = let
  lastIndex :: Index
  lastIndex = if ((Set.size s /= 2) || (Set.notMember i s)) then error "you fucked up reduceGroupWithIndexValue2" else Set.findMin $ Set.delete i s
  divCandidates = [(total `mod` value == 0, total `div` value), (value `mod` total == 0, value `div` total), (True, value * total)]
  correctDivGuesses =  map snd $ filter fst divCandidates
  rawValues = case op of
      '+' -> [total - value]
      '-' -> [value - total, value + total]
      '*' -> if (total `mod` value == 0) then [total `div` value] else []
      '/' -> correctDivGuesses
      _   -> error ("I did not expect operation " ++ show op)
  seriousValues = filter (\x -> (x >= 0) && (x <=9)) rawValues
  in (lastIndex, Possibilities (Set.fromList seriousValues))

-- PUZZLE NO. 200234
testBoardKK1 = parseBoard "000000000000000000000000000000000000000000000000000000000000000000000000000000000"
testMGKK1 = [
    mathGroupFromList '*' 1260 [0,1,2,10]
--  , mathGroupFromList '+' 3    [3,4]
-- try specifying these squares as "givens" to test given functionality
  , mathGroupFromList '1' 2    [3]
  , mathGroupFromList '1' 1    [4]
  , mathGroupFromList '*' 12   [5,6]
  , mathGroupFromList '+' 14   [7,8]
  , mathGroupFromList '-' 8    [9,18]
  , mathGroupFromList '+' 3    [11,20]
  , mathGroupFromList '-' 3    [12, 21]
  , mathGroupFromList '-' 1    [13,22]
  , mathGroupFromList '+' 16   [14,23,24]
  , mathGroupFromList '*' 80   [15,16,25]
  , mathGroupFromList '-' 2    [17,26]
  , mathGroupFromList '*' 1344 [19, 27, 28, 29]
  , mathGroupFromList '+' 20   [30, 38, 39]
  , mathGroupFromList '+' 10   [31, 40, 49]
  , mathGroupFromList '-' 8    [32, 33]
  , mathGroupFromList '+' 5    [34, 35]
  , mathGroupFromList '/' 2    [36,37]
  , mathGroupFromList '+' 17   [41, 42, 50]
  , mathGroupFromList '*' 63   [43, 44]
  , mathGroupFromList '*' 15   [45, 46]
  , mathGroupFromList '+' 10   [47, 48]
  , mathGroupFromList '+' 18   [51, 52, 53, 61]
  , mathGroupFromList '+' 14   [54, 63]
  , mathGroupFromList '*' 18   [55, 64, 65]
  , mathGroupFromList '-' 2    [56, 57]
  , mathGroupFromList '+' 19   [58, 66, 67]
  , mathGroupFromList '-' 4    [59, 68]
  , mathGroupFromList '+' 9    [60, 69]
  , mathGroupFromList '+' 5    [62, 71]
  , mathGroupFromList '+' 24   [70, 78, 79, 80]
  , mathGroupFromList '-' 3    [72, 73]
  , mathGroupFromList '+' 7    [74, 75]
  , mathGroupFromList '*' 14   [76, 77]
  ]
testKK = BoardKK testBoardKK1 testMGKK1

-- (testBoardKK2,testMGKK2) =  applyGuessWithMathGroup3 0 testBoardKK1 (head testMGKK1) 3
BoardKK lastBoard lastGroups = tryToSolve testKK
firstGuess = head $ makeGuessBoards $ BoardKK lastBoard lastGroups
betterGuess = initialReduceFromKnown firstGuess

emptyBoard = parseBoard (take 81 $ repeat '0')
boardFromDecoder :: [(Char, Int, Set Index)] -> BoardKK
boardFromDecoder gs = let
  tripleToMG (op, total, indices) = MathGroup op total indices
  mgs = map tripleToMG gs
  in BoardKK emptyBoard mgs

beforeBug = initialReduceFromKnown  $ boardFromDecoder KenKenDecoder.puzzle37665Export
