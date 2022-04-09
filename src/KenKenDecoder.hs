{-# LANGUAGE OverloadedStrings #-}

module KenKenDecoder where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.Set as Set
import Data.Set (Set)

puzzle200234 = "eyJpZCI6MjAwMjM0LCJkYXRhIjoiQVxyXG43IDkgNSAyIDEgMyA0IDYgOCBcclxuMSA0IDIgNyA5IDYgOCA1IDMgXHJcbjkgNiAxIDQgOCA3IDMgMiA1IFxyXG40IDcgOCA2IDUgMSA5IDMgMiBcclxuMiAxIDYgOCAzIDQgNSA3IDkgXHJcbjMgNSA5IDEgMiA4IDYgNCA3IFxyXG44IDMgNyA1IDYgOSAyIDEgNCBcclxuNiAyIDMgOSA0IDUgNyA4IDEgXHJcbjUgOCA0IDMgNyAyIDEgOSA2IFxyXG5UXHJcbjEyNjAgICAwICAgMCAgIDMgICAwICAxMiAgIDAgIDE0ICAgMFxyXG4gIDggICAwICAgMyAgIDMgICAxICAxNiAgODAgICAwICAgMlxyXG4gIDAgMTM0NCAgIDAgICAwICAgMCAgIDAgICAwICAgMCAgIDBcclxuICAwICAgMCAgIDAgIDIwICAxMCAgIDggICAwICAgNSAgIDBcclxuICAyICAgMCAgIDAgICAwICAgMCAgMTcgICAwICA2MyAgIDBcclxuIDE1ICAgMCAgMTAgICAwICAgMCAgIDAgIDE4ICAgMCAgIDBcclxuIDE0ICAxOCAgIDIgICAwICAxOSAgIDQgICA5ICAgMCAgIDVcclxuICAwICAgMCAgIDAgICAwICAgMCAgIDAgICAwICAyNCAgIDBcclxuICAzICAgMCAgIDcgICAwICAxNCAgIDAgICAwICAgMCAgIDBcclxuU1xyXG4qIDAgMCArIDAgKiAwICsgMFxyXG4tIDAgKyAtIC0gKyAqIDAgLVxyXG4wICogMCAwIDAgMCAwIDAgMFxyXG4wIDAgMCArICsgLSAwICsgMFxyXG4vIDAgMCAwIDAgKyAwICogMFxyXG4qIDAgKyAwIDAgMCArIDAgMFxyXG4rICogLSAwICsgLSArIDAgK1xyXG4wIDAgMCAwIDAgMCAwICsgMFxyXG4tIDAgKyAwICogMCAwIDAgMFxyXG5WXHJcbjAgMCAxIDAgMSAwIDEgMFxyXG4xIDEgMSAxIDEgMSAwIDFcclxuMSAxIDEgMSAxIDAgMSAxXHJcbjAgMCAxIDEgMSAwIDEgMFxyXG4wIDEgMCAxIDEgMCAxIDBcclxuMCAxIDAgMSAxIDEgMCAwXHJcbjEgMSAwIDEgMSAxIDEgMVxyXG4xIDAgMSAwIDEgMSAxIDFcclxuMCAxIDAgMSAwIDEgMCAwXHJcbkhcclxuMSAwIDEgMSAxIDEgMCAxXHJcbjAgMSAwIDEgMSAxIDAgMVxyXG4xIDAgMSAxIDEgMSAxIDFcclxuMSAwIDEgMCAxIDEgMSAxXHJcbjEgMCAxIDAgMCAxIDAgMVxyXG4xIDAgMSAxIDAgMSAwIDFcclxuMSAxIDEgMSAxIDEgMCAxXHJcbjEgMCAxIDEgMSAwIDEgMFxyXG4xIDAgMSAxIDEgMSAwIDFcclxuXHJcbiIsInNpemUiOjksImxldmVsIjoibWVkaXVtIiwib3BlcmF0aW9ucyI6ImFkbXMiLCJubyI6MjAwMjM0LCJndWVzdCI6ZmFsc2UsImRhaWx5Ijp0cnVlLCJzdGF0ZSI6bnVsbCwidV9sZXZlbCI6IkIifQ=="
puzzle37665 = "eyJpZCI6Mzc2NjUsImRhdGEiOiJBXHJcbjQgNyAxIDUgMyA2IDIgOSA4IFxyXG42IDEgMiA5IDcgNCA4IDMgNSBcclxuMiAzIDcgNCA2IDUgOSA4IDEgXHJcbjcgNSAzIDIgNCA4IDEgNiA5IFxyXG44IDQgNSAxIDIgOSAzIDcgNiBcclxuNSA2IDkgNyA4IDMgNCAxIDIgXHJcbjkgMiA0IDggMSA3IDYgNSAzIFxyXG4zIDkgOCA2IDUgMSA3IDIgNCBcclxuMSA4IDYgMyA5IDIgNSA0IDcgXHJcblRcclxuMTY4ICAgMCAgIDYgICAwICAgMiAgIDAgICA0ICAgMyAgNDBcclxuICAwICAgOCAgIDAgIDE2ICAgMCAgMTggICAwICAgMCAgIDBcclxuICAwICAgMCAgIDMgICAwICAxMiAgIDAgICAwICA0OCAgIDhcclxuIDM1ICAgMCAgIDYgICAwICAgMCAgIDEgICAwICAgMCAgIDBcclxuICAyICAgMCAzMTUgICAwICAgMCAgIDAgICAzICAxMyAgIDBcclxuICAxICAgMCAgIDAgICAwICAxNiAgIDcgICAwICAgOCAgIDBcclxuIDcyICAgMCAgIDAgICAyICAgMCAgIDAgMjEwICAgMCAgMjRcclxuICAzIDM0NTYgICAwICAgMCAgMTkgICAwICAgMCAgIDAgICAwXHJcbiAgMCAgIDAgICAwICAgMCAgIDAgICAwICAgMCAgMjggICAwXHJcblNcclxuKiAwICsgMCAvIDAgLyAvICpcclxuMCArIDAgKyAwICsgMCAwIDBcclxuMCAwIC0gMCArIDAgMCAqIC1cclxuKiAwICogMCAwIC0gMCAwIDBcclxuLyAwICogMCAwIDAgMSArIDBcclxuLSAwIDAgMCArICsgMCArIDBcclxuKiAwIDAgLSAwIDAgKiAwICpcclxuLyAqIDAgMCArIDAgMCAwIDBcclxuMCAwIDAgMCAwIDAgMCAqIDBcclxuVlxyXG4wIDEgMCAxIDAgMSAxIDFcclxuMSAwIDEgMCAxIDEgMSAxXHJcbjAgMSAwIDEgMSAwIDEgMVxyXG4wIDEgMCAxIDEgMSAwIDFcclxuMCAxIDEgMSAxIDEgMSAwXHJcbjAgMSAwIDEgMSAwIDEgMFxyXG4wIDAgMSAxIDAgMSAxIDFcclxuMSAwIDEgMSAxIDAgMSAwXHJcbjEgMCAxIDAgMCAxIDEgMFxyXG5IXHJcbjAgMSAxIDEgMSAxIDEgMFxyXG4xIDAgMSAxIDEgMSAxIDBcclxuMSAxIDEgMSAwIDEgMSAwXHJcbjEgMSAxIDAgMSAxIDAgMVxyXG4xIDEgMCAwIDEgMCAxIDBcclxuMSAwIDEgMCAxIDEgMSAxXHJcbjAgMSAxIDEgMSAxIDAgMFxyXG4wIDEgMCAxIDEgMCAxIDFcclxuMCAxIDAgMSAxIDEgMCAxXHJcblxyXG4iLCJzaXplIjo5LCJsZXZlbCI6ImhhcmQiLCJvcGVyYXRpb25zIjoiYWRtcyIsIm5vIjozNzY2NSwiZ3Vlc3QiOmZhbHNlLCJkYWlseSI6ZmFsc2UsInN0YXRlIjpudWxsLCJ1X2xldmVsIjoiQiJ9"

data ThingWithData = ThingWithData
  { cannotCallItData :: String }
  deriving (Show)

instance Aeson.FromJSON ThingWithData where
  parseJSON = Aeson.withObject "ThingWithData" $ \obj -> do
    extractedData <- obj .: "data"
    return (ThingWithData { cannotCallItData = extractedData })

-- I can't put a type signature on this because lazy strings or something
decodeData encoded = let
  jsonStr = case Base64.decode encoded of
    Left _ -> error "Base64.decode came back Left oh god no"
    Right s -> s
  thingFromJSON = case Aeson.decode jsonStr of
    Nothing -> error "Aeson.decode came back Nothing"
    Just t -> t
  in cannotCallItData thingFromJSON

linesBetween :: String -> String -> [String] -> [String]
linesBetween start end allLines = let
  fromStart = tail $ dropWhile (/= start) $ allLines
  in takeWhile (/= end) fromStart

linesWithTotals:: [String] -> [String]
linesWithTotals = linesBetween "T\r" "S\r"

linesWithOperations :: [String] -> [String]
linesWithOperations = linesBetween "S\r" "V\r"

linesWithVBorders :: [String] -> [String]
linesWithVBorders = linesBetween "V\r" "H\r"

linesWithHBorders :: [String] -> [String]
linesWithHBorders = linesBetween "H\r" "\r"

type Index = Int
openNeighbors :: Int -> Set Index -> Set Index -> Index -> [Index]
openNeighbors boardSize hBorders vBorders currentIndex = let
  atLeftEdge = currentIndex `mod` boardSize == 0
  atRightEdge = (currentIndex + 1) `mod` boardSize == 0
  atTopEdge = currentIndex `div` boardSize == 0
  atBottomEdge = currentIndex `div` boardSize == boardSize - 1
  leftNeighbor = currentIndex - 1
  rightNeighbor = currentIndex + 1
  downNeighbor = currentIndex + boardSize
  upNeighbor = currentIndex - boardSize
  canGoLeft = not (atLeftEdge || Set.member leftNeighbor vBorders)
  canGoRight = not (atRightEdge || Set.member currentIndex vBorders)
  canGoDown = not (atBottomEdge || Set.member currentIndex hBorders)
  canGoUp = not (atTopEdge || Set.member upNeighbor hBorders)
  in map snd $ filter fst [(canGoLeft, leftNeighbor), (canGoRight, rightNeighbor), (canGoUp, upNeighbor), (canGoDown, downNeighbor)]

exploreCage0 :: Int -> Set Index -> Set Index -> Set Index -> Set Index -> Set Index
exploreCage0 boardSize hBorders vBorders toExplore visited = let
  currentIndex = Set.findMin toExplore
  neighbors = openNeighbors boardSize hBorders vBorders currentIndex
  unvisitedNeighbors = filter (\n -> Set.notMember n visited) neighbors
  newVisited = Set.insert currentIndex visited
  newToExplore = Set.union (Set.delete currentIndex toExplore) (Set.fromList unvisitedNeighbors)
  in if (Set.null newToExplore) then newVisited else exploreCage0 boardSize hBorders vBorders newToExplore newVisited

exploreCage :: Int -> Set Index -> Set Index -> Index -> Set Index
exploreCage boardSize hBorders vBorders currentIndex =
  exploreCage0 boardSize hBorders vBorders (Set.singleton currentIndex) (Set.empty)

parseHBorders :: [String] -> Set Index
parseHBorders lineStrs = let
  wordsByLine = map words lineStrs
  withRowIDs :: [[(Int, String)]]
  withRowIDs = map (zip [0..]) wordsByLine
  withColumnIDs :: [(Int, [(Int, String)])]
  withColumnIDs = zip [0..] withRowIDs
  rowWidth = length wordsByLine
  coordsToIndex r c = (rowWidth * r) + c
  innerFunc c (r, str) = (coordsToIndex r c, str)
  outerFunc :: (Int, [(Int, String)]) -> [(Int, String)]
  outerFunc (c, ls) = map (innerFunc c) ls
  withIndexIDs :: [(Index, String)]
  withIndexIDs = concat $ map outerFunc withColumnIDs
  onlyTheOnes = filter (\(i, str) -> str == "1") withIndexIDs
  in Set.fromList $ map fst onlyTheOnes

parseVBorders :: [String] -> Set Index
parseVBorders lineStrs = let
  wordsByLine = map words lineStrs
  withColumnIDs :: [[(Int, String)]]
  withColumnIDs = map (zip [0..]) wordsByLine
  withRowIDs :: [(Int, [(Int, String)])]
  withRowIDs = zip [0..] withColumnIDs
  columnHeight = length wordsByLine
  coordsToIndex r c = (columnHeight * r) + c
  innerFunc r (c, str) = (coordsToIndex r c, str)
  outerFunc (r, ls) = map (innerFunc r) ls
  withIndexIDs = concat $ map outerFunc withRowIDs
  onlyTheOnes = filter (\(i, str) -> str == "1") withIndexIDs
  in Set.fromList $ map fst onlyTheOnes

parseAll :: String -> [(Char, Int, Set Index)]
parseAll textBlob = let
  asLines = lines textBlob
  vBorders = parseVBorders $ linesWithVBorders asLines
  hBorders = parseHBorders $ linesWithHBorders asLines
  ops = concat $ map words $ linesWithOperations asLines
  totals = map (\s -> read s :: Int) $ concat $ map words $ linesWithTotals asLines
  indexOpTotalAll = zip3 [0..] ops totals
  indexOpTotal :: [(Index, String, Int)]
  indexOpTotal = filter (\(_,o,_) -> o /= "0") indexOpTotalAll
  boardSize = length $ linesWithVBorders asLines
  messyPairs :: [((Index, String, Int), Set Index)]
  messyPairs = zip indexOpTotal (map (\(i,_,_) -> exploreCage boardSize hBorders vBorders i) indexOpTotal)
  fixMessyPair :: ((Index, String, Int), Set Index) -> (Char, Int, Set Index)
  fixMessyPair ((_, opStr, total), indices) = (head opStr, total, indices)
  in map fixMessyPair messyPairs

-- parseEncoded :: String -> [(Char, Int, Set Index)]
parseEncoded encoded = parseAll $ decodeData encoded

puzzle200234Export = parseEncoded puzzle200234
puzzle37665Export = parseEncoded puzzle37665

