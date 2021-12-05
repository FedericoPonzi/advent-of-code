{-# LANGUAGE ScopedTypeVariables #-}

module Day04_test (tests) where
import Data.Char (isSpace)
import Data.List.Split
import Day04
import System.IO
import Test.HUnit

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

simpleInput =
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
  \\n\
  \22 13 17 11  0\n\
  \8  2 23  4 24\n\
  \21  9 14 16  7\n\
  \6 10  3 18  5\n\
  \1 12 20 15 19\n\
  \\n\
  \3 15  0  2 22\n\
  \9 18 13 17  5\n\
  \19  8  7 25 23\n\
  \20 11 10 24  4\n\
  \14 21 16 12  6\n\
  \\n\
  \14 21 17 24  4\n\
  \10 16 15  9 19\n\
  \18  8 23 26 20\n\
  \22 11 13  6  5\n\
  \2  0 12  3  7"

parseBoard :: [[Char]] -> [[[Int]]]
parseBoard b = do
  let rows :: [[String]] = map (splitOn "\n") b
  let rowsSplittedBySpace :: [[[String]]] = map (map (split (dropBlanks . condense . dropDelims $ oneOf " "))) rows
  let simpleBoardsInt :: [[[Int]]] = map (map (map read)) rowsSplittedBySpace
  let simpleBoardsWCols = map (\singleBoard -> do
                                    let cols = map (singleCol singleBoard)  [0..4]
                                    singleBoard ++ cols ) simpleBoardsInt
  simpleBoardsWCols

parseInput :: String -> ([Int], [BingoTable])
parseInput content = do
    let (drawnNumbersStr : boardRaw) = splitOn "\n\n" content
    let numbers = map read (splitOn "," drawnNumbersStr)
    (numbers, parseBoard boardRaw)

singleCol :: Foldable t => t [a] -> Int -> [a]
singleCol boardsInt pos = do
    foldl (\ acc l ->  acc ++ [l !! pos] ) [] boardsInt

testSimple :: Test
testSimple =
  TestCase
    ( do
        let (simpleNumbers, simpleBoard) = parseInput simpleInput
        assertEqual "day 04 part 1" 4512 (day04 simpleNumbers simpleBoard)
    )


test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day04.txt" ReadMode
        contents :: String <- hGetContents handle
        let (numbers, board) = parseInput contents
        assertEqual
          "test 1"
          22680
          (day04 numbers board)
        hClose handle
    )



test2Simple :: Test
test2Simple =
  TestCase
    ( do
        let (simpleNumbers, simpleBoard) = parseInput simpleInput
        assertEqual "day 04 part 2" 1924 (day04p2 simpleNumbers simpleBoard)
    )
test2 :: Test
test2 =
  TestCase
    ( do
        handle <- openFile "inputs/day04.txt" ReadMode
        contents :: String <- hGetContents handle
        let (numbers, board) = parseInput contents
        assertEqual
          "test 2"
          16168
          (day04p2 numbers board)
        hClose handle
    )


tests :: [Test]
tests = [testSimple, test1, test2Simple, test2]
