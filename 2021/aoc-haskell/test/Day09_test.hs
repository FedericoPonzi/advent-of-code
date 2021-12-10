{-# LANGUAGE ScopedTypeVariables #-}

module Day09_test where

import Control.Monad (join)
import Data.List.Split
import Data.Maybe (fromMaybe)
import Day09
import System.IO
import Test.HUnit


simpleInput =
  "2199943210\n\
  \3987894921\n\
  \9856789892\n\
  \8767896789\n\
  \9899965678"
safeGet :: Int -> [a] -> Maybe a
safeGet n arr =  if n < len && n >= 0 then Just (arr !! n) else Nothing where len = length arr

safeGetPos :: Int -> Int -> [[Int]] -> Maybe Int
safeGetPos row col m = safeGet col =<< safeGet row m

innerLength l = length (head l)

adjancetBuilder :: Int -> Int -> [[Int]] -> Int
adjancetBuilder r c m = do
  let depth = fromMaybe 9 (safeGetPos r c m)
  let index = r * innerLength m + c
  depth

north :: Int -> Int -> [[Int]] -> Int
north row = adjancetBuilder (row -1)

south :: Int -> Int -> [[Int]] -> Int
south row = adjancetBuilder (row + 1)

east :: Int -> Int -> [[Int]] -> Int
east row col = adjancetBuilder row (col - 1)

weast :: Int -> Int -> [[Int]] -> Int
weast row col = adjancetBuilder row (col + 1)


parseInput :: String -> [(Int, [Int])]
parseInput contents = do
  let asLines :: [[Int]] = map (map read . chunksOf 1) (lines contents)
  let zipped = zip [0 ..] $ map (zip [0 ..]) asLines
  let getEdges row col = do [north row col asLines, south row col asLines, east row col asLines, weast row col asLines]
  let edgeBuilder edges depth = do
        let ret =  [(depth, [edges !! 0, edges !! 1, edges !! 2, edges !! 3])]
        ret

  concatMap (\(indexRow, rowList) -> concatMap (\(indexCol, depthVal) -> edgeBuilder (getEdges indexRow indexCol) depthVal ) rowList) zipped

testSimple :: Test
testSimple =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 08 simple" 15 (day09 pos)
    )

test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day09.txt" ReadMode
        contents :: String <- hGetContents handle
        let pos = parseInput contents
        assertEqual
          "test 1"
          591
          (day09 pos)
        hClose handle
    )

parseInput2 :: String -> [[Int]]
parseInput2 contents = map (map read . chunksOf 1) (lines contents)

testSimple2 :: Test
testSimple2 =
  TestCase
    ( do
        let pos = parseInput2 simpleInput
        print pos
        assertEqual "day 08 simple2 " 1134 (day09p2 pos)
    )
test2 :: Test
test2 =
  TestCase
    ( do
        handle <- openFile "inputs/day09.txt" ReadMode
        contents :: String <- hGetContents handle
        let pos = parseInput2 contents
        assertEqual
          "test 2"
          1113424
          (day09p2 pos)
        hClose handle
    )

tests :: [Test]
tests = [test1, testSimple, testSimple2, test2]
