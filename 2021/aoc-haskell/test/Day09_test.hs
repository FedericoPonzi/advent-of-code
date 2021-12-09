{-# LANGUAGE ScopedTypeVariables #-}

module Day09_test where

import Control.Monad (join)
import Data.Graph.UGraph
import Data.List.Split
import qualified Data.Map as DataMap
import Data.Maybe (fromMaybe)
import Day09
import System.IO
import Test.HUnit
import Data.Graph.Types

simpleInput =
  "2199943210\n\
  \3987894921\n\
  \9856789892\n\
  \8767896789\n\
  \9899965678"

safeGet :: Int -> [a] -> Maybe a
safeGet n arr = do
  let len = length arr
  if n < len && n >= 0 then Just (arr !! n) else Nothing

safeGetPos :: Int -> Int -> [[Int]] -> Maybe Int
safeGetPos row col m = do
  safeGet col =<< safeGet row m

innerLength l = length (head l)

adjancetBuilder :: Int -> Int -> [[Int]] -> Int
adjancetBuilder r c m = do
  let depth = fromMaybe 9 (safeGetPos r c m)
  let index = r * innerLength m + c
  depth

north row = adjancetBuilder (row -1)

south row = adjancetBuilder (row + 1)

east row col = adjancetBuilder row (col - 1)

weast row col = adjancetBuilder row (col + 1)


parseInput :: String -> [(Int, [Int])]
parseInput contents = do
  let asLines :: [[Int]] = map (map read . chunksOf 1) (lines contents)
  let lines_length = length (head asLines)
  let zipped = zip [0 ..] $ map (zip [0 ..]) asLines
  let getEdges row col = do [north row col asLines, south row col asLines, east row col asLines, weast row col asLines]
  let edgeBuilder edges row col depth = do
        let ret =  [(depth, [edges !! 0, edges !! 1, edges !! 2, edges !! 3])]
        ret

  concatMap (\(indexRow, rowList) -> concatMap (\(indexCol, depthVal) -> edgeBuilder (getEdges indexRow indexCol) indexRow indexCol depthVal ) rowList) zipped

testSimple :: Test
testSimple =
  TestCase
    ( do
        let pos = parseInput simpleInput
        print pos
        assertEqual "day 08 simple" 26 (day09 pos)
    )

test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day09.txt" ReadMode
        contents :: String <- hGetContents handle
        let pos = parseInput contents
        assertEqual
          "test 2"
          973499
          (day09 pos)
        hClose handle
    )
tests :: [Test]
tests = [testSimple,test1]
