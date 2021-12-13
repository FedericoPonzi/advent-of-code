{-# LANGUAGE ScopedTypeVariables #-}

module Day11_test where

import Data.List.Split
import Day11
import System.IO
import Test.HUnit

simpleInput :: String
simpleInput =
  "5483143223\n\
  \2745854711\n\
  \5264556173\n\
  \6141336146\n\
  \6357385478\n\
  \4167524645\n\
  \2176841721\n\
  \6882881134\n\
  \4846848554\n\
  \5283751526"

superSimpleInput :: [Char]
superSimpleInput =
  "11111\n\
  \19991\n\
  \19191\n\
  \19991\n\
  \11111"

step1 :: [Char]
step1 =
  "6594254334\n\
  \3856965822\n\
  \6375667284\n\
  \7252447257\n\
  \7468496589\n\
  \5278635756\n\
  \3287952832\n\
  \7993992245\n\
  \5957959665\n\
  \6394862637"

step2 :: [Char]
step2 =
  "8807476555\n\
  \5089087054\n\
  \8597889608\n\
  \8485769600\n\
  \8700908800\n\
  \6600088989\n\
  \6800005943\n\
  \0000007456\n\
  \9000000876\n\
  \8700006848"

parseInput :: String -> [[Int]]
parseInput contents = do
  let asLines = lines contents
  map (map read . chunksOf 1) asLines

testSimple1 :: Test
testSimple1 =
  TestCase
    ( do
        let pos = parseInput superSimpleInput
        assertEqual "day 11 simple" [[4, 5, 6, 5, 4], [5, 1, 1, 1, 5], [6, 1, 1, 1, 6], [5, 1, 1, 1, 5], [4, 5, 6, 5, 4]] (snd (singleEpoch (snd $ singleEpoch pos)))
    )

testSimple :: Test
testSimple =
  TestCase
    ( do
        assertEqual "day 11 simple" (parseInput step2) (snd $ singleEpoch (parseInput step1))
    )

testSimple3 :: Test
testSimple3 =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 11 simple" 1656 (fst $ day11 pos 100)
    )

step20 :: [Char]
step20 =
  "3936556452\n\
  \5686556806\n\
  \4496555690\n\
  \4448655580\n\
  \4456865570\n\
  \5680086577\n\
  \7000009896\n\
  \0000000344\n\
  \6000000364\n\
  \4600009543"

testSimple4 :: Test
testSimple4 =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 11 simple" (parseInput step20) (snd $ day11 pos 20)
    )

step100 :: [Char]
step100 =
  "0397666866\n\
  \0749766918\n\
  \0053976933\n\
  \0004297822\n\
  \0004229892\n\
  \0053222877\n\
  \0532222966\n\
  \9322228966\n\
  \7922286866\n\
  \6789998766"

testSimple5 :: Test
testSimple5 =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 11 simple" (parseInput step100) (snd (day11 pos 100))
    )

test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day11.txt" ReadMode
        contents :: String <- hGetContents handle
        let pos = parseInput contents
        assertEqual
          "test 1"
          1632
          (fst $ day11 pos 100)
        hClose handle
    )

step195 :: [Char]
step195 =
  "0000000000\n\
  \0000000000\n\
  \0000000000\n\
  \0000000000\n\
  \0000000000\n\
  \0000000000\n\
  \0000000000\n\
  \0000000000\n\
  \0000000000\n\
  \0000000000"

test2Simple1 :: Test
test2Simple1 =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 11 simple" (parseInput step195) (snd (day11 pos 195))
    )

test2Simple2 :: Test
test2Simple2 =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 11 simple" 195 (day11p2 pos 0)
    )
test2 :: Test
test2 =
  TestCase
    ( do
        handle <- openFile "inputs/day11.txt" ReadMode
        contents :: String <- hGetContents handle
        let pos = parseInput contents
        assertEqual
          "test 2"
          303
          (day11p2 pos 0)
        hClose handle
    )
tests :: [Test]
tests = [testSimple, testSimple1, testSimple3, testSimple4, testSimple5, test1, test2Simple1, test2Simple2, test2]
