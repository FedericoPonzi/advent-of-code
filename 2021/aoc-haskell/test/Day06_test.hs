{-# LANGUAGE ScopedTypeVariables #-}

module Day06_test (tests) where

import Data.List.Split
import Day06 (day06, day06p2)
import System.IO
import Test.HUnit

simpleInput :: [Char]
simpleInput = "3,4,3,1,2"

parseInput :: String -> [Int]
parseInput contents = do
  map read (splitOn "," contents)

testSimple :: Test
testSimple =
  TestCase
    ( do
        let fish = parseInput simpleInput
        assertEqual "day 06 simple" 26 (day06p2 fish 18)
    )

testSimple2 :: Test
testSimple2 =
  TestCase
    ( do
        let fish = parseInput simpleInput
        assertEqual "day 06 simple" 5934 (day06p2 fish 80)
    )

test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day06.txt" ReadMode
        contents :: String <- hGetContents handle
        let fish = parseInput contents
        assertEqual
          "test 1"
          388739
          (day06 fish 80)
        hClose handle
    )


test2Simple :: Test
test2Simple =
  TestCase
    ( do
        let fish = parseInput simpleInput
        assertEqual "day 06 simple 2" 26984457539 (day06p2 fish 256)
    )

test2 :: Test
test2 =
  TestCase
    ( do
        handle <- openFile "inputs/day06.txt" ReadMode
        contents :: String <- hGetContents handle
        let fish = parseInput contents
        assertEqual
          "test 1"
          1741362314973
          (day06p2 fish 256)
        hClose handle
    )
tests :: [Test]
tests = [testSimple, testSimple2, test1, test2Simple, test2]
