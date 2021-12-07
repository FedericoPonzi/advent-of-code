{-# LANGUAGE ScopedTypeVariables #-}

module Day07_test (tests) where

import Data.List.Split
import qualified Data.Map
import Day07
import System.IO
import Test.HUnit

simpleInput :: [Char]
simpleInput = "16,1,2,0,4,2,7,1,2,14"

parseInput :: String -> [Int]
parseInput contents = do
  map read (splitOn "," contents)

testSimple :: Test
testSimple =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 07 simple" 37 (day07 pos)
    )

test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day07.txt" ReadMode
        contents :: String <- hGetContents handle
        let positions = parseInput contents
        assertEqual
          "test 1"
          352254
          (day07 positions)
        hClose handle
    )

test2Simple :: Test
test2Simple =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 07 simple" 168 (day07p2 pos)
    )

test2 :: Test
test2 =
  TestCase
    ( do
        handle <- openFile "inputs/day07.txt" ReadMode
        contents :: String <- hGetContents handle
        let positions = parseInput contents
        assertEqual
          "test 2"
          99053143
          (day07p2 positions)
        hClose handle
    )

tests :: [Test]
tests = [testSimple, test1, test2Simple, test2]
