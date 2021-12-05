module Day03_test (tests) where

import Data.List.Split
import Day03 (day03, day03p2)
import System.IO
import Test.HUnit

simpleInput :: [[Char]]
simpleInput =
  [ "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ]

single :: String -> [Int]
single l = map read (chunksOf 1 l)

convert :: [String] -> [[Int]]
convert = map single

testSimple :: Test
testSimple =
  TestCase
    ( do
        assertEqual
          "simple test"
          198
          (day03 (convert simpleInput))
    )

test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day03.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            input1 = convert singlewords
        assertEqual
          "test 1"
          3148794
          (day03 input1)
    )

test2Simple :: Test
test2Simple =
  TestCase
    ( do
        assertEqual
          "simple test"
          230
          (day03p2 (convert simpleInput))
    )

test2 :: Test
test2 =
  TestCase
    ( do
        handle <- openFile "inputs/day03.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            input1 = convert singlewords
        assertEqual
          "test 1"
          2795310
          (day03p2 input1)
    )

tests :: [Test]
tests = [testSimple, test1, test2Simple, test2]
