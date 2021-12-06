{-# LANGUAGE ScopedTypeVariables #-}

module Day05_test (tests) where

import Data.Char (isSpace)
import Data.List.Split
import Day05 (Segment (..), day05, day05p2)
import System.IO
import Test.HUnit

simpleInput =
  "0,9 -> 5,9\n\
  \8,0 -> 0,8\n\
  \9,4 -> 3,4\n\
  \2,2 -> 2,1\n\
  \7,0 -> 7,4\n\
  \6,4 -> 2,0\n\
  \0,9 -> 2,9\n\
  \3,4 -> 1,4\n\
  \0,0 -> 8,8\n\
  \5,5 -> 8,2"

parseRow :: String -> Segment
parseRow r = do
  let (startPointRaw : endPointRaw) :: [String] = splitOneOf "->" r
  let (startX : startY) = splitOneOf "," startPointRaw
  let (endX : endY) = splitOneOf "," (concat endPointRaw)
  Segment {startPoint = (read startX, read (concat startY)), endPoint = (read endX, read (concat endY))}

parseInput :: String -> [Segment]
parseInput contents = do
  map parseRow (lines contents)

testSimple :: Test
testSimple =
  TestCase
    ( do
        let coordinates = parseInput simpleInput
        assertEqual "day 05 part 1" 5 (day05 coordinates)
    )

test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day05.txt" ReadMode
        contents :: String <- hGetContents handle
        let coordinates = parseInput contents
        assertEqual
          "test 1"
          7436
          (day05 coordinates)
        hClose handle
    )

test2Simple :: Test
test2Simple =
  TestCase
    ( do
        let coordinates = parseInput simpleInput
        assertEqual "day 05 part 2" 12 (day05p2 coordinates)
    )

test2 :: Test
test2 =
  TestCase
    ( do
        handle <- openFile "inputs/day05.txt" ReadMode
        contents :: String <- hGetContents handle
        let coordinates = parseInput contents
        assertEqual
          "test 2"
          7436
          (day05p2 coordinates)
        hClose handle
        
    )


tests :: [Test]
tests = [testSimple, test1, test2Simple, test2]
