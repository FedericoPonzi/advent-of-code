{-# LANGUAGE ScopedTypeVariables #-}

module Day10_test where

import Data.List.Split
import Day10
import System.IO
import Test.HUnit

simpleInput =
  "[({(<(())[]>[[{[]{<()<>>\n\
  \[(()[<>])]({[<{<<[]>>(\n\
  \{([(<{}[<>[]}>{[]{[(<()>\n\
  \(((({<>}<{<{<>}{[]{[]{}\n\
  \[[<[([]))<([[{}[[()]]]\n\
  \[{[{({}]{}}([{[{{{}}([]\n\
  \{<[[]]>}<{[{[{[]{()[[[]\n\
  \[<(<(<(<{}))><([]([]()\n\
  \<{([([[(<>()){}]>(<<{{\n\
  \<{([{{}}[<[[[<>{}]]]>[]]"

si = "<{([{{}}[<[[[<>{}]]]>[]]"

parseInput :: String -> [[Chunks]]
parseInput contents = do
  let asLines = lines contents
  map (map chunksFromChar . chunksOf 1) asLines

testSimple :: Test
testSimple =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 10 simple" 26397 (day10 pos)
    )

test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day10.txt" ReadMode
        contents :: String <- hGetContents handle
        let pos = parseInput contents
        assertEqual
          "test 1"
          278475
          (day10 pos)
        hClose handle
    )

test2Simple :: Test
test2Simple =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 10 simple" 0 (day10p2 pos)
    )

test2 :: Test
test2 =
  TestCase
    ( do
        handle <- openFile "inputs/day10.txt" ReadMode
        contents :: String <- hGetContents handle
        let pos = parseInput contents
        assertEqual
          "test 1"
          278475
          (day10p2 pos)
        hClose handle
    )

tests :: [Test]
tests = [testSimple, test1, test2]
