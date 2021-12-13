{-# LANGUAGE ScopedTypeVariables #-}

module Day12_test where

import Data.List.Split
import qualified Data.Map as DataMap
import Day12
import System.IO
import Test.HUnit

superSimple =
  "start-A\n\
  \start-b\n\
  \A-c\n\
  \A-b\n\
  \b-d\n\
  \A-end\n\
  \b-end"

simpleInput =
  "dc-end\n\
  \HN-start\n\
  \start-kj\n\
  \dc-start\n\
  \dc-HN\n\
  \LN-dc\n\
  \HN-end\n\
  \kj-sa\n\
  \kj-HN\n\
  \kj-dc"

parseInput :: String -> DataMap.Map CaveType [CaveType]
parseInput contents = do
  let asLines = lines contents
  let foldFn = DataMap.unionWith (++)
  let listOfMaps =
        concatMap
            ( \a -> do
                let parts = splitOn "-" a
                let first = fromStringToCave (head parts)
                let second = fromStringToCave (parts !! 1)
                [DataMap.fromList [(first, [second]), (second, [first])]]
            )
            asLines
  foldl foldFn DataMap.empty listOfMaps

testSimple :: Test
testSimple =
  TestCase
    ( do
        let pos = parseInput superSimple
        assertEqual "day 10 simple" 10 (day12 pos)
    )

testSimple2 :: Test
testSimple2 =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 10 simple" 19 (day12 pos)
    )

simpleLarger :: [Char]
simpleLarger = "fs-end\n\
               \he-DX\n\
               \fs-he\n\
               \start-DX\n\
               \pj-DX\n\
               \end-zg\n\
               \zg-sl\n\
               \zg-pj\n\
               \pj-he\n\
               \RW-he\n\
               \fs-DX\n\
               \pj-RW\n\
               \zg-RW\n\
               \start-pj\n\
               \he-WI\n\
               \zg-he\n\
               \pj-fs\n\
               \start-RW"
testSimple3 :: Test
testSimple3 =
  TestCase
    ( do
        let pos = parseInput simpleLarger
        assertEqual "day 10 simple" 226 (day12 pos)
    )

test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day12.txt" ReadMode
        contents :: String <- hGetContents handle
        let pos = parseInput contents
        assertEqual
          "test 1"
          5076
          (day12 pos)
        hClose handle
    )

test2Simple1 :: Test
test2Simple1 =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 12 simple smaller" 103 (day12p2 pos)
    )

test2Simple :: Test
test2Simple =
  TestCase
    ( do
        let pos = parseInput superSimple
        assertEqual "day 12 simple smaller" 36 (day12p2 pos)
    )

test2Simple2 :: Test
test2Simple2 =
  TestCase
    ( do
        let pos = parseInput simpleLarger
        assertEqual "day 12 simple larger" 3509 (day12p2 pos)
    )


test2 :: Test
test2 =
  TestCase
    ( do
        handle <- openFile "inputs/day12.txt" ReadMode
        contents :: String <- hGetContents handle
        let pos = parseInput contents
        assertEqual
          "test 2"
          145643
          (day12p2 pos)
        hClose handle
    )

tests :: [Test]
tests = [testSimple, testSimple2, testSimple3, test1, test2Simple, test2Simple1, test2Simple2, test2]
