{-# LANGUAGE ScopedTypeVariables #-}

module Day08_test (tests) where

import Data.List.Split
import qualified Data.Map
import Day08
import System.IO
import Test.HUnit

simpleInput :: [Char]
simpleInput = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
               \edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
               \fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
               \fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
               \aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
               \fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
               \dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
               \bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
               \egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
               \gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

splitAtFirst :: String -> [String] -> ([String], [String])
splitAtFirst x = fmap (drop 1) . break (x ==)

-- split input list by newline, by pipe and by space
parseInput :: String -> [([String], [String])]
parseInput contents = do
  let inputLines :: [String] = splitOn "\n" contents
  let splitSpace = map (splitOn " ")
  let sections :: [[String]] = splitSpace inputLines
  map (splitAtFirst "|") sections

testSimple :: Test
testSimple =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 08 simple" 26 (day08 pos)
    )

test1 :: Test
test1 =
  TestCase
    ( do
        handle <- openFile "inputs/day08.txt" ReadMode
        contents :: String <- hGetContents handle
        let positions = parseInput contents
        assertEqual
          "test 1"
          284
          (day08 positions)
        hClose handle
    )

test2Simple :: Test
test2Simple =
  TestCase
    ( do
        let pos = parseInput "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        print pos
        assertEqual "day 08 simple" 5353 (day08p2 pos)
    )

test2Simple2 :: Test
test2Simple2 =
  TestCase
    ( do
        let pos = parseInput simpleInput
        assertEqual "day 08 simple" 61229 (day08p2 pos)
    )
test2 :: Test
test2 =
  TestCase
    ( do
        handle <- openFile "inputs/day08.txt" ReadMode
        contents :: String <- hGetContents handle
        let positions = parseInput contents
        assertEqual
          "test 2"
          973499
          (day08p2 positions)
        hClose handle
    )
tests :: [Test]
tests = [testSimple, test1, test2Simple, test2Simple2, test2]
