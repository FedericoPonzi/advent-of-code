module Day01_test(tests) where
import Day01
import Test.HUnit
import System.IO

convert :: [String] -> [Int]
convert = map read

testSimple :: Test
testSimple = TestCase $ assertEqual
  "simple test" 1 ( day01 [1,2] )

testEmpty :: Test
testEmpty = TestCase $ assertEqual
  "simple test" 0 ( day01 [] )

testEmpty2 :: Test
testEmpty2 = TestCase $ assertEqual
  "simple test" 0 ( day01 [2,1] )

testDayOne = TestCase (do  handle <- openFile "inputs/day01.txt" ReadMode
                           contents <- hGetContents handle
                           let singlewords = words contents
                               intList = convert singlewords    
                           assertEqual "simpletest" 1527 ( day01 intList )
                           hClose handle)


tests :: [Test]
tests = [testSimple, testEmpty, testEmpty2, testDayOne]

