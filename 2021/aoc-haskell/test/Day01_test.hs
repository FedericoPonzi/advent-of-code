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

testDayOne :: Test
testDayOne = TestCase (do  handle <- openFile "inputs/day01.txt" ReadMode
                           contents <- hGetContents handle
                           let singlewords = words contents
                               intList = convert singlewords
                           assertEqual "day one part one" 1527 ( day01 intList )
                           hClose handle)
test_p1 :: [Test]
test_p1 = [testSimple, testEmpty, testEmpty2, testDayOne]


testPtwoSimple :: Test
testPtwoSimple = TestCase $ assertEqual
  "simple test" 1 ( day01p2 [1, 2, 3, 4] )

testPtwoEmpty :: Test
testPtwoEmpty = TestCase $ assertEqual
  "simple test" 0 ( day01p2 [4, 3, 2, 1] )

testPtwoEmpty2 :: Test
testPtwoEmpty2 = TestCase $ assertEqual
  "simple test" 0 ( day01p2 [] )


testPtwo :: Test
testPtwo =  TestCase (do  handle <- openFile "inputs/day01.txt" ReadMode
                          contents <- hGetContents handle
                          let singlewords = words contents
                              intList = convert singlewords
                          assertEqual "day one part two" 1575 ( day01p2 intList )
                          hClose handle)

test_p2 :: [Test]
test_p2 = [ testPtwoSimple, testPtwoEmpty, testPtwoEmpty2, testPtwo]

tests :: [Test]
tests = test_p1 ++ test_p2



