module Day03_test(tests) where
import Day03
import Test.HUnit
import System.IO
import Data.List.Split


testSimple :: Test
testSimple =  TestCase (do  handle <- openFile "inputs/example.txt" ReadMode
                            contents <- hGetContents handle
                            let sl =  splitWhen (=="\n") (chunksOf 1 contents)
                            assertEqual "day one part one" 4361 ( day03 sl )
                            hClose handle)
testGetNumberFromPos :: Test
testGetNumberFromPos = TestCase
                    (do
                      assertEqual "simple" 467 (getNumberFromPos ["4","6","7",".",".","1","1","4",".","."] 0)
                      assertEqual "simple" 467 (getNumberFromPos ["4","6","7",".",".","1","1","4",".","."] 1)
                      assertEqual "simple" 467 (getNumberFromPos ["4","6","7",".",".","1","1","4",".","."] 2)
                    )
testP1 :: Test
testP1 =  TestCase (do  handle <- openFile "inputs/input.txt" ReadMode
                        contents <- hGetContents handle
                        let sl =  splitWhen (=="\n") (chunksOf 1 contents)
                        assertEqual "day one part one" 517021 ( day03 sl )
                        hClose handle)

test_p1 :: [Test]
test_p1 = [testSimple, testGetNumberFromPos, testP1]

tests :: [Test]
tests = test_p1

