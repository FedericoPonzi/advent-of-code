module Main where

import Day01_test
import Day02_test
import Day03_test
import Day04_test
import Day05_test
import Day06_test

import Test.HUnit


allTests :: [Test]
allTests =  Day06_test.tests -- ++ Day05_test.tests ++ Day04_test.tests ++ Day03_test.tests  ++ Day01_test.tests ++ Day02_test.tests

main :: IO ()
main = do
    _ <- runTestTT $ TestList allTests
    putStrLn "Testing is completed."