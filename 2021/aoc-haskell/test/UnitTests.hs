module Main where

import Day01_test
import Day02_test
import Test.HUnit


all_tests :: [Test]
all_tests = Day01_test.tests ++ Day02_test.tests

main :: IO ()
main = do
    _ <- runTestTT $ TestList all_tests
    putStrLn "Testing is completed."