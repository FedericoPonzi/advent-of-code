module Main where

import Day01_test
import Test.HUnit
import System.IO
all_tests = Day01_test.tests

f :: [String] -> [Integer]
f = map read

main :: IO ()
main = do
    _ <- runTestTT $ TestList all_tests
    putStrLn "Testing is completed."