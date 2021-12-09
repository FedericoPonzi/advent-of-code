module Main where

import Day09_test

import Test.HUnit


allTests :: [Test]
allTests =  Day09_test.tests

main :: IO ()
main = do
    _ <- runTestTT $ TestList allTests
    putStrLn "Testing is completed."