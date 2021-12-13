module Main where

import Day12_test

import Test.HUnit


allTests :: [Test]
allTests =  Day12_test.tests

main :: IO ()
main = do
    _ <- runTestTT $ TestList allTests
    putStrLn "Testing is completed."