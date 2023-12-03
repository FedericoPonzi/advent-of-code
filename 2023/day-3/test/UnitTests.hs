module Main where

import Day03_test

import Test.HUnit


allTests :: [Test]
allTests =  Day03_test.tests

main :: IO ()
main = do
    _ <- runTestTT $ TestList allTests
    putStrLn "Testing is completed."