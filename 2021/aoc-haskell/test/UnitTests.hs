module Main where

import Day11_test

import Test.HUnit


allTests :: [Test]
allTests =  Day11_test.tests

main :: IO ()
main = do
    _ <- runTestTT $ TestList allTests
    putStrLn "Testing is completed."