module Main where

import Day13_test

import Test.HUnit


allTests :: [Test]
allTests =  Day13_test.tests

main :: IO ()
main = do
    _ <- runTestTT $ TestList allTests
    putStrLn "Testing is completed."