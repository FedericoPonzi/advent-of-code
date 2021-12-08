module Main where

import Day08_test

import Test.HUnit


allTests :: [Test]
allTests =  Day08_test.tests

main :: IO ()
main = do
    _ <- runTestTT $ TestList allTests
    putStrLn "Testing is completed."