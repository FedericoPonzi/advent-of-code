module Main where

import Day01_test
import Day02_test
import Day03_test
import Test.HUnit


allTests :: [Test]
allTests = Day03_test.tests -- ++ Day01_test.tests ++ Day02_test.tests 

main :: IO ()
main = do
    _ <- runTestTT $ TestList allTests
    putStrLn "Testing is completed."