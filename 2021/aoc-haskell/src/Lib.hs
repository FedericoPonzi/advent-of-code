module Lib
    ( module Day01, someFunc
    ) where

import Day01

someFunc :: IO ()
someFunc = putStrLn "someFunc"
myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

foo::Int ->Int
foo = (+1)
