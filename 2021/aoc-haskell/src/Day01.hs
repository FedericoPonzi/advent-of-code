module Day01( day01 ) where

day01 :: [Int] -> Int

day01 (x:y:xs) = if y > x
                 then 1 + day01(y:xs)
                 else 0 + day01(y:xs)
day01 [] = 0
day01 [x] = 0