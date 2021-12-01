module Day01( day01, day01p2 ) where

day01 :: [Int] -> Int

day01 (x:y:xs) = if y > x
                 then 1 + day01(y:xs)
                 else 0 + day01(y:xs)
day01 _ = 0

day01p2 :: [Int] -> Int
day01p2 (x:y:w:z:xs) = if z > x
                       then 1 + day01p2(y:w:z:xs)
                       else 0 + day01p2(y:w:z:xs)
day01p2 _ = 0