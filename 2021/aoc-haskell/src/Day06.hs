{-# LANGUAGE ScopedTypeVariables #-}

module Day06 (day06, day06p2) where

import Data.List (sort)
import Data.Map (fromList, fromListWith, lookup, toList, Map)
import Data.Maybe (fromMaybe)

newDay :: [Int] -> [Int]
newDay fish = do
  let pregnantFishAmount = length (filter (== 0) fish)
  let computeClock = map (\a -> a -1) fish
  let newBorns = replicate pregnantFishAmount 8
  let resetted = replicate pregnantFishAmount 6

  let filteredInvalid = filter (>= 0) computeClock
  sort (filteredInvalid ++ newBorns ++ resetted)

day06 :: [Int] -> Int -> Int
day06 fish days = do
  if days <= 0
    then length fish
    else day06 (newDay fish) (days -1)

frequency :: [Int] -> Map Int Int
frequency xs = fromListWith (+) [(x, 1) | x <- xs]

computeFishes :: Map Int Int -> Map Int Int
computeFishes fishMap = do
  let birth = fromMaybe 0 (Data.Map.lookup 0 fishMap)
  let day0 = fromMaybe 0 (Data.Map.lookup 1 fishMap)
  let day1 = fromMaybe 0 (Data.Map.lookup 2 fishMap)
  let day2 = fromMaybe 0 (Data.Map.lookup 3 fishMap)
  let day3 = fromMaybe 0 (Data.Map.lookup 4 fishMap)
  let day4 = fromMaybe 0 (Data.Map.lookup 5 fishMap)
  let day5 = fromMaybe 0 (Data.Map.lookup 6 fishMap)
  let day6 = fromMaybe 0 (Data.Map.lookup 7 fishMap) + birth
  let day7 = fromMaybe 0 (Data.Map.lookup 8 fishMap)
  let day8 = birth
  fromList [(0, day0), (1, day1), (2, day2), (3, day3), (4, day4), (5, day5), (6, day6), (7, day7), (8, day8)]


day06p2W :: Map Int Int -> Int -> Int
day06p2W fishMap days = do
  if days <= 0
    then sum (map snd (toList fishMap))
    else day06p2W (computeFishes fishMap) (days -1)

day06p2 :: [Int] -> Int -> Int
day06p2 fish days = do
  let initialFishMap = frequency fish
  day06p2W initialFishMap days

