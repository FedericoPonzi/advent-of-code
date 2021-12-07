{-# LANGUAGE ScopedTypeVariables #-}

module Day07 (day07, day07p2) where

import Data.List (foldl1')
import qualified Data.Map as DataMap
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)

frequency :: (Ord k, Num a) => [k] -> DataMap.Map k a
frequency xs = DataMap.fromListWith (+) [(x, 1) | x <- xs]

computeFuelRequired1 :: DataMap.Map Int Int -> Int -> Int -> Int
computeFuelRequired1 crabPosDistribution from to = abs (to - from) * fromJust (DataMap.lookup from crabPosDistribution)

calculateFuel :: [Int] -> (DataMap.Map Int Int -> Int -> Int -> Int) -> Int
calculateFuel crabsPositions computeFuel = do
  -- 1. frequencies to know how many of each fish is there
  let crabPosDistribution = frequency crabsPositions
  -- used to index crabPosdistribution:
  let uniqPos = fromList crabsPositions
  let uniqPosL = toList uniqPos

  -- create an array [0..max]
  let distanceVector = [0 .. (maximum uniqPosL)]
  -- to is always >= from
  let finalDistanceVector = map (\dest -> sum (map (\from -> computeFuel crabPosDistribution from dest) uniqPosL)) distanceVector
  foldl1' min finalDistanceVector

day07 :: [Int] -> Int
day07 crabsPositions = do
  calculateFuel crabsPositions computeFuelRequired1

computeFuelRequired2 :: DataMap.Map Int Int -> Int -> Int -> Int
computeFuelRequired2 crabsPosDistribution from to = do
  let sumToN n = fromIntegral (n * (n + 1) `div` 2)
  fromIntegral (sumToN (abs (to - from)) * fromJust (DataMap.lookup from crabsPosDistribution))

day07p2 :: [Int] -> Int
day07p2 crabsPositions = do
  calculateFuel crabsPositions computeFuelRequired2
