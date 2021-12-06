{-# LANGUAGE ScopedTypeVariables #-}

module Day05 (day05, Segment (..), day05p2) where

import Data.List (sort)
import Data.Map (fromListWith, toList)

data Segment = Segment {startPoint :: (Int, Int), endPoint :: (Int, Int)} deriving (Show, Eq, Ord)

sortPoints :: Segment -> Segment
sortPoints segment = do
  let sorted = sort [startPoint segment, endPoint segment]
  let (x1, y1) = head sorted
  let (x2, y2) = last sorted
  Segment (x1, y1) (x2, y2)

predicateVerticalHorizontal :: Segment -> Bool
predicateVerticalHorizontal seg = do
  let (x1, y1) = startPoint seg
  let (x2, y2) = endPoint seg
  x1 == x2 || y1 == y2

verticalSegments :: [Segment] -> [Segment]
verticalSegments segments = do
  filter predicateVerticalHorizontal segments


genPoints :: Segment -> [(Int, Int)]
genPoints segment = do
  let sorted = sort [startPoint segment, endPoint segment]
  let (x1, y1) = head sorted
  let (x2, y2) = last sorted
  let recursion x y s = [(x1, y1)] ++ genPoints (Segment (x, y) s)
  if x1 < x2
    then recursion (x1 + 1) y1 (last sorted)
    else
      if y1 < y2
        then recursion x1 (y1 + 1) (last sorted)
        else [(x1, y1)]

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

day05 :: [Segment] -> Int
day05 segments = do
  let filteredVerticals = verticalSegments segments
  let allPointsInSegment :: [(Int, Int)] = concatMap genPoints filteredVerticals
  let repetitions = map snd (frequency allPointsInSegment)
  length (filter (> 1) repetitions)

genPointsDiagonal :: Segment -> [(Int, Int)]
genPointsDiagonal segment = do
  let (x1, y1) = startPoint segment
  let (x2, y2) = endPoint segment
  let recursion x y s = [(x1, y1)] ++ genPointsDiagonal (Segment (x, y) s)
  let newX = if x1 > x2 then x1 - 1 else x1 +1
  let newY = if y1 > y2 then y1 - 1 else y1 +1
  if x1 == x2 && y1 == y2 then
    [(x1,y1)]
  else
     recursion newX newY (endPoint segment)



day05p2 :: [Segment] -> Int
day05p2 segments = do
  let nonVertical = filter (not . predicateVerticalHorizontal) segments
  let sortedSegments = map sortPoints nonVertical
  let allPointsInSegmentDiag :: [(Int, Int)] = concatMap genPointsDiagonal sortedSegments
  let allPointsInSegmentVertHor = concatMap genPoints (verticalSegments segments)
  let repetitions = map snd (frequency (allPointsInSegmentDiag ++ allPointsInSegmentVertHor))
  length (filter (> 1) repetitions)
