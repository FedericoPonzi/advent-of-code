{-# LANGUAGE ScopedTypeVariables #-}

module Day09 (day09, day09p2) where

import Data.List
import Data.Maybe (fromMaybe)

safeGet :: Int -> [a] -> Maybe a
safeGet n arr = if n < len && n >= 0 then Just (arr !! n) else Nothing where len = length arr

safeGetPos :: Int -> Int -> [[Int]] -> Maybe Int
safeGetPos row col m = safeGet col =<< safeGet row m

innerLength :: Foldable t => [t a] -> Int
innerLength l = length (head l)

isLowest :: (Foldable t, Ord a) => (a, t a) -> Bool
isLowest (d, adj) = do
  not (any (<= d) adj)

-- Calculate risk level of lowest points
day09 :: [(Int, [Int])] -> Int
day09 adjList = do
  let lowestList :: [(Int, [Int])] = filter isLowest adjList
  let lowestRiskList :: [Int] = map (\a -> 1 + fst a) lowestList
  sum lowestRiskList

findBasins m rowLen colLen visited row col = do
  let index = (row, col)
  if index `elem` visited
    then (0, visited)
    else
      if (m !! row !! col) == 9
        then (0, index : visited)
        else do
          let filterValid (r, c) = 0 <= r && r < rowLen && 0 <= c && c < colLen
          let directions = filter filterValid [(row -1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
          let foldFn (accBasins, accVisited) (r, c) =
                let (bFound, visitedb) = findBasins m rowLen colLen accVisited r c
                 in (bFound + accBasins, visitedb)
          let (basins, vistedd) = foldl foldFn (0, index : visited) directions
          (basins + 1, vistedd)

countBasins rowLen colLen m toVisit visited basins = do
  let unvisited = take 1 toVisit
  if null unvisited
    then (basins, visited)
    else do
      let (row, col) = head unvisited
      let (b, visitedNew) = findBasins m rowLen colLen [] row col
      countBasins rowLen colLen m (toVisit \\ visitedNew) (visitedNew ++ visited) (b : basins)

filterNines matrix toVisit = do
  let isNine (r, c) = matrix !! r !! c == 9
  filter isNine toVisit

day09p2 :: [[Int]] -> Int
day09p2 matrix = do
  let rowLen = length matrix
  let colLen = innerLength matrix
  let zipped = concatMap (\a -> zip (repeat a) [0 .. colLen -1]) [0 .. rowLen -1]
  let visited = filterNines matrix zipped
  let totalBasins = countBasins rowLen colLen matrix zipped visited []
  let (basins, visited) = totalBasins
  product $ take 3 $ reverse $ sort basins
