module Day12 where

import Data.Char (isUpper)
import qualified Data.Map as DataMap
import Data.Maybe (fromJust)
import Data.List(nub)

data CaveType = BigCave String | SmallCave String | Start | End deriving (Eq, Ord, Show)

fromStringToCave :: [Char] -> CaveType
fromStringToCave input
  | input == "start" = Start
  | input == "end" = End
  | all isUpper input = BigCave input
  | otherwise = SmallCave input

isSmallCave c = case c of
  SmallCave _ -> True
  _ -> False

isBigCave c = case c of
  BigCave _ -> True
  _ -> False

isStart c = case c of
  Start -> True
  _ -> False

isEnd c = case c of
  End -> True
  _ -> False

traverseCaves :: [CaveType] -> DataMap.Map CaveType [CaveType] -> (CaveType -> [CaveType] -> Bool) -> Int
traverseCaves visited cavernsMap legalize = do
  let lastElement = last visited
  if isEnd lastElement
    then 1
    else do
      let links = fromJust (lastElement `DataMap.lookup` cavernsMap)
      let legalVisits = filter (`legalize` visited) links
      sum (map (\a -> traverseCaves (visited ++ [a]) cavernsMap legalize) legalVisits )

legalize :: CaveType -> [CaveType] -> Bool
legalize a visited = isBigCave a || isEnd a || (isSmallCave a && notElem a visited)

day12 :: DataMap.Map CaveType [CaveType] -> Int
day12 cavesMap = do
  traverseCaves [Start] cavesMap legalize

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

alreadyVisitedSmallCaveTwice visited = hasDuplicates (filter (isSmallCave) visited)
legalize2 :: CaveType -> [CaveType] -> Bool
legalize2 a visited = isBigCave a || isEnd a || (isSmallCave a && (not(alreadyVisitedSmallCaveTwice visited) || notElem a visited))

day12p2 :: DataMap.Map CaveType [CaveType] -> Int
day12p2 cavesMap = do
  traverseCaves [Start] cavesMap legalize2
