{-# LANGUAGE ScopedTypeVariables #-}

module Day08 where
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as DataMap
import qualified Data.Set as DataSet
import Data.Maybe(fromJust)

isUniquePattern :: Foldable t => t a -> Bool
isUniquePattern pattern = do
  length pattern `elem` [2,3,4,7]

countUniquePatterns :: ([String], [String]) -> Int
countUniquePatterns (_, outputPattern)= do
  length (filter isUniquePattern outputPattern)

day08 :: [([String], [String])] -> Int
day08 signalPatterns = do
  sum (map countUniquePatterns signalPatterns)

type PatternAssociationMap = DataMap.Map Int (DataSet.Set Char)

buildPatterns :: [String] -> PatternAssociationMap -> PatternAssociationMap
buildPatterns (p:patternLine) associationMap = do
  let pAsSet :: DataSet.Set Char = DataSet.fromList p
  let returnBuildPattern val = buildPatterns patternLine (DataMap.insert val pAsSet associationMap)

  if length p == 2 then
    returnBuildPattern 1
  else if length p == 3 then
    returnBuildPattern 7
  else if length p == 4 then
    returnBuildPattern 4
  else if length p == 7 then
    returnBuildPattern 8
  else if length p == 6 then  -- either a 6 or a 9.
    do
      let seven = fromJust (DataMap.lookup 7 associationMap)
      if length (DataSet.difference pAsSet seven) == 4 then -- it's a 6
        returnBuildPattern 6
      else
        do
          let four = fromJust (DataMap.lookup 4 associationMap)
          if length (DataSet.difference pAsSet four) == 3 then -- it must be a 0
                returnBuildPattern 0
          else                                                 -- it must be a 9
                returnBuildPattern 9
  else do
         let four = fromJust (DataMap.lookup 4 associationMap)
         if length (DataSet.difference pAsSet four) == 3 then -- it must be a 2
                returnBuildPattern 2
         else do -- it's either a 3 or a 5
            let seven = fromJust (DataMap.lookup 7 associationMap)
            if length (DataSet.difference pAsSet seven) == 2 then -- length 2: 3, length 3: 5
                  returnBuildPattern 3
            else
                  returnBuildPattern 5

buildPatterns [] associationMap = associationMap

invert :: (Ord k, Ord v) => DataMap.Map k [v] -> DataMap.Map v [k]
invert m = DataMap.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, vs) <- DataMap.toList m, v <- vs]

buildDigit :: PatternAssociationMap -> String -> Int
buildDigit associationMap p = do
  let pAsSet :: DataSet.Set Char = DataSet.fromList p
  let inverted :: DataMap.Map (DataSet.Set Char) Int = DataMap.fromList( concatMap (\(k, v) -> [(v, k)]) (DataMap.toList associationMap))
  fromJust (DataMap.lookup pAsSet inverted)

digitsJoiner :: [Int] -> Int
digitsJoiner = read . concatMap show

singleRow :: ([String], [String]) -> Int
singleRow pattern = do
  let signalMap :: PatternAssociationMap = buildPatterns ( sortBy (compare `on` length) (fst pattern)) DataMap.empty
  digitsJoiner (map (buildDigit signalMap) (snd pattern))

day08p2 :: [([String], [String])] -> Int
day08p2 signalPatterns = do
  sum (map singleRow signalPatterns)
