{-# LANGUAGE ScopedTypeVariables #-}

module Day08 where

import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as DataMap
import Data.Maybe (fromJust)
import qualified Data.Set as DataSet

isUniquePattern :: Foldable t => t a -> Bool
isUniquePattern pattern = do
  length pattern `elem` [2, 3, 4, 7]

countUniquePatterns :: ([String], [String]) -> Int
countUniquePatterns (_, outputPattern) = do
  length (filter isUniquePattern outputPattern)

day08 :: [([String], [String])] -> Int
day08 signalPatterns = do
  sum (map countUniquePatterns signalPatterns)

type PatternAssociationMap = DataMap.Map Int (DataSet.Set Char)

patternToNumber :: PatternAssociationMap -> DataSet.Set Char -> Int
patternToNumber associationMap pAsSet = do
  let diffSet n = length (DataSet.difference pAsSet (fromJust (DataMap.lookup n associationMap)))
  case length pAsSet of
    2 -> 1
    3 -> 7
    4 -> 4
    7 -> 8
    5 -> do
      let diffSeven = diffSet 7
      let diffFour = diffSet 4
      case (diffFour, diffSeven) of
        (3, _) -> 2
        (_, 3) -> 5
        _ -> 3
    _ -> do
      let diffSeven = diffSet 7
      let diffFour = diffSet 4
      case (diffFour, diffSeven) of
        (_, 4) -> 6
        (3, _) -> 0
        _ -> 9

buildPatterns :: [String] -> PatternAssociationMap -> PatternAssociationMap
buildPatterns (p : patternLine) associationMap = do
  let pAsSet :: DataSet.Set Char = DataSet.fromList p
  let returnBuildPattern val = buildPatterns patternLine (DataMap.insert val pAsSet associationMap)
  returnBuildPattern (patternToNumber associationMap pAsSet)

buildPatterns [] associationMap = associationMap

invert :: (Ord k, Ord v) => DataMap.Map k [v] -> DataMap.Map v [k]
invert m = DataMap.fromListWith (++) pairs
  where
    pairs = [(v, [k]) | (k, vs) <- DataMap.toList m, v <- vs]

buildDigit :: PatternAssociationMap -> String -> Int
buildDigit associationMap p = do
  let pAsSet :: DataSet.Set Char = DataSet.fromList p
  let inverted :: DataMap.Map (DataSet.Set Char) Int = DataMap.fromList (concatMap (\(k, v) -> [(v, k)]) (DataMap.toList associationMap))
  fromJust (DataMap.lookup pAsSet inverted)

digitsJoiner :: [Int] -> Int
digitsJoiner = read . concatMap show

singleRow :: ([String], [String]) -> Int
singleRow pattern = do
  let signalMap :: PatternAssociationMap = buildPatterns (sortBy (compare `on` length) (fst pattern)) DataMap.empty
  digitsJoiner (map (buildDigit signalMap) (snd pattern))

day08p2 :: [([String], [String])] -> Int
day08p2 signalPatterns = do
  sum (map singleRow signalPatterns)
