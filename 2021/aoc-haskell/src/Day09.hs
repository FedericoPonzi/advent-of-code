{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day09 where
import GHC.Generics (Generic)
import Data.Hashable
import Control.Monad (join)
import Data.Graph.UGraph
import Data.Graph.Types

type Label = Int
type Depthness = Int

data Element = Element Depthness Label
    deriving (Show, Ord, Eq, Generic)
instance Hashable Element

isLowest (d, adj) = do
   (length $ filter(<=d) adj) == 0

-- Calculate risk level of lowest points
day09 :: [(Int, [Int])] -> Int
day09 adjList = do
   let lowestList :: [(Int, [Int])] = filter isLowest adjList
   let lowestRiskList :: [Int] = map (\a -> fst a +1)  lowestList
   sum lowestRiskList