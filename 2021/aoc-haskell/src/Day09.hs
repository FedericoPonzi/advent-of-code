{-# LANGUAGE DeriveGeneric #-}
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

myGraph :: UGraph Element ()
myGraph = fromEdgesList
    [ Element 1 2 <->  Element 4 5
    ]

-- Calculate risk level of lowest points
day09 :: UGraph Element () -> Int
day09 map = do
  1
