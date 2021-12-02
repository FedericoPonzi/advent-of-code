module Day02 (day02, day02p2, Commands (..)) where

data Commands = Forward Int | Down Int | Up Int deriving (Read, Show)

folderp1 :: (Int, Int) -> Commands -> (Int, Int)
folderp1 (depth, horizontal) c = case c of
  Forward x -> (depth + x, horizontal)
  Down x -> (depth, horizontal + x)
  Up x -> (depth, horizontal - x)

day02 :: [Commands] -> Int
day02 x =
  let (depth, horizontal) = foldl folderp1 (0, 0) x
   in depth * horizontal

foldp2 :: (Int, Int, Int) -> Commands -> (Int, Int, Int)
foldp2 (depth, aim, horizontal) a = case a of
  Forward x -> (depth + aim * x, aim, horizontal + x)
  Down x -> (depth, aim + x, horizontal)
  Up x -> (depth, aim - x, horizontal)

day02p2 :: [Commands] -> Int
day02p2 x =
  let (a, _, c) = foldl foldp2 (0, 0, 0) x
   in a * c
