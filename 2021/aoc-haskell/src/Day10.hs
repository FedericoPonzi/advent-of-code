{-# LANGUAGE ScopedTypeVariables #-}

module Day10 where

import Data.Either (fromLeft, lefts)
import Data.List (sort)

data Chunks = OpenRound | OpenSquare | OpenCurly | OpenAngle | CloseRound | CloseSquare | CloseCurly | CloseAngle deriving (Show, Eq)

chunksFromChar :: String -> Chunks
chunksFromChar c = case c of
  "{" -> OpenCurly
  "}" -> CloseCurly
  "<" -> OpenAngle
  ">" -> CloseAngle
  "(" -> OpenRound
  ")" -> CloseRound
  "[" -> OpenSquare
  "]" -> CloseSquare
  _ -> error "Invalid chunk provided"

isOpenChunk :: Chunks -> Bool
isOpenChunk c = do
  c `elem` [OpenCurly, OpenSquare, OpenRound, OpenAngle]

isMatchingChunk c1 c2 = case (c1, c2) of
  (OpenCurly, CloseCurly) -> True
  (OpenAngle, CloseAngle) -> True
  (OpenRound, CloseRound) -> True
  (OpenSquare, CloseSquare) -> True
  _ -> False

findIllegal :: [Chunks] -> Either Chunks [Chunks]
findIllegal (h : l) = do
  if length l == 0
    then Right [] -- incomplete
    else
      if isOpenChunk (head l)
        then case findIllegal l of
          Left a -> Left a
          Right remaining -> findIllegal (h : remaining) -- while there are open brackets, recurse.
        else
          if isMatchingChunk h (head l)
            then Right (drop 1 l)
            else Left (head l)

countPoints either = case either of
  Left a -> case a of
    CloseRound -> 3
    CloseSquare -> 57
    CloseCurly -> 1197
    CloseAngle -> 25137
  Right _ -> 0

day10 :: [[Chunks]] -> Int
day10 chunks = do
  sum (map (countPoints . findIllegal) chunks)

getMatchingChunk c = case c of
  OpenRound -> CloseRound
  OpenCurly -> CloseCurly
  OpenSquare -> CloseSquare
  OpenAngle -> CloseAngle

autocompleteChunks :: [Chunks] -> Either [Chunks] [Chunks]
autocompleteChunks (h : l) = do
  if null l
    then Left [getMatchingChunk h]
    else
      if isOpenChunk (head l)
        then case autocompleteChunks l of
          Left a -> Left (getMatchingChunk h : a)
          Right remaining -> autocompleteChunks (h : remaining) -- while there are open brackets, recurse.
        else
          if isMatchingChunk h (head l)
            then Right (drop 1 l)
            else Left [head l]

filterIncomplete a = do
  case findIllegal a of
    Left _ -> False
    Right _ -> True

getChunkValue :: Num p => Chunks -> p
getChunkValue c = case c of
  CloseRound -> 1
  CloseSquare -> 2
  CloseCurly -> 3
  CloseAngle -> 4

middle = m =<< drop 1
  where
    m [] = take 1
    m [_] = take 2
    m (_ : _ : ys) = m ys . drop 1

foldFn :: Int -> Chunks -> Int
foldFn acc chunk = acc * 5 + getChunkValue chunk

day10p2 :: [[Chunks]] -> Int
day10p2 chunks = do
  let incomplete :: [[Chunks]] = filter filterIncomplete chunks
  head $
    middle $
      sort $
        map
          ( \a -> do
              let z :: Either [Chunks] [Chunks] = autocompleteChunks a
              let b :: [Chunks] = reverse (head (lefts [z]))
              let res :: Int = foldl foldFn 0 b
              res
          )
          incomplete
