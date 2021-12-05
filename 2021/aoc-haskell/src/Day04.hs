module Day04 (day04, BingoTable, day04p2) where

-- In american bingo, you need to check columns and rows.
-- In order to simplify this check, I'm transforming columns to rows.
-- A row contains numbers which haven ot being drawn yet.
--newtype BingoTable = BingoTable { getBingoTable :: [[Int]] }
type BingoTable = [[Int]]

filterNumber :: Int -> [BingoTable] -> [BingoTable]
filterNumber number tables = do
  map (map (filter (/= number))) tables

isWinner :: [[a]] -> Bool
isWinner = foldl (\acc row -> null row || acc) False

finalResult :: Int -> [BingoTable] -> Int
finalResult n winner = do
  let (winnerNoCols, _) = splitAt 5 (head winner)
   in n * sum (concat winnerNoCols)

day04 :: [Int] -> [BingoTable] -> Int
day04 (n : xs) boards = do
  let filteredBoards = filterNumber n boards
  let winner = filter isWinner filteredBoards
  if not (null winner)
    then finalResult n winner
    else day04 xs filteredBoards

day04p2 :: [Int] -> [BingoTable] -> Int
day04p2 (n : xs) boards = do
  let filteredBoards = filterNumber n boards
  let lastWinner = filter (foldl (\acc row -> null row || acc) False) filteredBoards
  let filteredBoardsNoWinners = filter (not . isWinner) filteredBoards
  if null filteredBoardsNoWinners
    then finalResult n lastWinner
    else day04p2 xs filteredBoardsNoWinners
