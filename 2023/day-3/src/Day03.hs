module Day03 (day03, getNumberFromPos) where
import Data.Char (isDigit)
import Data.List (intercalate)

findSymbols :: [[String]] -> [Int]
findSymbols grid = map (mapRow grid) $ zip [0..] $ map (zip [0..]) grid

mapRow :: [[String]] -> (Int, [( Int, String )]) -> Int
mapRow grid row = sum $ map (getNumberFromSymbol grid (fst row) . fst) $ filter (isSymbol . snd) $ snd row

getNumberFromSymbol :: [[String]] -> Int -> Int -> Int
getNumberFromSymbol grid row col = do
  let top_left = getNumberFromPos (grid !! (row - 1)) (col -1)
  let top = getNumberFromPos (grid !! (row - 1)) col
  let top_right = getNumberFromPos (grid !! (row -1)) (col + 1)
  let left = getNumberFromPos (grid !! row) (col -1)
  let right = getNumberFromPos (grid !! row) (col + 1)
  let bottom_left = getNumberFromPos (grid !! (row + 1)) (col - 1)
  let bottom = getNumberFromPos (grid !! (row + 1)) col
  let bottom_right = getNumberFromPos (grid !! (row + 1)) (col + 1)
  let top_row = if row > 0 then if all isDigit $ grid !! (row - 1) !! col then top else top_left + top_right else 0
  let bottom_row = if row + 1 < length grid then if all isDigit $ grid !! (row + 1) !! col then bottom else bottom_left + bottom_right else 0
  left + right + top_row + bottom_row

isSymbol :: String -> Bool
isSymbol c = not (all isDigit c) &&  c /= "."

day03 :: [[String]] -> Int
day03 inputList = do
  let numbers = findSymbols inputList
  sum numbers


getNumberFromPos ::[String] ->  Int ->  Int
getNumberFromPos rowList pos = do
  if length rowList > pos && pos >= 0 && all isDigit (rowList !! pos) then read $ intercalate "" $ getAdjacentDirectionLeft (pos - 1) rowList ++ [rowList !! pos] ++ getAdjacentDirectionRight (pos + 1) rowList
  else 0

getAdjacentDirectionRight :: Int -> [String] -> [String]
getAdjacentDirectionRight pos rowList = if pos < length rowList && all isDigit (rowList !! pos) then [rowList !! pos] ++ getAdjacentDirectionRight (pos + 1) rowList
                                         else []

getAdjacentDirectionLeft :: Int -> [String] -> [String]
getAdjacentDirectionLeft pos rowList = if pos >= 0 && all isDigit (rowList !! pos) then getAdjacentDirectionLeft (pos - 1) rowList ++ [rowList !! pos]
                                       else []




