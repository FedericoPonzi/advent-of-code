module Day03 (day03, day03p2) where

bin2dec :: [Char] -> Int
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
  where
    c2i c = if c == '0' then 0 else 1

toDecimal :: [Int] -> Int
-- array of bits to decimal
toDecimal l = bin2dec (concatMap (\a -> if a == 0 then "0" else "1") l)


calculateGamma :: Int -> [Int] -> [Int]
calculateGamma halfLen = map (\a -> if a > halfLen then 1 else 0)

calculateEpsilon :: [Int] -> [Int]
calculateEpsilon = map (\a -> if a == 0 then 1 else 0)


day03 :: [[Int]] -> Int
-- input format: [[0,1,0,1],[0,1,1,1]]
day03 inputList = do
  let halfLen = fromIntegral (length inputList) `div` 2
  let accumulator = replicate (length (head inputList)) 0
  let summed = foldl (zipWith (+)) accumulator inputList
  let gamma = calculateGamma halfLen summed
  let epsilonDec = toDecimal (calculateEpsilon gamma)
  epsilonDec * toDecimal gamma

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

resolveOxygen :: [[Int]] -> Int -> [Int]
resolveOxygen inputList pos = do
  let halfLen = ceiling (intToFloat (length inputList) / 2)
  let summed = foldl (\acc inp -> (inp !! pos) + acc) 0 inputList
  let filtered = filter (\el -> if summed >= halfLen then (el !! pos) == 1 else (el !! pos) == 0) inputList
  let len = length filtered
  if len > 1 then resolveOxygen filtered (pos + 1) else head filtered

resolveCo2 :: Integral a => [[a]] -> Int -> [a]
resolveCo2 inputList pos = do
  let halfLen = ceiling (intToFloat (length inputList) / 2)
  let summed = foldl (\acc inp -> (inp !! pos) + acc) 0 inputList
  let filtered = filter (\el -> if summed < halfLen then (el !! pos) == 1 else (el !! pos) == 0) inputList
  let len = length filtered
  if len > 1 then resolveCo2 filtered (pos + 1) else head filtered

day03p2 :: [[Int]] -> Int
day03p2 inputList = do
  let oxygen = resolveOxygen inputList 0
  let co2 = resolveCo2 inputList 0
  toDecimal co2 * toDecimal oxygen
