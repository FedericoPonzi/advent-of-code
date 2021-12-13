{-# LANGUAGE ScopedTypeVariables #-}

module Day11 where

--getFlashEnergyLevel energyMatrix rowIndex colIndex row = do

hasFleshed energy = if energy >= 10 then 1 else 0

flashOctopuses :: [[Int]] -> [[Int]]
flashOctopuses energyMatrix =
  map
    ( \(rowIndex, row) ->
        map
          ( \(colIndex, energy) -> do
              let getEnergyAt reqRow reqCol = if 0 <= reqRow && reqRow < length energyMatrix && 0 <= reqCol && reqCol < length row then hasFleshed ((energyMatrix !! reqRow) !! reqCol) else 0
              let topLeft :: Int = getEnergyAt (rowIndex - 1) (colIndex - 1)
              let left :: Int = getEnergyAt rowIndex (colIndex - 1)
              let bottomLeft = getEnergyAt (rowIndex + 1) (colIndex - 1)

              let topRight :: Int = getEnergyAt (rowIndex - 1) (colIndex + 1)
              let right = getEnergyAt rowIndex (colIndex + 1)
              let bottomRight = getEnergyAt (rowIndex + 1) (colIndex + 1)

              let top :: Int = getEnergyAt (rowIndex - 1) colIndex
              let bottom = getEnergyAt (rowIndex + 1) colIndex

              if 0 < energy && energy < 10 then (energy + sum [topLeft, top, topRight, left, right, bottom, bottomLeft, bottomRight]) else -1
          )
          (zip [0 ..] row)
    )
    (zip [0 ..] energyMatrix)

{-
9 + 1 = 10

-}

increaseLevel :: [[Int]] -> [[Int]]
increaseLevel = map (map (+ 1))

flashingStep :: ([[Int]], Int) -> ([[Int]], Int)
flashingStep (matrix, flashedPrevious) = do
  let flashedMatrix :: [[Int]] = flashOctopuses matrix
  let flashed = sum (map (length . filter (\a -> a >= 10 || a < 0)) flashedMatrix)
  if flashed /= flashedPrevious then flashingStep (flashedMatrix, flashed) else (flashedMatrix, flashed)

singleEpoch :: [[Int]] -> (Int, [[Int]])
singleEpoch octopusEnergy = do
  let octopusEnergyIncreased = increaseLevel octopusEnergy
  let (flashedMatrix, _) = flashingStep (octopusEnergyIncreased, 0)
  let totalFlashed = sum (map (length . filter (< 0)) flashedMatrix)
  (totalFlashed, map (map (\a -> if a == -1 then 0 else a)) flashedMatrix)

day11 :: [[Int]] -> Int -> (Int, [[Int]])
day11 octopusEnergy steps = do
  foldl
    ( \(flashes, matrix) _ -> do
        let (roundFlashed, newMatrix) = singleEpoch matrix
        (flashes + roundFlashed, newMatrix)
    )
    (0, octopusEnergy)
    [0..steps - 1]

day11p2 :: [[Int]] -> Int -> Int
day11p2 octopusEnergy steps = do
  if any (any ( > 0)) octopusEnergy then day11p2 (snd (day11 octopusEnergy 1)) steps +1 else steps