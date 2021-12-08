module Day7 (problem1, problem2) where

problem1 :: [Int] -> Int
problem1 positions =
  minimum (map (calculateConsumption positions) [(minimum positions)..(maximum positions)])

problem2 :: [Int] -> Int
problem2 positions =
  minimum (map (calculateConsumption2 positions) [(minimum positions)..(maximum positions)])

calculateConsumption :: [Int] -> Int -> Int
calculateConsumption positions x = sum (map (abs . ((-) x)) positions)

calculateConsumption2 :: [Int] -> Int -> Int
calculateConsumption2 positions x = sum (map (cost . abs . ((-) x)) positions)

cost :: Int -> Int
cost x = x * (x + 1) `div` 2
