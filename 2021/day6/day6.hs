module Day6 (problem1, problem2) where

problem1 :: [Int] -> Int
problem1 fish =
  let fishMap = map (\x -> length (filter (== x) fish)) [0..8]
  in sum (applyDays 80 fishMap)

problem2 :: [Int] -> Int
problem2 fish =
  let fishMap = map (\x -> length (filter (== x) fish)) [0..8]
  in sum (applyDays 256 fishMap)

applyDays :: Int -> [Int] -> [Int]
applyDays 0 fish = fish
applyDays days fish =
  applyDays (days - 1) [fish !! 1, fish !! 2, fish !! 3, fish !! 4, fish !! 5, fish !! 6, (fish !! 0 + fish !! 7), fish !! 8, fish !! 0]
