module Day1 (problem1, problem2) where

import Control.Monad (liftM2, liftM3)

chunk2 :: [t] -> [(t, t)]
chunk2 = liftM2 zip id (drop 1)

chunk3 :: [t] -> [(t, t, t)]
chunk3 = liftM3 zip3 id (drop 1) (drop 2)

problem1 :: [Int] -> Int
problem1 = length . filter (\(x, y) -> y > x) . chunk2

problem2 :: [Int] -> Int
problem2 = length . filter (\(x, y) -> y > x) . chunk2 . map (\(x, y, z) -> x + y + z) . chunk3
