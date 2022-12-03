{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Char(ord, isLower)
import Data.List(intersect)
import Text.Printf

main :: IO ()
main = do
  rucksacks <- fmap T.lines $ TIO.readFile "input"
  let prioritySum = sum . map rucksackValue $ rucksacks
  printf "Priority sum: %d\n" prioritySum
  let groups = chunksOf 3 rucksacks
      badgeSum = sum $ map groupBadge groups
  printf "Badge sum: %d\n" badgeSum

rucksackValue :: T.Text -> Int
rucksackValue line =
  let lineLength = T.length line
      splitPoint = div lineLength 2
      (firstHalf, lastHalf) = T.splitAt splitPoint line
      shared = intersect (T.unpack firstHalf) (T.unpack lastHalf) in
    charToValue (head shared)

groupBadge :: [T.Text] -> Int
groupBadge rucksacks =
  let badge = foldl1 intersect (map T.unpack rucksacks) in
    charToValue (head badge)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size l =
  take size l : chunksOf size (drop size l)

charToValue :: Char -> Int
charToValue letter =
  if isLower letter
  then (ord letter) - 96
  else (ord letter) - 38
