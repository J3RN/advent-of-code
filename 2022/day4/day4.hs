{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Either (rights)
import Text.Printf (printf)
import Debug.Trace (trace)

type Assignment = (Int, Int)

main :: IO ()
main = do
  contents <- TIO.readFile "input"
  let myLines = T.lines . T.dropEnd 1 $ contents
  let assignmentEithers = map parseAssignments $ myLines
  let pairAssignments = rights assignmentEithers
  printf "Num containing: %d\n" (length . filter pairContains $ pairAssignments)
  printf "Num overlapping: %d\n" (length . filter pairOverlaps $ pairAssignments)

pairContains :: (Assignment, Assignment) -> Bool
pairContains (first, second) =
  first `contains` second || second `contains` first

contains :: Assignment -> Assignment -> Bool
contains first second =
  (fst first) <= (fst second) && (snd first) >= (snd second)

pairOverlaps :: (Assignment, Assignment) -> Bool
pairOverlaps (first, second) =
  first `overlaps` second || second `overlaps` first

overlaps :: Assignment -> Assignment -> Bool
overlaps first second =
  (fst first) <= (fst second) && (snd first) >= (fst second)

parseAssignments :: T.Text -> Either String (Assignment, Assignment)
parseAssignments line = do
  (first, rest) <- parsePair line
  let rest2 = T.drop 1 rest
  (second, _) <- parsePair rest2
  return (first, second)

parsePair :: T.Text -> Either String (Assignment, T.Text)
parsePair text = do
  (first, rest) <- TR.decimal text
  let rest2 = T.drop 1 rest
  (second, rest3) <- TR.decimal rest2
  return ((first, second), rest3)
