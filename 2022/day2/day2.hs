{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Text.Printf
import Data.List(find)
import Data.Maybe(fromJust)

data Outcome = Loss | Draw | Win deriving Eq
data Play = Rock | Paper | Scissors deriving Eq
data Game = Game {theirPlay :: Play, myPlay :: Play, outcome :: Outcome}

games :: [Game]
games = [
           (Game Rock Rock Draw),
           (Game Rock Paper Win),
           (Game Rock Scissors Loss),
           (Game Paper Rock Loss),
           (Game Paper Paper Draw),
           (Game Paper Scissors Win),
           (Game Scissors Rock Win),
           (Game Scissors Paper Loss),
           (Game Scissors Scissors Draw)
           ]

main :: IO ()
main = do
  contents <- fmap (T.lines) $ TIO.readFile "input"
  let scores = map computeScore contents
  printf "Part 1: %d\n" (sum scores)
  let scores' = map computeScore' contents
  printf "Part 2: %d\n" (sum scores')

computeScore :: T.Text -> Integer
computeScore line =
   let (theirs, mine) = splitLine line
       (theirPlay, myPlay) = (computePlay theirs, computePlay mine) in
     (outcomeToScore $ computeOutcome theirPlay myPlay) + (playScore myPlay)

computeScore' :: T.Text -> Integer
computeScore' line =
  let (theirs, mine) = splitLine line
      theirPlay = computePlay theirs
      outcome = computeOutcome' mine
      myPlay = computePlay' theirPlay outcome in
    (outcomeToScore outcome) + (playScore myPlay)

splitLine :: T.Text -> (T.Text, T.Text)
splitLine line =
  let (theirs, mine) = T.splitAt 1 line in
                       (theirs, (T.drop 1 mine))

outcomeToScore :: Outcome -> Integer
outcomeToScore Loss = 0
outcomeToScore Draw = 3
outcomeToScore Win = 6

computeOutcome :: Play -> Play -> Outcome
computeOutcome theirs mine =
  outcome . fromJust . find (\x -> (theirPlay x == theirs && myPlay x == mine)) $ games

computeOutcome' :: T.Text -> Outcome
computeOutcome' "X" = Loss
computeOutcome' "Y" = Draw
computeOutcome' "Z" = Win

computePlay :: T.Text -> Play
computePlay "A" = Rock
computePlay "X" = Rock
computePlay "B" = Paper
computePlay "Y" = Paper
computePlay "C" = Scissors
computePlay "Z" = Scissors

computePlay' :: Play -> Outcome -> Play
computePlay' theirs result =
  myPlay . fromJust . find (\x -> (theirPlay x == theirs && outcome x == result)) $ games

playScore :: Play -> Integer
playScore Rock = 1
playScore Paper = 2
playScore Scissors = 3
