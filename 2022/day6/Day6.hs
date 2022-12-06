-- Today I'm just going to use plain ole [Char] Strings because I'm lazy

-- Part 1: Report index of first message character
-- mjqjpqmgbljsphdztnvjfqwrcgsmlb
-- 01234567
-- 7 (g) is the first character of the message

import Data.Maybe (maybe)
import Data.List (find, nub, tails)

import Text.Printf (printf)

main :: IO ()
main = do
  contents <- readFile "input"
  let wins = windowsOf 4 contents
  start <- maybe (fail "Could not find start sequnce!") pure $ startSequence wins
  printf "First index of message: %d\n" (fst start)
  let wins' = windowsOf 14 contents
  start' <- maybe (fail "Could not find start sequence!") pure $ startSequence wins'
  printf "First index of message: %d\n" (fst start')

windowsOf :: Int -> [a] -> [(Int, [a])]
windowsOf num l =
  zip [num..] . map (take num) . filter ((>= num) . length) . tails $ l

startSequence :: Eq a => [(Int, [a])] -> Maybe (Int, [a])
startSequence sequences =
  find (uniqueChars . snd) sequences
  where uniqueChars x = nub x == x
