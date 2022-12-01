{-# LANGUAGE OverloadedStrings #-}

-- Using Data.Text instead of Data.String just in case the input is _very_ large.
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Either
import Data.List
import Text.Printf

main :: IO ()
main = do
  rawInput <- TIO.readFile "input"
  let groups = groupInput rawInput
  let parsedInput = parseStrings groups
  let sums = map sum parsedInput
  printf "Max: %d\n" . maximum $ sums
  printf "Sum of top 3: %d\n" . sum . take 3 . sortBy (flip compare) $ sums

parseStrings :: [[T.Text]] -> [[Integer]]
parseStrings groups = map (map textToInt) groups

-- If text is unable to be parsed to an integer, it will be parsed as '0'
textToInt :: T.Text -> Integer
textToInt text =
  fst $ fromRight (0, "") (TR.decimal text)

groupInput :: T.Text -> [[T.Text]]
groupInput input =
  let groups = T.splitOn "\n\n" input in
    map (T.splitOn "\n") groups
