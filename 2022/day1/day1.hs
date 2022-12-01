{-# LANGUAGE OverloadedStrings #-}

-- Using Data.Text instead of Data.String just in case the input is _very_ large.
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Either
import Data.List

main :: IO ()
main = do
  rawInput <- TIO.readFile "input"
  let groups = groupInput rawInput
  let parsedInput = castStrings groups
  putStrLn . show . maximum . map sum $ parsedInput
  putStrLn . show . sum . take 3 . sortBy (flip compare) . map sum $ parsedInput

castStrings :: [[T.Text]] -> [[Integer]]
castStrings groups = map (map textToInt) groups

-- If text is unable to be parsed to an integer, it will be parsed as '0'
textToInt :: T.Text -> Integer
textToInt text =
  fst $ fromRight (0, "") (TR.decimal text)

groupInput :: T.Text -> [[T.Text]]
groupInput input =
  let groups = T.splitOn "\n\n" input in
    map (T.splitOn "\n") groups
