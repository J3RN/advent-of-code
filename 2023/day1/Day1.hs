import Control.Applicative ((<|>))

import Data.List (isPrefixOf, tails)

main :: IO ()
main = do
  contents <- readFile "input"
  let nums = map numFromStr $ lines contents
  print $ sum nums

numFromStr :: String -> Int
numFromStr s = let digits = digitsFromStr s
               -- Implicit assumption that there is at least 1 digit
               in head digits * 10 + last digits

digitsFromStr :: String -> [Int]
digitsFromStr s =
  foldr extractDigit [] (tails s)
  where extractDigit tl acc =
          case parseDigit tl of
            Just digit -> digit:acc
            Nothing -> acc

parseDigit :: String -> Maybe Int
parseDigit str = (parseLiteralDigit str) <|> (parseWordDigit str)

parseLiteralDigit :: String -> Maybe Int
parseLiteralDigit str
    |  "1" `isPrefixOf` str  = Just 1
    |  "2" `isPrefixOf` str  = Just 2
    |  "3" `isPrefixOf` str  = Just 3
    |  "4" `isPrefixOf` str  = Just 4
    |  "5" `isPrefixOf` str  = Just 5
    |  "6" `isPrefixOf` str  = Just 6
    |  "7" `isPrefixOf` str  = Just 7
    |  "8" `isPrefixOf` str  = Just 8
    |  "9" `isPrefixOf` str  = Just 9
    | otherwise              = Nothing

parseWordDigit :: String -> Maybe Int
parseWordDigit str
    | "one"   `isPrefixOf` str = Just 1
    | "two"   `isPrefixOf` str = Just 2
    | "three" `isPrefixOf` str = Just 3
    | "four"  `isPrefixOf` str = Just 4
    | "five"  `isPrefixOf` str = Just 5
    | "six"   `isPrefixOf` str = Just 6
    | "seven" `isPrefixOf` str = Just 7
    | "eight" `isPrefixOf` str = Just 8
    | "nine"  `isPrefixOf` str = Just 9
    | otherwise                = Nothing
