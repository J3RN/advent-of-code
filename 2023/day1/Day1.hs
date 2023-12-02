import Control.Applicative ((<|>), liftA2)

import Data.List (tails)

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

-- Implementation is admittedly a bit fancier than it needs to be
parseDigit :: String -> Maybe Int
parseDigit = liftA2 (<|>) parseLiteralDigit parseWordDigit

parseLiteralDigit :: String -> Maybe Int
parseLiteralDigit str
    | beginsWith str "1"     = Just 1
    | beginsWith str "2"     = Just 2
    | beginsWith str "3"     = Just 3
    | beginsWith str "4"     = Just 4
    | beginsWith str "5"     = Just 5
    | beginsWith str "6"     = Just 6
    | beginsWith str "7"     = Just 7
    | beginsWith str "8"     = Just 8
    | beginsWith str "9"     = Just 9
    | otherwise              = Nothing

parseWordDigit :: String -> Maybe Int
parseWordDigit str
    | beginsWith str "one"   = Just 1
    | beginsWith str "two"   = Just 2
    | beginsWith str "three" = Just 3
    | beginsWith str "four"  = Just 4
    | beginsWith str "five"  = Just 5
    | beginsWith str "six"   = Just 6
    | beginsWith str "seven" = Just 7
    | beginsWith str "eight" = Just 8
    | beginsWith str "nine"  = Just 9
    | otherwise              = Nothing

beginsWith :: String -> String -> Bool
beginsWith base comp =
  take (length comp) base == comp
