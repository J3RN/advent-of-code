{-# LANGUAGE OverloadedStrings #-}
import           Data.Function    (on)
import qualified Data.List        as List
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Debug.Trace      (traceShowId)
import           Text.Parsec      (Parsec, digit, eof, many, many1, manyTill,
                                   space, string)
import           Text.Parsec.Text (parseFromFile)

main :: IO ()
main = do
  listsRes <- parseFromFile parseFile "input"
  lists <- either (fail.show) return listsRes
  print (listDifferences lists)
  print (listSimilarity lists)

listDifferences :: ([Int], [Int]) -> Int
listDifferences = sum . uncurry (zipWith (\a b -> abs (a - b)) `on` List.sort)

listSimilarity :: ([Int], [Int]) -> Int
listSimilarity (a, b) = sum $ map (itemScore b) a
 where itemScore l i = i * length (filter (== i) l)

parseFile :: Parsec Text () ([Int], [Int])
parseFile = do
  rows <- manyTill parseRow eof
  return (unzip rows)

parseRow :: Parsec Text () (Int, Int)
parseRow = do
  l <- many1 digit
  many1 space
  r <- many1 digit
  many space
  return (read l, read r)
