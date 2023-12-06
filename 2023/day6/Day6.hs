import Control.Monad (guard)

import Text.Parsec
import Text.Parsec.String (Parser)

data Record = Record { _time :: Int
                     , _distance :: Int
                     }
            deriving Show

main :: IO ()
main = do
  contents <- readFile "input"
  records <- either (fail . show) pure $ runParser parseRecords () "input" contents
  let bDs = map numBetterDistances records
  print $ product bDs
  record <- either (fail . show) pure $ runParser parseRecord () "input" contents
  print $ numBetterDistances record

numBetterDistances :: Record -> Int
numBetterDistances record =
  let d = (toEnum $ _distance record) :: Double
      t = (toEnum $ _time record) :: Double
      lowerBound = (t - sqrt(t ** 2 - 4 * d)) / 2
      upperBound = (t + sqrt(t ** 2 - 4 * d)) / 2
  in floor upperBound - floor lowerBound

-- Parser

parseRecords :: Parser [Record]
parseRecords = do
  times <- parseTimes
  _ <- char '\n'
  distances <- parseDistances
  return $ map (uncurry Record) $ zip times distances

parseTimes :: Parser [Int]
parseTimes =
  string "Time:" *> many1 (char ' ') *> sepBy (read <$> many1 digit) (many1 (char ' '))

parseDistances :: Parser [Int]
parseDistances =
  string "Distance:" *> many1 (char ' ') *> sepBy (read <$> many1 digit) (many1 (char ' '))

parseRecord :: Parser Record
parseRecord = do
  time <- string "Time:" *> (read . concat <$> many1 (many1 (char ' ') *> many1 digit))
  _ <- char '\n'
  distance <- string "Distance:" *> (read . concat <$> many1 (many1 (char ' ') *> many1 digit))
  return (Record time distance)
