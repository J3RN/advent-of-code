import Data.List (find)
import Data.Maybe (fromMaybe)

import Text.Parsec
import Text.Parsec.String (Parser)

data Game = Game { _id :: Int
                 , _turns :: [Turn]
                 }

data Turn = Turn { _redCount :: Int
                 , _blueCount :: Int
                 , _greenCount :: Int
                 }

data Constraint = Constraint { _redLimit :: Int
                             , _blueLimit :: Int
                             , _greenLimit :: Int
                             }

part1Constraint :: Constraint
part1Constraint = Constraint { _redLimit = 12
                             , _greenLimit = 13
                             , _blueLimit = 14
                             }

main :: IO ()
main = do
  contents <- readFile "input"
  games <- either (fail . show) pure $ runParser parseFile () "input" contents
  let possibleGames = filter (possible part1Constraint) games
  print $ sum $ map _id possibleGames
  let mins = map computeMin games
      powers = map computePower mins
  print $ sum powers

possible :: Constraint -> Game -> Bool
possible constraint (Game { _turns = turns })  =
  (maximum $ map _redCount turns) <= _redLimit constraint &&
  (maximum $ map _blueCount turns) <= _blueLimit constraint &&
  (maximum $ map _greenCount turns) <= _greenLimit constraint

computeMin :: Game -> Constraint
computeMin (Game { _turns = turns }) =
  Constraint { _redLimit = maximum $ map _redCount turns
             , _greenLimit = maximum $ map _greenCount turns
             , _blueLimit = maximum $ map _blueCount turns
             }

computePower :: Constraint -> Int
computePower c =
  _redLimit c * _greenLimit c * _blueLimit c

-- Parser

parseFile :: Parser [Game]
parseFile = sepEndBy parseGame (char '\n')

parseGame :: Parser Game
parseGame = do
  _ <- string "Game "
  strId <- many1 digit
  _ <- string ": "
  turns <- parseTurns
  return (Game {_id = (read strId), _turns = turns})

parseTurns :: Parser [Turn]
parseTurns = sepBy parseTurn (string "; ")

data Color = Red | Green | Blue deriving Eq

parseTurn :: Parser Turn
parseTurn = do
  cubes <- sepBy parseCubes (string ", ")
  return Turn { _redCount = findColorCount cubes Red
              , _greenCount = findColorCount cubes Green
              , _blueCount = findColorCount cubes Blue
              }

findColorCount :: [(Color, Int)] -> Color -> Int
findColorCount cubes color =
  fromMaybe 0 $ fmap snd $ find (\x -> fst x == color) cubes

parseCubes :: Parser (Color, Int)
parseCubes = do
  strCount <- many1 digit
  _ <- char ' '
  color <- parseColor
  return (color, read strCount)

parseColor :: Parser Color
parseColor = do
  parseRed <|> parseBlue <|> parseGreen

parseRed :: Parser Color
parseRed = string "red" *> return Red

parseGreen :: Parser Color
parseGreen = string "green" *> return Green

parseBlue :: Parser Color
parseBlue = string "blue" *> return Blue
