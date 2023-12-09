import Data.Functor (($>))
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.String (Parser)

data Direction = L | R
               deriving Show

type Location = String
type DesertMap = Map Location (Location, Location)

data State = State { _currentLocation :: Location
                   , _directions      :: [Direction]
                   , _count           :: Int
                   }

main :: IO ()
main = do
  contents <- readFile "input"
  (directions, desertMap) <- either (fail . show) pure $ runParser parseFile () "input" contents
  print $ part1 desertMap directions (== "ZZZ") "AAA"
  let startingPoints = (filter ("A" `isSuffixOf`) (Map.keys desertMap))
      -- I made a number of assumptions that were coincidentally correct.
      loopTimes = map (part1 desertMap directions ("Z" `isSuffixOf`)) startingPoints
  print $ foldl1 lcm loopTimes

part1 :: DesertMap -> [Direction] -> (Location -> Bool) -> Location -> Int
part1 desertMap directions check location =
  let initialState = (Main.State { _currentLocation = location, _directions = cycle directions, _count = 0 })
      finalState   = until (check . _currentLocation) (walk desertMap) initialState
  in _count finalState

walk :: DesertMap -> Main.State -> Main.State
walk desertMap state =
  let dir:rest = _directions state
      next = nextLoc desertMap dir (_currentLocation state)
  in (Main.State {_currentLocation = next, _directions = rest, _count = _count state + 1})

nextLoc :: DesertMap -> Direction -> Location -> Location
nextLoc desertMap dir here =
  let (l, r) = desertMap Map.! here
  in case dir of
       L -> l
       R -> r

-- Parser

parseFile :: Parser ([Direction], DesertMap)
parseFile = do
  directions <- parseDirections
  _ <- string "\n\n"
  desertMap <- parseDesertMap
  return (directions, desertMap)

parseDirections :: Parser [Direction]
parseDirections = many1 (parseLeft <|> parseRight)

parseLeft :: Parser Direction
parseLeft = char 'L' $> L

parseRight :: Parser Direction
parseRight = char 'R' $> R

parseDesertMap :: Parser DesertMap
parseDesertMap = Map.fromList <$> sepEndBy parseDesertRow (char '\n')

parseDesertRow :: Parser (Location, (Location, Location))
parseDesertRow = do
  here <- count 3 alphaNum
  _ <- string " = ("
  left <- count 3 alphaNum
  _ <- string ", "
  right <- count 3 alphaNum
  _ <- string ")"
  return (here, (left, right))
