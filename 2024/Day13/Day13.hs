import qualified Data.List        as List
import           Text.Parsec
import           Text.Parsec.Text (Parser, parseFromFile)

import           Debug.Trace      (traceShowId)

type Point = (Integer, Integer)
type Transform = (Integer, Integer)
data Button = A | B
type Solution = (Integer, Integer)
data Machine = Machine { _a     :: Transform
                       , _b     :: Transform
                       , _prize :: Point
                       } deriving (Show)

main :: IO ()
main = do
  machines <- parseFromFile parseFile "input" >>= either (fail . show) pure
  print . sum . map minCostWin $ machines
  print . sum . map (minCostWin . adjust) $ machines
  where adjust m@(Machine {_prize = (prizeX, prizeY)}) = m{_prize = (prizeX + 10000000000000, prizeY + 10000000000000)}

minCostWin :: Machine -> Integer
minCostWin = maybeMin . map tabulateCost . solutions
  where maybeMin [] = 0
        maybeMin l  = List.minimum l

solutions :: Machine -> [Solution]
solutions (Machine { _a = (aXT, aYT), _b = (bXT, bYT), _prize = (prizeX, prizeY)}) =
  let det = aXT * bYT - aYT *  bXT
      nA = (bYT * prizeX - bXT * prizeY) `div` det
      nB = (-(aYT * prizeX) + aXT * prizeY) `div` det
  in if det == 0
            || nA * aXT + nB * bXT /= prizeX
            || nA * aYT + nB * bYT /= prizeY
     then []
     else [(nA, nB)]

tabulateCost :: Solution -> Integer
tabulateCost (aPresses, bPresses) = aPresses * 3 + bPresses

--

parseFile :: Parser [Machine]
parseFile = parseMachine `sepBy` char '\n' <* eof

parseMachine :: Parser Machine
parseMachine = do
  aButtonT <- parseButton 'A'
  bButtonT <- parseButton 'B'
  prizeLoc <- parsePrize
  return Machine {_a = aButtonT, _b = bButtonT, _prize = prizeLoc}

parseButton :: Char -> Parser Transform
parseButton c = do
  _ <- string "Button "
  _ <- satisfy (== c)
  _ <- string ": X+"
  xt <- read <$> many1 digit
  _ <- string ", Y+"
  yt <- read <$> many1 digit
  _ <- char '\n'
  return (xt, yt)

parsePrize :: Parser Point
parsePrize = do
  _ <- string "Prize: X="
  x <- read <$> many1 digit
  _ <- string ", Y="
  y <- read <$> many1 digit
  _ <- char '\n'
  return (x, y)
