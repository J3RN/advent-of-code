import           Control.Monad  (void)
import           Data.Bifunctor (bimap, second)
import qualified Data.List      as List
import qualified Data.Map       as Map
import           Data.Text      (Text)
import qualified Data.Text.IO   as T
import           Debug.Trace
import           Text.Parsec
-- import Text.Parsec.Text

type Point = (Int, Int)
type Antenna = (Char, Point)
type Bounds = (Int, Int)

-- I'm assuming Manhattan distance for the distance calculation
-- (δx, δy)
type Distance = (Int, Int)

main :: IO ()
main = do
  (bounds, antennas) <- parseFromFile parseFile "input" >>= either (fail.show) pure
  let anntennaGroups = Map.toList . Map.fromListWith (++) $ map (second List.singleton) antennas
  print . length . filter (isInsideBounds bounds) . List.nub . concatMap (antinodes . snd) $ anntennaGroups
  print . length . List.nub . concatMap (harmonicAntinodes bounds . snd) $ anntennaGroups

isInsideBounds :: Bounds -> Point -> Bool
isInsideBounds (maxX, maxY) (x, y) = x >= 0 && y >= 0 && x < maxX && y < maxY

antinodes :: [Point] -> [Point]
antinodes antennas = concatMap (uncurry antinodes') antennaPairs
  where antennaPairs = combinations antennas

antinodes' :: Point -> Point -> [Point]
antinodes' point1 point2 =
  let (δx, δy) = distance point1 point2
  in [translate point1 (-δx, -δy), translate point2 (δx, δy)]

harmonicAntinodes :: Bounds -> [Point] -> [Point]
harmonicAntinodes bounds antennas = concatMap (uncurry (harmonicAntinodes' bounds)) antennaPairs
  where antennaPairs = combinations antennas

harmonicAntinodes' :: Bounds -> Point -> Point -> [Point]
harmonicAntinodes' bounds point1 point2 =
  let (δx, δy) = distance point1 point2
  in point1:(takeWhile (isInsideBounds bounds) (iterate (translate (δx, δy)) point1)
             ++ takeWhile (isInsideBounds bounds) (iterate (translate (-δx, -δy)) point1))

distance :: Point -> Point -> Distance
distance (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

translate :: Point -> Distance -> Point
translate (x, y) (δx, δy) = (x + δx, y + δy)

-- This is n^2 time; *not* fast
-- Excludes (self, self); but would be trivial to add
combinations :: [a] -> [(a, a)]
combinations []     = []
combinations (x:xs) = map (x,) xs ++ combinations xs

-- ANOTHER stateful parser? 😱

data ParserState = ParserState { _antennas :: [Antenna]
                               , _width    :: Int
                               , _row      :: Int
                               , _col      :: Int
                               }

type Parser = Parsec Text ParserState

initialState :: ParserState
initialState = ParserState { _antennas = []
                           , _width = 0
                           , _row = 0
                           , _col = 0
                           }

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- T.readFile fname
         return (runP p initialState fname input)

parseFile :: Parser ((Int, Int), [Antenna])
parseFile = do
  manyTill (parseRow >> char '\n' >> updateState (\s -> s{_row = _row s + 1, _width = _col s, _col = 0})) eof
    >> ((\s -> ((_width s, _row s), _antennas s)) <$> getState)

parseRow :: Parser ()
parseRow = void (many1 (parseCell >> updateState (\s -> s{_col = _col s + 1})))

parseCell :: Parser ()
parseCell = void (char '.') <|> parseAntenna

parseAntenna :: Parser ()
parseAntenna = satisfy (/= '\n') >>= (\c ->  updateState (\s -> s {_antennas = (c, (_col s, _row s)):_antennas s}))
