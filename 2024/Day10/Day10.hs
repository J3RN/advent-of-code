import           Control.Applicative (liftA2)
import           Data.Bifunctor      (first)
import           Data.Functor        (($>))
import qualified Data.List           as List
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import           Data.Maybe          (mapMaybe)
import           Text.Parsec
import           Text.Parsec.Text    (Parser, parseFromFile)

type Point = (Int, Int)
type Height = Int
type TopologyMap = Map Point Height

main :: IO ()
main = do
  topology <- parseFromFile parseFile "input" >>= either (fail . show) pure
  print . sum . trailheadCounts $ topology
  print . sum . trailRatings $ topology

trailheadCounts :: TopologyMap -> [Int]
trailheadCounts topology = map trailheadCount $ filter ((== 0) . snd) $ Map.assocs topology
  where trailheadCount :: (Point, Height) -> Int
        trailheadCount = length . List.nub . trailheadsFrom
        trailheadsFrom :: (Point, Height) -> [Point]
        trailheadsFrom (point, 9)      = [point]
        trailheadsFrom (point, height) = concatMap trailheadsFrom . filter ((== height + 1) . snd) . mapMaybe (liftA2 (<$>) (,) (topology !?)) . neighbors $ point

trailRatings :: TopologyMap -> [Int]
trailRatings topology = map trailCount $ filter ((== 0) . snd) $ Map.assocs topology
  where trailCount :: (Point, Height) -> Int
        trailCount = length . List.nub . trails
        trails :: (Point, Height) -> [[Point]]
        trails (point, 9)      = [[point]]
        trails (point, height) = concatMap (map (point:) . trails) . filter ((== height + 1) . snd) . mapMaybe (\n -> (n,) <$> topology !? n) . neighbors $ point

neighbors :: Point -> [Point]
neighbors (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

-- Parser (not stateful today, though it could be!)

parseFile :: Parser TopologyMap
parseFile = Map.fromList . concat . zipWith applyRowIx [0..] <$> manyTill parseRow eof
  where applyRowIx rowIx = map (first (,rowIx))

parseRow :: Parser [(Int, Height)]
parseRow = zip [0..] <$> many1 ((read . pure <$> digit) <|> (char '.' $> (-1)))  <* char '\n'
