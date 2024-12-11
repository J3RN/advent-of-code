import           Data.Bifunctor   (first)
import           Data.Functor     (($>))
import qualified Data.List        as List
import           Data.Map         (Map, (!?))
import qualified Data.Map         as Map
import           Text.Parsec
import           Text.Parsec.Text (Parser, parseFromFile)

type Point = (Int, Int)
type Height = Int
type TopologyMap = Map Point Height

main :: IO ()
main = do
  topology <- parseFromFile parseFile "input" >>= either (fail . show) pure
  print . sum . trailheadCounts $ topology
  print . sum . trailRatings $ topology

trailheadCounts :: TopologyMap -> [Int]
trailheadCounts topology = map (trailheadCount 1 . fst) $ filter ((== 0) . snd) $ Map.assocs topology
  where trailheadCount :: Height -> Point -> Int
        trailheadCount height point = length $ List.nub $ trailheadsFrom 1 point
        trailheadsFrom :: Height -> Point -> [Point]
        trailheadsFrom 9      point = filter (\neighbor -> topology !? neighbor == Just 9) $ neighbors point
        trailheadsFrom height point = concatMap (trailheadsFrom (height + 1)) $ filter (\neighbor -> topology !? neighbor == Just height) $ neighbors point

trailRatings :: TopologyMap -> [Int]
trailRatings topology = map (trailCount 1 . fst) $ filter ((== 0) . snd) $ Map.assocs topology
  where trailCount :: Height -> Point -> Int
        trailCount height point = length . List.nub $ trails height point
        trails :: Height -> Point -> [[Point]]
        trails 9      point = map ((point:) . List.singleton) $ filter (\neighbor -> topology !? neighbor == Just 9) $ neighbors point
        trails height point = concatMap (map (point:) . trails (height + 1)) $ filter (\neighbor -> topology !? neighbor == Just height) $ neighbors point

neighbors :: Point -> [Point]
neighbors (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

-- Parser (not stateful today, though it could be!)

parseFile :: Parser TopologyMap
parseFile = Map.fromList . concat . zipWith applyRowIx [0..] <$> manyTill parseRow eof
  where applyRowIx rowIx = map (first (,rowIx))

parseRow :: Parser [(Int, Height)]
parseRow = zip [0..] <$> many1 ((read . pure <$> digit) <|> (char '.' $> (-1)))  <* char '\n'
