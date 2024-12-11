import           Data.Bifunctor   (first)
import           Data.Map         (Map)
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

trailheadCounts :: TopologyMap -> [Int]
trailheadCounts topology = map (trailheadCount topology) (Map.assocs topology)
  where trailheadCount = _

-- Parser (not stateful today, though it could be!)

parseFile :: Parser TopologyMap
parseFile = Map.fromList . concat . zipWith applyRowIx [0..] <$> manyTill parseRow eof
  where applyRowIx rowIx = map (first (,rowIx))

parseRow :: Parser [(Int, Height)]
parseRow = zip [0..] <$> many1 (read . pure <$> digit)  <* char '\n'
