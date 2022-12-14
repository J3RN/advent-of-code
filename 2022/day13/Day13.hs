import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Printf (printf)

import Data.List (findIndex, sort)
import Data.Maybe (mapMaybe)

type Packet = [Datum]
data Datum
  = I Int
  | L [Datum]
  deriving Show

main :: IO ()
main = do
  contents <- readFile "input"
  packetPairs <- either (fail . show) pure $ runParser parseFile () "input" contents
  let indices = map fst . filter (\ixPair -> inOrder (snd ixPair)) $ zip [1..] packetPairs :: [Int]
  printf "Sum of indices: %d\n" $ sum indices
  let dividers = [(L [L [I 2]]), (L [L [I 6]])]
      myData = map L . flatten $ packetPairs
      sorted = sort (myData ++ dividers)
      dividerIndices = mapMaybe (\x -> findIndex ((==) x) sorted) dividers
  printf "Product of divider indices: %d\n" . product . map (+ 1) $ dividerIndices

flatten :: [(a, a)] -> [a]
flatten pairs =
  reverse . foldl (\acc p -> (fst p):(snd p):acc) [] $ pairs

inOrder :: (Packet, Packet) -> Bool
inOrder (left, right) =
  L left <= L right

instance Eq Datum where
  I a       == I b       = a == b
  I a       == L b       = L [I a] == L b
  L a       == I b       = L a == L [I b]
  L (h1:t1) == L (h2:t2) = h1 == h2 && (L t1 == L t2)
  L []      == L []      = True
  _         == _         = False

instance Ord Datum where
  (<=) (I a)       (I b)       = a <= b
  (<=) a@(I _)     b@(L _)     = (L [a]) <= b
  (<=) a@(L _)     b@(I _)     = a <= (L [b])
  (<=) (L [])      _           = True
  (<=) _           (L [])      = False
  (<=) (L (h1:t1)) (L (h2:t2)) = (h1 < h2) || (h1 == h2 && L t1 <= L t2)

-- This parser should be fun and simple!

parseFile :: Parser [(Packet, Packet)]
parseFile = sepEndBy parsePair (char '\n')

parsePair :: Parser (Packet, Packet)
parsePair = do
  left <- parsePacket
  _ <- char '\n'
  right <- parsePacket
  _ <- char '\n'
  return (left, right)

parsePacket :: Parser Packet
parsePacket = do
  _ <- char '['
  fields <- sepBy parseDatum (char ',')
  _ <- char ']'
  return fields

parseDatum :: Parser Datum
parseDatum = parseList <|> parseInt

parseList :: Parser Datum
parseList = do
  _ <- char '['
  fields <- sepBy parseDatum (char ',')
  _ <- char ']'
  return (L fields)

parseInt :: Parser Datum
parseInt = fmap (I . read) (many1 digit)
