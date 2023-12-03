import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.List (groupBy)
import Data.Maybe (mapMaybe)

type Pos = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "input"
  let matrix = lines contents
      indexedMatrix = indexMatrix matrix
      symbols = extractSymbols indexedMatrix
      numbers = extractNumbers indexedMatrix
  print $ sum $ validPartIds symbols numbers
  print $ sum $ gearRatios symbols numbers

indexMatrix :: [[Char]] -> [(Char, Pos)]
indexMatrix m = concat $ zipWith (\l y -> zipWith (\c x -> (c, (y, x))) l [1 ..]) m [1 ..]

extractSymbols :: [(Char, Pos)] -> [(Char, Pos)]
extractSymbols = filter (\(c, _pos) -> isSymbol c)

isSymbol :: Char -> Bool
isSymbol c = (c /= '.') && not (isDigit c)

extractNumbers :: [(Char, Pos)] -> [(Int, [Pos])]
extractNumbers = collateChunks . numChunks
  where collateChunks = map (\chunk -> (read $ map fst chunk, map snd chunk))

-- Holy point-free Batman
numChunks :: [(Char, Pos)] -> [[(Char, Pos)]]
numChunks = filter (isDigit . fst . head) . groupBy (\x y -> membership x == membership y)
  where membership = bimap isDigit fst

validPartIds :: [(Char, Pos)] -> [(Int, [Pos])] -> [Int]
validPartIds symbols numbers = map fst $ filter (valid symbols) numbers
  where valid symbols' number = [] /= filter (uncurry near) [(sPos, numPos) | sPos <- map snd symbols', numPos <- snd number]

gearRatios :: [(Char, Pos)] -> [(Int, [Pos])] -> [Int]
gearRatios symbols numbers = mapMaybe (gearRatio numbers) symbols

gearRatio :: [(Int, [Pos])] -> (Char, Pos) -> Maybe Int
gearRatio numbers ('*', sPos) =
  case filter (any (near sPos) . snd) numbers of
    [n1, n2] -> Just (fst n1 * fst n2)
    _        -> Nothing
gearRatio _ _ = Nothing

near :: Pos -> Pos -> Bool
near pos1 pos2 = distance (fst pos1) (fst pos2) <= 1 && distance (snd pos1) (snd pos2) <= 1

distance :: Int -> Int -> Int
distance x y = abs (x - y)
