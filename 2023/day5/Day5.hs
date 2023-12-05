import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)

import Text.Parsec
import Text.Parsec.String (Parser)

data Mapping = Mapping { _source :: Int,
                         _destination :: Int,
                         _range :: Int
                       }
             deriving Show

main :: IO ()
main = do
  contents <- readFile "input"
  (seeds, mappings) <- either (fail . show) pure $ runParser parseFile () "input" contents
  let minLoc = minimum $ foldl applyMappings seeds mappings
  print minLoc
  let seedRanges = makePairs seeds
  let minLoc' = minimum $ map fst $ foldl applyMappings' seedRanges mappings
  print minLoc'

defaultMapping :: Int -> Mapping
defaultMapping x = Mapping {_source = x, _destination = x, _range = 1}

findMapping :: Int -> [Mapping] -> Mapping
findMapping source mappings =
  fromMaybe (defaultMapping source) $ find (validMapping source) mappings

validMapping :: Int -> Mapping -> Bool
validMapping source mapping =
  source >= _source mapping && source < (_source mapping + _range mapping)

applyMappings :: [Int] -> [Mapping] -> [Int]
applyMappings seeds mappings =
  map (\source -> applyMapping source $ findMapping source mappings) seeds

applyMapping :: Int -> Mapping -> Int
applyMapping source mapping =
  _destination mapping + (source - _source mapping)

-- If there's an odd number of elements, the last one is truncated off.
makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs [_] = []
makePairs (x:y:rest) = (x, y) : makePairs rest

applyMappings' :: [(Int, Int)] -> [Mapping] -> [(Int, Int)]
applyMappings' seedRanges mappings =
  concatMap (mapMappings mappings) seedRanges
  where mapMappings ms seedRange =
          let (mappedRanges, unmatchedRanges) = foldl matchAndApply ([], [seedRange]) ms
          in mappedRanges ++ unmatchedRanges

matchAndApply :: ([(Int, Int)], [(Int, Int)]) -> Mapping -> ([(Int, Int)], [(Int, Int)])
matchAndApply (matched, unmatched) mapping =
  let (overlaps', unoverlapped) = overlaps mapping unmatched
  in (matched ++ map (applyMapping' mapping) overlaps', unoverlapped)

overlaps :: Mapping -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
overlaps mapping unmatcheds =
  let results = map (overlap mapping) unmatcheds
  in (mapMaybe fst results, concatMap snd results)

-- In the return value, fst is the portion of the range within the mapping, snd
-- are the portions of the range outside the mapping
overlap :: Mapping -> (Int, Int) -> (Maybe (Int, Int), [(Int, Int)])
overlap mapping@(Mapping {_source = mapStart, _range = r}) range@(rStart, rLen)
  | rEnd < mapStart || rStart > mapEnd = (Nothing, [range])
  | otherwise =
      let outsideStart = [(rStart, matchStart - rStart) | rStart < _source mapping]
          matched = (matchStart, matchEnd - matchStart + 1)
          outsideEnd = [(matchEnd + 1, rEnd - matchEnd) | rEnd > mapEnd]
      in (Just matched, outsideStart ++ outsideEnd)
  where rEnd = rStart + rLen - 1
        mapEnd = mapStart + r - 1
        matchStart = max rStart mapStart
        matchEnd = min rEnd mapEnd

applyMapping' :: Mapping -> (Int, Int) -> (Int, Int)
applyMapping' (Mapping {_source = source, _destination = dest}) (start, len) =
  (start + (dest - source), len)

-- Parser

parseFile :: Parser ([Int], [[Mapping]])
parseFile = do
  seeds <- parseSeeds
  _ <- string "\n\n"
  mappings <- sepBy parseMappings (char '\n')
  return (seeds, mappings)

parseSeeds :: Parser [Int]
parseSeeds = string "seeds: " *> sepBy (read <$> many1 digit) (char ' ')

parseMappings :: Parser [Mapping]
parseMappings = do
  _ <- manyTill anyToken (char '\n')
  sepEndBy parseMapping (char '\n')

parseMapping :: Parser Mapping
parseMapping = do
  [destStart, sourceStart, range] <- sepBy (read <$> many1 digit) (char ' ')
  return Mapping { _source = sourceStart, _destination = destStart, _range = range }
