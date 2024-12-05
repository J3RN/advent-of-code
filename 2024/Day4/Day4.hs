import           Data.Map (Map, (!?))
import qualified Data.Map as Map

type Puzzle = Map (Int, Int) Char

main :: IO ()
main = do
  puzzle <- parseFile "input"
  print $ sum (map (xmasCount puzzle) (Map.keys puzzle))
  print $ length (filter (isXMas puzzle) (Map.keys puzzle))

parseFile :: FilePath -> IO Puzzle
parseFile fname = do
  raw <- readFile fname
  let rows = lines raw
  return . Map.fromList . concatMap (\(row, rowIx) -> zipWith (\char colIx -> ((colIx, rowIx), char)) row [1..]) $ zip rows [1..]

-- Return count of "XMAS" starting from given coord
-- General approach: From each point, check all directions to see if it's "XMAS"
--                   8 checks: up down left right up-right down-right down-left down-right
xmasCount :: Puzzle -> (Int, Int) -> Int
xmasCount puzzle (x, y) =
  length (filter isXmas paths)
  where
    isXmas = (== [Just 'X', Just 'M', Just 'A', Just 'S']) . map (puzzle !?)
    -- There's probably a more intelligent way to generate these
    paths = [ [(x, y), (x+1, y-1), (x+2, y-2), (x+3, y-3)]
            , [(x, y), (x+1, y),   (x+2, y),   (x+3, y)]
            , [(x, y), (x+1, y+1), (x+2, y+2), (x+3, y+3)]
            , [(x, y), (x,   y+1), (x,   y+2), (x,   y+3)]
            , [(x, y), (x-1, y+1), (x-2, y+2), (x-3, y+3)]
            , [(x, y), (x-1, y),   (x-2, y),   (x-3, y)]
            , [(x, y), (x-1, y-1), (x-2, y-2), (x-3, y-3)]
            , [(x, y), (x,   y-1), (x,   y-2), (x,   y-3)]]

-- "Is coord the top-left of an X-MAS?"
isXMas :: Puzzle -> (Int, Int) -> Bool
isXMas puzzle (x, y) =
  let (c1, c2) = xCoords
  in isMas (map (puzzle !?) c1) && isMas (map (puzzle !?) c2)
  where
    isMas res = res == [Just 'M', Just 'A', Just 'S'] || res == [Just 'S', Just 'A', Just 'M']
    xCoords = ([(x, y), (x+1, y+1), (x+2, y+2)], [(x+2, y), (x+1, y+1), (x, y+2)])
