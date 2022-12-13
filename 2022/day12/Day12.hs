import Data.Array.IArray
import Data.List (find)
import Data.Maybe (catMaybes, fromJust)

import Text.Printf (printf)

type Row = Int
type Col = Int
type Pos = (Row, Col)
type Path = [Pos]
data Cell = Cell { visited :: Bool
                 , height :: Char
                 }
            deriving Show

type Matrix = Array Pos Cell

main :: IO ()
main = do
  matrix <- fmap parseFile $ readFile "input"
  paths <- maybe (fail "No paths found!") pure $ pathsFrom matrix (startPos matrix) (targetPos matrix)
  printf "Shortest number of steps: %d\n" . minimum . map length $ paths

-- No Parsec today, would be overkill
parseFile :: String -> Matrix
parseFile file =
  let inputLines = lines file
      dims = (length inputLines, length . head $ inputLines)
  -- Again, sorry I'm using '++'
  in listArray ((1, 1), dims) . map (Cell False) $ foldl1 (++) inputLines

pathsFrom :: Matrix -> Pos -> Pos -> Maybe [Path]
pathsFrom matrix start end
  | start == end = Just [[]]
  | otherwise =
    let visitedMatrix = matrix // [(start, (matrix ! start) { visited = True })]
        myCandidates = map fst . candidates visitedMatrix $ start
        subpaths = concat . catMaybes . map (\newStart -> pathsFrom visitedMatrix newStart end) $ myCandidates
    in case subpaths of
      [] -> Nothing
      paths -> Just (map (\p -> start:p) paths)

startPos :: Matrix -> Pos
startPos matrix =
  fromJust . findPos ((==) 'S' . height) $ matrix

targetPos :: Matrix -> Pos
targetPos matrix =
  fromJust . findPos ((==) 'E' . height) $ matrix

findPos :: (Cell -> Bool) -> Matrix -> Maybe Pos
findPos test matrix =
  fmap fst . find (test . snd) . assocs $ matrix

candidates :: Matrix -> Pos -> [(Pos, Cell)]
candidates matrix pos@(row, col) =
  let myVal = matrix ! pos
      viable = filter (withinBounds matrix) [(row - 1, col), (row, col - 1), (row + 1, col), (row, col + 1)]
  in filter (canReach myVal . snd) . map (\cPos -> (cPos, matrix ! cPos)) $ viable

withinBounds :: Matrix -> Pos -> Bool
withinBounds matrix (row, col) =
  let (_, (maxRow, maxCol)) = bounds matrix
  in row >= 1 && col >= 1 && row <= maxRow && col <= maxCol

canReach :: Cell -> Cell -> Bool
canReach _            (Cell True _  ) = False
canReach (Cell _ 'S') (Cell _    'a') = True
canReach (Cell _ 'S') (Cell _    'b') = True
canReach (Cell _ 'S') (Cell _    _  ) = False
canReach (Cell _ 'y') (Cell _    'E') = True
canReach (Cell _ 'z') (Cell _    'E') = True
canReach (Cell _ _  ) (Cell _    'E') = False
canReach (Cell _ a  ) (Cell _    b  ) = b <= succ a
