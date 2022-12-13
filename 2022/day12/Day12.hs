import Data.Array.IArray
import Data.List (find)
import Data.Maybe (fromJust)

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
  path <- maybe (fail "No paths found!") pure $ pathFrom matrix [[startPos matrix]] (targetPos matrix)
  printf "Shortest number of steps: %d\n" . length $ path

-- No Parsec today, would be overkill
parseFile :: String -> Matrix
parseFile file =
  let inputLines = lines file
      dims = (length inputLines, length . head $ inputLines)
  -- Again, sorry I'm using '++'
  in listArray ((1, 1), dims) . map (Cell False) $ foldl1 (++) inputLines

pathFrom :: Matrix -> [Path] -> Pos -> Maybe Path
pathFrom matrix paths end =
  let (updatedMatrix, newPaths) = foldl progress (matrix, []) paths
  in case find ((==) end . head) newPaths of
    Just (_end:path) -> Just path
    Nothing          -> pathFrom updatedMatrix newPaths end
    -- I don't like exhaustiveness some days
    -- Usually that means there's a better way to write something, but 🤷
    Just []          -> Just []

-- Terrible name, I know, but it used to be named 'foobar' so give me some
-- credit.  'progress' finds the new candidates for the next iteration and marks
-- them as visited in the matrix.  The reason this is done in one function
-- instead of two is that the candidates are marked in the matrix as they're
-- discovered such that the next iteration won't enqueue the same cells.
progress :: (Matrix, [Path]) -> Path -> (Matrix, [Path])
progress (matrix, newPaths) path =
  let myCandidates = map fst $ candidates matrix (head path)
      updatedMatrix = foldl (markVisited) matrix myCandidates
      myNewPaths = map (\c -> c:path) myCandidates
  in (updatedMatrix, myNewPaths ++ newPaths)

markVisited :: Matrix -> Pos -> Matrix
markVisited matrix pos =
  let oldVal = matrix ! pos
  in matrix // [(pos, oldVal {visited = True})]

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
