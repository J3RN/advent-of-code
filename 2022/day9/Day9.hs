import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String (Parser)

import Text.Printf (printf)

import Data.List (intersect, nub)

type Pos = (Int, Int)

data Step
  = R
  | L
  | U
  | D
  deriving Show

main :: IO ()
main = do
  contents <- readFile "input"
  steps <- either (fail . show) pure $ runParser parseFile () "input" contents
  let states = scanl moveList [(0,0), (0,0)] steps
      uniquePos = nub . map (head . reverse) $ states
      numUnique = length uniquePos
  printf "Unique tail positions: %d\n" numUnique
  let intermediateLists = scanl moveList (replicate 10 (0, 0)) steps
      uniquePos' = nub . map (head . reverse) $ intermediateLists
      numUnique' = length uniquePos'
  printf "Unique 10' tail positions: %d\n" numUnique'

-- Surrounding also includes the given point
surrounding :: Pos -> [Pos]
surrounding (row, col) =
  [(r, c) | r <- [row-1..row+1], c <- [col-1..col+1]]

moveKnot :: Pos -> Step -> Pos
moveKnot (r, c) R = (r, c + 1)
moveKnot (r, c) L = (r, c - 1)
moveKnot (r, c) U = (r - 1, c)
moveKnot (r, c) D = (r + 1, c)

diagonals :: Pos -> [Pos]
diagonals (row, col) =
  [(r,c) | r <- [row-1..row+1], c <- [col-1..col+1], (r /= row && c /= col)]

-- Catch the tail up to the head, if necessary
moveTail :: Pos -> Pos -> Pos
moveTail hPos@(hRow, hCol) tPos@(tRow, tCol)
  | hPos `elem` (surrounding tPos) = tPos
  | hRow /= tRow && hCol == tCol =   ((hRow + tRow) `div` 2, hCol)
  | hRow == tRow && hCol /= tCol =   (hRow, (hCol + tCol) `div` 2)
  | otherwise =                      (head ((diagonals tPos) `intersect` (surrounding hPos)))

-- Moves the head using @step@, then iterates the list, using the previous
-- element to determine the new position of the element
moveList :: [Pos] -> Step -> [Pos]
moveList (h:rest) step =
  let newHead = moveKnot h step
  in reverse . foldl movePair [newHead] $ rest
-- Really just here to assuage the compiler
moveList empty _step = empty

movePair :: [Pos] -> Pos -> [Pos]
movePair list e = updatedPos:list
  where updatedPos = moveTail (head list) e

--- Parser
-- Notably, the parser will expand lines such as "R 2" to [R, R]

parseFile :: Parser [Step]
parseFile =
  fmap mconcat (many1 parseLine)

--  This seems contrived, but I can't off-hand think of a better way
parseLine :: Parser [Step]
parseLine = do
  dir <- parseDir
  _ <- char ' '
  n <- many1 digit
  _ <- char '\n'
  return (replicate (read n) dir)

parseDir :: Parser Step
parseDir = tokenPrim (\c -> show [c])
                    (\pos c _cs -> updatePosChar pos c)
                    maybeDir
  where maybeDir 'R' = Just R
        maybeDir 'L' = Just L
        maybeDir 'U' = Just U
        maybeDir 'D' = Just D
        maybeDir _ = Nothing
