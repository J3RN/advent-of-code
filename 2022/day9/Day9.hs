import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String (Parser)

import Text.Printf (printf)

import Data.List (intersect, nub)

import Debug.Trace (trace)

type Pos = (Int, Int)

data PairState = PairState { headPos :: Pos
                           , tailPos :: Pos
                           }

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
  let states = scanl applyStep (PairState {headPos = (0, 0), tailPos = (0, 0)}) steps
      uniquePos = nub . map tailPos $ states
      numUnique = length uniquePos
  printf "Unique tail poses: %d\n" numUnique

applyStep :: PairState -> Step -> PairState
applyStep state step =
  moveTail (moveHead state step)

-- Surrounding also includes the given point
surrounding :: Pos -> [Pos]
surrounding (row, col) =
  [(r, c) | r <- [row-1..row+1], c <- [col-1..col+1]]

moveHead :: PairState -> Step -> PairState
moveHead (PairState hPos tPos) step =
  (PairState (moveKnot hPos step) tPos)

moveKnot :: Pos -> Step -> Pos
moveKnot (r, c) R = (r, c + 1)
moveKnot (r, c) L = (r, c - 1)
moveKnot (r, c) U = (r - 1, c)
moveKnot (r, c) D = (r + 1, c)

diagonals :: Pos -> [Pos]
diagonals (row, col) =
  [(r,c) | r <- [row-1..row+1], c <- [col-1..col+1], (r /= row && c /= col)]

-- Catch the tail up to the head, if necessary
moveTail :: PairState -> PairState
moveTail (PairState hPos@(hRow, hCol) tPos@(tRow, tCol))
  | hPos `elem` (surrounding tPos) = PairState hPos tPos
  | hRow /= tRow && hCol == tCol = PairState hPos ((hRow + tRow) `div` 2, hCol)
  | hRow == tRow && hCol /= tCol = PairState hPos (hRow, (hCol + tCol) `div` 2)
  | otherwise = PairState hPos (head ((diagonals tPos) `intersect` (surrounding hPos)))

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
