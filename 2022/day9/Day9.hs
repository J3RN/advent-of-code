import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String (Parser)

type Pos = (Int, Int)

data BoardState = BoardState { headPos :: Pos
                             , tailPos :: Pos
                             }

data Step
  = R
  | L
  | U
  | D
  deriving Show

surroundingEight :: Pos -> [Pos]
surroundingEight (row, col) =
  [(r, c) | r <- [row-1..row+1], c <- [col-1..col+1], (r /= row || c /= col)]

isMoveRequired :: Pos -> Pos -> Bool
isMoveRequired headPos tailPos =
  if headPos `elem` (surroundingEight tailPos)
  then False
  else True

moveHead :: BoardState -> Step -> BoardState
moveHead (BoardState headPos tailPos) step =
  (BoardState (move headPos step) tailPos)
  where move (r, c) R = (r, c + 1)
        move (r, c) L = (r, c - 1)
        move (r, c) U = (r - 1, c)
        move (r, c) D = (r + 1, c)

-- Catch the tail up to the head, if necessary
moveTail :: BoardState -> BoardState
moveTail (headPos@(hRow, hCol), tailPos@(tRow, tCol)) =
  

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
