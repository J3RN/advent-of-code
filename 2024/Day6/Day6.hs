import           Control.Monad  (void)
import           Data.Bifunctor (first)
import           Data.List      (nub)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.Text      (Text)
import qualified Data.Text.IO   as T
import           Text.Parsec

type Point = (Int, Int)

data Direction = U | R | L | D
  deriving (Show, Eq)

main :: IO ()
main = do
  (bounds, start, obstacles) <- parseFromFile parseFile "input" >>= either (fail . show) pure
  -- print (bounds, start, obstacles)
  print . length . nub $ path bounds start U obstacles

path :: (Int, Int) -> Point -> Direction -> Set Point -> [Point]
path = path' []

path' :: [Point] -> (Int, Int) -> Point -> Direction -> Set Point -> [Point]
path' p dims@(width, height) pos dir obstacles
  | fst pos < 0 || snd pos < 0 || fst pos >= width || snd pos >= height = p
  | otherwise = let next = nextPos pos dir
                 in if next `Set.member` obstacles
                    then path' p dims pos (turn dir) obstacles
                    else path' (pos:p) dims next dir obstacles

nextPos :: Point -> Direction -> Point
nextPos (x, y) U = (x, y - 1)
nextPos (x, y) R = (x + 1, y)
nextPos (x, y) D = (x, y + 1)
nextPos (x, y) L = (x - 1, y)

turn :: Direction -> Direction
turn U = R
turn R = D
turn D = L
turn L = U

-- A stateful parser? 😱

data ParserState = ParserState { _obstacles :: Set Point
                               , _start     :: Point
                               , _width     :: Int
                               , _row       :: Int
                               , _col       :: Int
                               }

type Parser = Parsec Text ParserState

initialState :: ParserState
initialState = ParserState { _obstacles = Set.empty
                           , _start = (0, 0)
                           , _width = 0
                           , _row = 0
                           , _col = 0
                           }

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- T.readFile fname
         return (runP p initialState fname input)

parseFile :: Parser ((Int, Int), Point, Set Point)
parseFile = do
  manyTill (parseRow >> char '\n' >> updateState (\s -> s{_row = _row s + 1, _width = _col s, _col = 0})) eof
    >> ((\s -> ((_width s, _row s), _start s, _obstacles s)) <$> getState)

parseRow :: Parser ()
parseRow = void (many1 (parseCell >> updateState (\s -> s{_col = _col s + 1})))

parseCell :: Parser ()
parseCell = parseStart <|> parseObstacle <|> void (char '.')

parseObstacle :: Parser ()
parseObstacle = char '#' >> updateState (\s -> s {_obstacles = Set.insert (_col s, _row s) (_obstacles s)})

parseStart :: Parser ()
parseStart = char '^' >> updateState (\s -> s{_start = (_col s, _row s)})
