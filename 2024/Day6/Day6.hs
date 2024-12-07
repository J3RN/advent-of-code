import           Control.Monad  (void)
import           Data.Bifunctor (first)
import           Data.List      (nub)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.Text      (Text)
import qualified Data.Text.IO   as T
import           Text.Parsec

type Point = (Int, Int)

data Direction = U | R | D | L
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  (bounds, start, obstacles) <- parseFromFile parseFile "input" >>= either (fail . show) pure
  let p = path bounds start U obstacles
  print . length $ p
  print . length . Set.filter (isInfinitePath bounds start U . flip Set.insert obstacles) $ p

isInfinitePath :: (Int, Int) -> Point -> Direction -> Set Point -> Bool
isInfinitePath = isInfinitePath' Set.empty

isInfinitePath' :: Set (Direction, Point) -> (Int, Int) -> Point -> Direction -> Set Point -> Bool
isInfinitePath' p dims@(width, height) pos dir obstacles
  | fst pos < 0 || snd pos < 0 || fst pos >= width || snd pos >= height = False
  | (dir, pos) `Set.member` p = True
  | otherwise = let next = nextPos pos dir
                 in if next `Set.member` obstacles
                    then isInfinitePath' (Set.insert (dir, pos) p) dims pos (turn dir) obstacles
                    else isInfinitePath' (Set.insert (dir, pos) p) dims next dir obstacles


path :: (Int, Int) -> Point -> Direction -> Set Point -> Set Point
path = path' Set.empty

path' :: Set Point -> (Int, Int) -> Point -> Direction -> Set Point -> Set Point
path' p dims@(width, height) pos dir obstacles
  | fst pos < 0 || snd pos < 0 || fst pos >= width || snd pos >= height = p
  | otherwise = let next = nextPos pos dir
                 in if next `Set.member` obstacles
                    then path' p dims pos (turn dir) obstacles
                    else path' (Set.insert pos p) dims next dir obstacles

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
