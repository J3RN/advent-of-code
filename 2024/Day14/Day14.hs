import           Control.Monad.State
import           Data.List           (intercalate)
import qualified Data.Set            as Set
import           Text.Parsec         (char, digit, eof, many1, manyTill, option,
                                      satisfy, string)
import           Text.Parsec.Text    (Parser, parseFromFile)

type Point = (Int, Int)
type Velocity = (Int, Int)
data Robot = Robot { _pos :: Point
                   , _v   :: Velocity
                   } deriving (Show)

xLimit = 101
yLimit = 103

main :: IO ()
main = do
  robots <- parseFromFile parseFile "input" >>= either (fail . show) pure
  print . quadrantProd . nTimes 100 (map translateRobot) $ robots
  searchForTree robots 0


searchForTree :: [Robot] -> Int -> IO ()
searchForTree robots i = do
  let (robots', turns) = runState (searchForTree' robots) i
  putStrLn (visualize robots')
  putStrLn "this it?"
  resp <- getLine
  if resp == "y"
    then print turns
    else searchForTree (map translateRobot robots') (turns + 1)

searchForTree' :: [Robot] -> State Int [Robot]
searchForTree' rs = if mightBeTree rs
                    then return rs
                    else modify (+1) >> searchForTree' (map translateRobot rs)

quadrantProd :: [Robot] -> Int
quadrantProd robots = tl * tr * br * bl
  where robotPos = map _pos robots
        xBound = xLimit `div` 2
        yBound = yLimit `div` 2
        tl = length $ filter (\(x, y) -> x < xBound && y < yBound) robotPos
        tr = length $ filter (\(x, y) -> x > xBound && y < yBound) robotPos
        br = length $ filter (\(x, y) -> x > xBound && y > yBound) robotPos
        bl = length $ filter (\(x, y) -> x < xBound && y > yBound) robotPos

translateRobot :: Robot -> Robot
translateRobot r@(Robot pos v) = r {_pos = translate pos v}

translate :: Point -> Velocity -> Point
translate (x, y) (δx, δy) = wrap (x + δx, y + δy)
  where wrap (x, y) = (wrap' x xLimit, wrap' y yLimit)
        wrap' i iLimit | i < 0       = iLimit + i
                       | i >= iLimit = i - iLimit
                       | otherwise   = i

mightBeTree :: [Robot] -> Bool
mightBeTree robots = any (isPoint . _pos) robots
  where roboPosSet = Set.fromList (map _pos robots)
        isPoint pt = all (`Set.member` roboPosSet) (pointPoints pt)
        pointPoints (x, y) = [(x+1, y+1), (x-1, y+1), (x-2, y+2), (x+2, y+2), (x+3, y+3), (x-3, y+3)]

visualize :: [Robot] -> String
visualize robots = intercalate "\n" (map visualizeRow [0..yLimit - 1])
  where posSet = Set.fromList . map _pos $ robots
        visualizeRow y = map (visualizeCol y) [0..xLimit - 1]
        visualizeCol y x = if (x, y) `Set.member` posSet then 'X' else '.'

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = iterate f x !! n

--

parseFile :: Parser [Robot]
parseFile = manyTill parseRobot eof

parseRobot :: Parser Robot
parseRobot = do
  _ <- string "p="
  initPos <- parseIntPair
  _ <- string " v="
  velo <- parseIntPair
  _ <- char '\n'
  return (Robot initPos velo)

parseIntPair :: Parser (Int, Int)
parseIntPair = do
  x <- parseSignedInt
  _ <- char ','
  y <- parseSignedInt
  return (x, y)

parseSignedInt :: Parser Int
parseSignedInt = read <$> (option "" (string "-") <> many1 digit)
