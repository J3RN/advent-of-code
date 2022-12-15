-- This in no way necessitates creating a matrix, as was my first inclination.

-- Two approaches:

-- 1. For each line, generate the entries for all the points in between each
--    pair of terminals.  This involves up-front work of generating the points
--    and populating the set (~Θ(p*l*log(p*l)) where p is the number of paths
--    and l is the average length of each path), but checking spot occupancy is
--    fast (O(log(n))) and inserting new points is fast as well (O(log(n))).
-- 2. Store the paths as, per se, a list of lines.  The up-front work is Θ(p*m)
--    where p is the number of paths and m is the average number of points in a
--    path.  Checking spot occupancy is Θ(p*m) as well, as we'd have to check
--    the point against each line.  We could store sand as lines of length 1—
--    the insertion into the list is O(1)—but there would quickly be many of
--    them making the occupancy check slower.
--
-- In summary, I'm starting with Option 1 and hopefully it won't bite me (e.g.
-- with really long paths).

-- Part1 Follow-up: Seems reasonably performant.  Part 1 takes 0.044s to run
-- which isn't _fast_, but is honestly faster than Emacs can render.  Also, I
-- was wrong about diagonals!

-- Part2 Follow-up: Well, Part 2 takes about 2.2 seconds to run, which sucks
-- quite frankly.  I'm going to break out the profiler and see what I can do.

-- Part duex: It's `sandNextPos`, probably `Set.notMember`.

import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Printf (printf)

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

type Pos = (Int, Int)
type Path = [Pos]
type Line = (Pos, Pos)
-- It only occurred to me much later how this is confusing
type Map = IntSet

main :: IO ()
main = do
  contents <- readFile "input"
  paths <- either (fail.show) pure $ runParser parseFile () "input" contents
  let m = buildMap paths
      maxD = maxDepth m
      numSand = unitsOfSand maxD dropSand m
  printf "Units of sand: %d\n" numSand
  let numSand' = unitsOfSand maxD dropSand' m
  printf "Units of sand: %d\n" numSand'

unitsOfSand :: Int -> (Int -> Pos -> Map -> Map) -> Map -> Int
unitsOfSand maxD dropFun m
  | m == newMap = 0
  | otherwise = 1 + unitsOfSand maxD dropFun newMap
  where newMap = dropFun maxD (500,0) m

buildMap :: [Path] -> Map
buildMap paths =
  foldl insertPath Set.empty paths

insertPath :: Map -> Path -> Map
insertPath m path =
   foldl insertLine m (pairs path)

insertLine :: Map -> Main.Line -> Map
insertLine m line =
  foldl insertPoint m (generatePoints line)

insertPoint :: Map -> Pos -> Map
insertPoint m pos = Set.insert (hash pos) m

pairs :: [a] -> [(a, a)]
pairs l = zip (drop 1 l) l

generatePoints :: (Pos, Pos) -> [Pos]
generatePoints ((x1, y1), (x2, y2))
  | x1 == x2 = [(x1, y) | y <- [(min y1 y2)..(max y1 y2)]]
  | y1 == y2 = [(x, y1) | x <- [(min x1 x2)..(max x1 x2)]]

maxDepth :: Map -> Int
maxDepth m =
  maximum . map (snd . unhash) . Set.elems $ m

dropSand :: Int -> Pos -> Map -> Map
dropSand maxD oldPos m
  | nextPos == oldPos  = Set.insert (hash oldPos) m
  | snd nextPos > maxD = m
  | otherwise          = dropSand maxD nextPos m
  where nextPos = sandNextPos m oldPos

dropSand' :: Int -> Pos -> Map -> Map
dropSand' maxD oldPos m
  | nextPos == oldPos       = Set.insert (hash oldPos) m
  | snd nextPos == maxD + 1 = Set.insert (hash nextPos) m
  | otherwise               = dropSand' maxD nextPos m
  where nextPos = sandNextPos m oldPos

sandNextPos :: Map -> Pos -> Pos
sandNextPos m oldPos@(x, y)
  | hash (x,     y + 1) `Set.notMember` m = (x,     y + 1)
  | hash (x - 1, y + 1) `Set.notMember` m = (x - 1, y + 1)
  | hash (x + 1, y + 1) `Set.notMember` m = (x + 1, y + 1)
  | otherwise                        = oldPos

-- Ah, a homebrewed hashing function!
-- Yeah yeah, the idea here is to create a unique integers from pairs.  This
-- uses Cantor's enumeration of pairs.

hash :: Pos -> Int
hash (x, y) =
  let s = (x + y)
  in ((s * (s + 1)) `div` 2) + y

-- Well, this hash is reversible which is fine for this usecase

unhash :: Int -> Pos
unhash z =
  let w = floor ((sqrt(8 * (toEnum z :: Double) + 1) - 1) / 2)
      t = (w * w + w) `div` 2
      y = z - t
  in (w - y, y)

-- Parser

parseFile :: Parser [Path]
parseFile = sepEndBy1 parsePath (char '\n')

parsePath :: Parser Path
parsePath = sepBy1 parsePos (string " -> ")

parsePos :: Parser Pos
parsePos = do
  x <- many1 digit
  _ <- char ','
  y <- many1 digit
  return ((read x), (read y))
