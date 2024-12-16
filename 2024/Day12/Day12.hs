import           Control.Applicative (liftA2)
import           Data.Bifunctor      (first)
import           Data.Function       (on)
import           Data.List           ((\\))
import           Data.Map            (Map, (!), (!?))
import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Debug.Trace         (traceShowId)
import           Text.Parsec         (char, eof, many1, manyTill, satisfy)
import           Text.Parsec.Text    (Parser, parseFromFile)

type Crop = Char
type Point = (Int, Int)
type GardenMap = Map Point Crop
type CropGroup = Set Point
type GroupNeighbors = Map Point [Point]

main :: IO ()
main = do
  gardenMap <- parseFromFile parseFile "input" >>= either (fail . show) pure
  let groups = formGroups gardenMap
      gn = foldl groupNeighbors Map.empty groups
  print . sum . map (tabulatePrice gn) . Set.toList $ groups
  print . sum . map (tabulatePrice' gn) . Set.toList $ groups

formGroups :: GardenMap -> Set CropGroup
formGroups gardenMap = foldl groupPoint Set.empty (Map.assocs gardenMap)
  where
    groupPoint :: Set CropGroup -> (Point, Crop) -> Set CropGroup
    groupPoint groups (pt, crop) =
      if any (Set.member pt) groups
      then groups
      else Set.insert (buildGroup crop (Set.singleton pt) pt) groups
    buildGroup :: Crop -> CropGroup -> Point -> CropGroup
    buildGroup crop group pt = let newValidPoints = filter (validGroupPoint group crop) $ neighbors pt
                                   updatedGroup = foldr Set.insert group newValidPoints
                                in foldl (buildGroup crop) updatedGroup newValidPoints
    validGroupPoint :: CropGroup ->Crop -> Point -> Bool
    validGroupPoint group crop pt = Set.notMember pt group && gardenMap !? pt ==Just crop

groupNeighbors :: GroupNeighbors -> CropGroup -> GroupNeighbors
groupNeighbors gn group = foldl addAdjacents gn group
  where addAdjacents :: GroupNeighbors -> Point -> GroupNeighbors
        addAdjacents gn pt = Map.insert pt (adjacents pt) gn
        adjacents pt = filter (`Set.member` group) (neighbors pt)

neighbors :: Point -> [Point]
neighbors (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

tabulatePrice :: GroupNeighbors -> CropGroup -> Int
tabulatePrice gn cropGroup = area * perimeter
  where area = Set.size cropGroup
        perimeter = sum . map ((4 -) . length . (gn !)) . Set.toList $ cropGroup

tabulatePrice' :: GroupNeighbors -> CropGroup -> Int
tabulatePrice' gn cropGroup = area * sides
  where area = Set.size cropGroup
        sides = length . traceShowId . pointsToSides $ perimeterPoints
        perimeterPoints = foldr (Set.union .  perimeterPoints') Set.empty cropGroup
        perimeterPoints' pt = Set.fromList (neighbors pt \\ gn ! pt)

type Side = Set Point

pointsToSides :: Set Point -> Set Side
pointsToSides perimPts = foldl addSides Set.empty perimPts
  where
    -- Based on the flawed assumption that a point may only be part of one side
    addSides :: Set Side -> Point -> Set Side
    addSides sides pt = Set.insert (maxBy (compare `on` length) (horSide pt) (vertSide pt)) sides
    vertSide (x, y) = Set.union
                            (Set.fromList . takeWhile (`Set.member` perimPts) . map (x,) $ [y..])
                            (Set.fromList . takeWhile (`Set.member` perimPts) . map (x,) $ [y, y - 1..])
    horSide (x, y) = Set.union
                            (Set.fromList . takeWhile (`Set.member` perimPts) . map (,y) $ [x..])
                            (Set.fromList . takeWhile (`Set.member` perimPts) . map (,y) $ [x, x - 1..])

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy f x y = case f x y of
                LT -> y
                _  -> x

-- Parser

parseFile :: Parser GardenMap
parseFile = Map.fromList . concat . zipWith applyRowIx [0..] <$> manyTill parseRow eof
  where applyRowIx rowIx = map (first (,rowIx))

parseRow :: Parser [(Int, Crop)]
parseRow = zip [0..] <$> many1 (satisfy (/= '\n'))  <* char '\n'
