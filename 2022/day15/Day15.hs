-- My original inclination was to iterate over each position and for each
-- position iterate over the list of sensors and determine if the position is
-- closer to the given sensor than any other sensor yet further that than
-- sensor's detected beacon.  I now that this is a terrible idea O(n*s*d) where
-- n is the number of positions (size of grid), s is the number of sensors, and
-- d is the average number of such positions as described above.

-- New idea: Populate the grid with `Sensor`, `Beacon`, `Scanned`, and `Unknown`
-- values.  For this, we start with a grid that is totally `Unknown`, and for
-- each sensor we place it, it's beacon, and fill the spaces between the sensor
-- and beacon with `Scanned`. O(s*d + n) (n from the scan of the grid).

-- Update: Using an Array to model the grid resulted in a heap overflow.  I
-- think the grid may be sparse (no guarantee), and perhaps using a Set will
-- work better.

import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Printf (printf)

import Data.Array.IArray

-- Ye olde `Pos` type, this time a proper record instead of a tuple
data Pos = Pos { x :: Int
               , y :: Int
               }
           deriving (Show, Eq, Ord, Ix)

data Cell = Sensor
          | Beacon
          | Scanned
          | Unknown
          deriving (Show, Eq)

type Grid = Array Pos Cell

data InputLine = InputLine { sensorPos :: Pos
                           , beaconPos :: Pos
                           }
                 deriving Show

main :: IO ()
main = do
  contents <- readFile "input"
  inputLines <- either (fail.show) pure $ runParser parseFile () "input" contents
  let allPos = (map sensorPos inputLines) ++ (map beaconPos inputLines)
      minX = minimum . map x $ allPos
      maxX = maximum . map x $ allPos
      minY = minimum . map y $ allPos
      maxY = maximum . map y $ allPos
      grid = foldl populate (emptyGrid (minX, minY) (maxX, maxY)) inputLines
      numPotential = length . filter (== Unknown) . map (grid !) $ [Pos {x = thisX, y = 2000000} | thisX <- [minX..maxX]]
  printf "Num potential at y = 2000000: %d\n" numPotential

emptyGrid :: (Int, Int) -> (Int, Int) -> Grid
emptyGrid (minX, minY) (maxX, maxY) =
  let values = replicate ((maxX - minX) * (maxY - minY)) Unknown
  in listArray (Pos {x=minX, y=minY}, Pos {x=maxX, y=maxY}) values

populate :: Grid -> InputLine -> Grid
populate grid (InputLine { sensorPos=sp, beaconPos=bp }) =
  let distance = taxicabDistance sp bp
      scanned = map (\pos -> (pos, Scanned)) . filter (withinBounds grid) $ scannedPoints distance sp
  in grid // ([(sp, Sensor), (bp, Beacon)] ++ scanned)

withinBounds :: Grid -> Pos -> Bool
withinBounds grid (Pos {x = thisX, y = thisY}) =
  let (Pos {x = minX, y = minY}, Pos {x = maxX, y = maxY}) = bounds grid
  in thisX >= minX
  && thisX <= maxX
  && thisY >= minY
  && thisY <= maxY

scannedPoints :: Int -> Pos -> [Pos]
scannedPoints distance sp@(Pos {x = sensorX, y = sensorY}) =
  [Pos {x = scanX, y = scanY} |
    scanX <- [sensorX - distance .. sensorX + distance],
    scanY <- [sensorY - distance .. sensorY + distance],
    (taxicabDistance sp Pos {x = scanX, y = scanY}) <= distance]

taxicabDistance :: Pos -> Pos -> Int
taxicabDistance (Pos {x=x1, y=y1}) (Pos {x=x2, y=y2}) =
  (abs (x1 - x2)) + (abs (y1 - y2))

-- Parser -- Not too bad; could've just done string splitting honestly

parseFile :: Parser [InputLine]
parseFile = sepEndBy1 parseLine (char '\n')

parseLine :: Parser InputLine
parseLine = do
  _ <- string "Sensor at "
  parsedSensorPos <- parsePos
  _ <- string ": closest beacon is at "
  parsedBeaconPos <- parsePos
  return (InputLine parsedSensorPos parsedBeaconPos)

parsePos :: Parser Pos
parsePos = do
  _ <- string "x="
  parsedX <- parseNumber
  _ <- string ", y="
  parsedY <- parseNumber
  return Pos { x = parsedX, y = parsedY }

parseNumber :: Parser Int
parseNumber = do
  sign <- option ' ' (char '-')
  num <- many1 digit
  return (read (sign:num))
