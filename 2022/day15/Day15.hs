import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Printf (printf)

import Data.List (find, maximumBy, minimumBy, nub)
import Data.Maybe (isJust)

import Debug.Trace (trace)

type Pos = (Int, Int)

data Sensor = Sensor { pos :: Pos
                     , distanceToBeacon :: Int
                     }
              deriving Show

data InputLine = InputLine { sensorPos :: Pos
                           , beaconPos :: Pos
                           }
                 deriving Show

main :: IO ()
main = do
  contents <- readFile "input"
  inputLines <- either (fail.show) pure $ runParser parseFile () "input" contents
  let sensors = map toSensor inputLines
      leftMostSensor = minimumBy compareByX sensors
      rightMostSensor = maximumBy compareByX sensors
      minX = (fst . pos $ leftMostSensor) - (distanceToBeacon leftMostSensor) - 1
      maxX = (fst . pos $ rightMostSensor) + (distanceToBeacon rightMostSensor) + 1
      row = 2000000
      ns = length . filter (isJust . scannedBy sensors) . map (\x -> (x, row)) $ [minX..maxX]
      numBeaconsInRow = length . filter (((==) row) . snd) . nub $ map beaconPos inputLines
  printf "Num scanned in row %d: %d\n" row $ ns - numBeaconsInRow
  -- let (thisX, thisY) = findUnscanned sensors (maybeNext 0 4000000 sensors) (0, 0)
  -- printf "Tuning frequency: %d\n" $ thisX * 4000000 + thisY

compareByX :: Sensor -> Sensor -> Ordering
compareByX s1 s2 = compare (fst . pos $ s1) (fst . pos $ s2)

numScanned :: [Sensor] -> Int -> Pos -> Int -> Int
numScanned sensors maxX p@(thisX, thisY) num =
  case nextX p <$> scannedBy sensors p of
    Just nX
      | nX <= maxX -> numScanned sensors maxX (nX, thisY) (num + (nX - thisX))
      | otherwise  -> num + (nX - thisX)
    Nothing
      | thisX + 1 <= maxX -> numScanned sensors maxX (thisX + 1,  thisY) num
      | otherwise         -> num

findUnscanned :: [Sensor] -> (Pos -> Maybe Pos) -> Pos -> Pos
findUnscanned sensors nextFun startPos =
  case nextFun startPos of
    Just np -> findUnscanned sensors nextFun np
    Nothing -> startPos

maybeNext :: Int -> Int -> [Sensor] -> Pos -> Maybe Pos
maybeNext minX maxX sensors thisPos =
  (nextPos minX maxX thisPos) <$> (scannedBy sensors thisPos)

nextX :: Pos -> Sensor -> Int
nextX p sensor@Sensor {pos = (xs, _ys)} =
  let dX = deltaX p sensor
  in xs + dX + 1

nextPos :: Int -> Int -> Pos -> Sensor -> Pos
nextPos minX maxX p@(_thisX, thisY) sensor@(Sensor {pos = (xs, _ys)}) =
  let nX = nextX p sensor
  in if nX > maxX
     then (minX, thisY + 1)
     else (nX, thisY)

deltaX :: Pos -> Sensor -> Int
deltaX (_thisX, thisY) (Sensor {pos = (_xs, ys), distanceToBeacon = d}) =
  d - (abs (thisY - ys))

scannedBy :: [Sensor] -> Pos -> Maybe Sensor
scannedBy sensors p =
  find (not . beyondScan p) sensors

toSensor :: InputLine -> Sensor
toSensor (InputLine {sensorPos = sp, beaconPos = bp}) =
  Sensor {pos = sp, distanceToBeacon = (taxicabDistance sp bp)}

taxicabDistance :: Pos -> Pos -> Int
taxicabDistance (x1, y1) (x2, y2) =
  (abs (x1 - x2)) + (abs (y1 - y2))

beyondScan :: Pos -> Sensor -> Bool
beyondScan p sensor =
  taxicabDistance p (pos sensor) > distanceToBeacon sensor

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
  return (parsedX, parsedY)

parseNumber :: Parser Int
parseNumber = do
  sign <- option ' ' (char '-')
  num <- many1 digit
  return (read (sign:num))
