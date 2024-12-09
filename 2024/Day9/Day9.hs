import           Data.Array   (Array, (!), (//))
import qualified Data.Array   as Array
import qualified Data.List    as List
import           Data.Text    (Text)
import qualified Data.Text.IO as T
import           GHC.Num      (integerFromInt)
import           Text.Parsec

main :: IO ()
main = do
  sectorList <- parseFromFile parseFile "input" >>= either (fail . show) pure
  let expandedSectorList = expand sectorList
  print . checksum . compact . listToArr . expand $ sectorList
  print . checksum . _ . contiguousCompact . listToArr $ sectorList

listToArr :: [a] -> Array Int a
listToArr l = Array.listArray (0, length l - 1) l

-- Convert the compact format to the expanded format
expand ::[(Int, Maybe Integer)] -> [Maybe Integer]
expand = concatMap (uncurry replicate)

checksum :: Array Int (Maybe Integer) -> Integer
checksum sectors = foldl u 0 (Array.assocs sectors)
  where u acc (i, e) = case e of
                              Just id -> acc + integerFromInt i * id
                              Nothing -> acc

compact :: Array Int (Maybe Integer) -> Array Int (Maybe Integer)
compact sectors = sectors // updates
  where updates = uncurry (compactUpdates sectors) (Array.bounds sectors)

compactUpdates :: Array Int (Maybe Integer) -> Int -> Int -> [(Int, Maybe Integer)]
compactUpdates sectors lPtr rPtr
  | lPtr >= rPtr = []
  | otherwise = case sectors ! lPtr of
                  Just _  -> compactUpdates sectors (lPtr + 1) rPtr
                  Nothing -> case sectors ! rPtr of
                               Just sub -> [(lPtr, Just sub), (rPtr, Nothing)] ++ compactUpdates sectors (lPtr + 1) (rPtr - 1)
                               Nothing  -> compactUpdates sectors lPtr (rPtr - 1)

-- This foldr is going to be hellaceously slow, but we need iterative updates to the sectors arr to
-- prevent double-allocating a slot
contiguousCompact :: Array Int (Int, Maybe Integer) -> Array Int (Int, Maybe Integer)
contiguousCompact sectors = foldr contiguousCompact' sectors (Array.assocs sectors)

contiguousCompact' :: (Int, (Int, Maybe Integer)) -> Array Int (Int, Maybe Integer) -> Array Int (Int, Maybe Integer)
contiguousCompact' (_, (_, Nothing)) sectors = sectors
contiguousCompact' (ix, (c, val)) sectors    = case findSizeSlot c sectors of
                                                 (Just (ix2, (c2, _))) | ix2 < ix -> sectors // _
                                                 Nothing    -> sectors
  where
    findSizeSlot :: Int -> Array Int (Int, Maybe Integer) -> Maybe (Int, (Int, Maybe Integer))
    findSizeSlot c = List.find ((>= c) . fst) . Array.assocs


-- THIS DOESN'T WORK BECAUSE WE DON'T GO LINEARLY LTR
-- contiguousCompact :: Array Int (Int, Maybe Integer) -> Array Int (Int, Maybe Integer)
-- contiguousCompact sectors = sectors // updates
--   where updates = uncurry (contiguousCompact' sectors) (first (+ 1) $ Array.bounds sectors)

-- contiguousCompact' :: Array Int (Int, Maybe Integer) -> Int -> Int -> [(Int, (Int, Maybe Integer))]
-- contiguousCompact' sectors lPtr rPtr
--   | lPtr >= rPtr = []
--   | otherwise = case sectors ! rPtr of
--                   (_, Nothing)     -> contiguousCompact' sectors lPtr (rPtr -1)
--                   (c, Just nodeId) -> _

-- Parser

data ParseState = ParseState { _isFree :: Bool
                             , _nodeId :: Integer
                             }
type Parser = Parsec Text ParseState

initialState :: ParseState
initialState = ParseState { _isFree = False
                          , _nodeId = 0
                          }

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- T.readFile fname
         return (runP p initialState fname input)

-- Parses the format used in the example, with `Nothing` representing free space
parseFile :: Parser [(Int, Maybe Integer)]
parseFile = manyTill parseSector (char '\n')

parseSector :: Parser (Int, Maybe Integer)
parseSector = do
  ParseState isFree nodeId <- getState
  c <- read . List.singleton <$> digit
  let res = if isFree then (c, Nothing) else (c, Just nodeId)
  putState (ParseState (not isFree) (if isFree then nodeId + 1 else nodeId))
  return res
