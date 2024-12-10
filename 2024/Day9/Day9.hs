import           Data.Array     (Array, (!), (//))
import qualified Data.Array     as Array
import           Data.Bifunctor (bimap, second)
import qualified Data.List      as List
import           Data.Maybe     (isJust)
import           Data.Text      (Text)
import qualified Data.Text.IO   as T
import           GHC.Num        (integerFromInt)
import           Text.Parsec

type NodeId = Integer

main :: IO ()
main = do
  sectorList <- parseFromFile parseFile "input" >>= either (fail . show) pure
  let expandedSectorList = expand sectorList
  print . checksum . compact . listToArr . expand $ sectorList
  print . checksum . contiguousCompact . listToArr . expand $ sectorList

listToArr :: [a] -> Array Int a
listToArr l = Array.listArray (0, length l - 1) l

-- Convert the compact format to the expanded format
expand :: [(Int, Maybe NodeId)] -> [Maybe NodeId]
expand = concatMap (uncurry replicate)

checksum :: Array Int (Maybe NodeId) -> NodeId
checksum sectors = foldl u 0 (Array.assocs sectors)
  where u acc (i, e) = case e of
                              Just id -> acc + integerFromInt i * id
                              Nothing -> acc

compact :: Array Int (Maybe NodeId) -> Array Int (Maybe NodeId)
compact sectors = sectors // updates
  where updates = uncurry (compactUpdates sectors) (Array.bounds sectors)

compactUpdates :: Array Int (Maybe NodeId) -> Int -> Int -> [(Int, Maybe NodeId)]
compactUpdates sectors lPtr rPtr
  | lPtr >= rPtr = []
  | otherwise = case sectors ! lPtr of
                  Just _  -> compactUpdates sectors (lPtr + 1) rPtr
                  Nothing -> case sectors ! rPtr of
                               Just sub -> [(lPtr, Just sub), (rPtr, Nothing)] ++ compactUpdates sectors (lPtr + 1) (rPtr - 1)
                               Nothing  -> compactUpdates sectors lPtr (rPtr - 1)

contiguousCompact :: Array Int (Maybe NodeId) -> Array Int (Maybe NodeId)
contiguousCompact sectors = contiguousCompact' sectors maxIx emptyCache
  where maxIx = snd . Array.bounds $ sectors
        emptyCache = Array.listArray (1,9) (repeat 0)

contiguousCompact' :: Array Int (Maybe NodeId) -> Int -> Array Int Int -> Array Int (Maybe NodeId)
contiguousCompact' sectors 0    _     = sectors
contiguousCompact' sectors rPtr cache = case sectors ! rPtr of
                                          Nothing     -> contiguousCompact' sectors (rPtr - 1) cache
                                          Just nodeId -> let (rPtr', blockSize) = findBlockSize sectors rPtr nodeId
                                                         in case findEmptySlot sectors blockSize rPtr' (cache ! blockSize) of
                                                              Just ix -> contiguousCompact' (sectors // updates sectors ix (rPtr' + 1) blockSize) rPtr' (cache // [(blockSize, ix)])
                                                              Nothing -> contiguousCompact' sectors rPtr' cache
  where findBlockSize :: Array Int (Maybe NodeId) -> Int -> Integer -> (Int, Int)
        findBlockSize sectors rPtr' nodeId = until (\(ix, _) -> ix == 0 || (sectors ! ix /= Just nodeId)) (bimap pred succ) (rPtr', 0)
        findEmptySlot :: Array Int (Maybe NodeId) -> Int -> Int -> Int -> Maybe Int
        findEmptySlot sectors blockSize maxIx ix = fst <$> List.find ((>= blockSize) . snd) (emptySlots sectors ix maxIx)
        emptySlots :: Array Int (Maybe NodeId) -> Int -> Int -> [(Int, Int)]
        emptySlots sectors ix maxIx | ix >= maxIx = []
        emptySlots sectors ix maxIx               = case sectors ! ix of
                                                      Just _  -> emptySlots sectors (ix + 1) maxIx
                                                      Nothing -> let end = until (\ix' -> ix' > maxIx || isJust (sectors ! ix')) (+ 1) ix
                                                                  in (ix, end - ix):emptySlots sectors end maxIx
        updates :: Array Int (Maybe NodeId) -> Int -> Int -> Int -> [(Int, Maybe NodeId)]
        updates sectors emptyStart fileStart fileSize = concat $ zipWith (\e f -> [(e, sectors ! f), (f, Nothing)]) [emptyStart..] [fileStart..(fileStart + fileSize - 1)]

-- Parser

data ParseState = ParseState { _isFree :: Bool
                             , _nodeId :: NodeId
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
parseFile :: Parser [(Int, Maybe NodeId)]
parseFile = manyTill parseSector (char '\n')

parseSector :: Parser (Int, Maybe NodeId)
parseSector = do
  ParseState isFree nodeId <- getState
  c <- read . List.singleton <$> digit
  let res = if isFree then (c, Nothing) else (c, Just nodeId)
  putState (ParseState (not isFree) (if isFree then nodeId + 1 else nodeId))
  return res
