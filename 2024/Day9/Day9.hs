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
  let sectors = Array.listArray (0, length sectorList - 1) sectorList
  print . checksum . compact $ sectors

checksum :: Array Int (Maybe Integer) -> Integer
checksum sectors = foldl u 0 (Array.assocs sectors)
  where u acc (i, e) = case e of
                              Just id -> acc + integerFromInt i * id
                              Nothing -> acc

compact :: Array Int (Maybe Integer) -> Array Int (Maybe Integer)
compact sectors = sectors // uncurry (compactUpdates sectors) (Array.bounds sectors)

compactUpdates :: Array Int (Maybe Integer) -> Int -> Int -> [(Int, Maybe Integer)]
compactUpdates sectors lPtr rPtr
  | lPtr >= rPtr = []
  | otherwise = case sectors ! lPtr of
                  Just _  -> compactUpdates sectors (lPtr + 1) rPtr
                  Nothing -> case sectors ! rPtr of
                               Just sub -> [(lPtr, Just sub), (rPtr, Nothing)] ++ compactUpdates sectors (lPtr + 1) (rPtr - 1)
                               Nothing  -> compactUpdates sectors lPtr (rPtr - 1)

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
parseFile :: Parser [Maybe Integer]
parseFile = concat <$> manyTill parseSector (char '\n')

parseSector :: Parser [Maybe Integer]
parseSector = do
  ParseState isFree nodeId <- getState
  c <- read . List.singleton <$> digit
  let res = if isFree then replicate c Nothing else replicate c (Just nodeId)
  putState (ParseState (not isFree) (if isFree then nodeId + 1 else nodeId))
  return res
