import           Control.Applicative (liftA2)
import qualified Data.List           as List
import           GHC.Num             (integerLogBase)
import           Text.Parsec
import           Text.Parsec.Text

type UnfinishedEquation = (Integer, [Integer])
type Operator = (Integer -> Integer -> Integer)

main :: IO ()
main = do
  eqs <- parseFromFile parseFile "input" >>= either (fail . show) pure
  print . sum . map fst . filter (solveable wrappedOps) $ eqs
  print . sum . map fst . filter (solveable wrappedOps') $ eqs

wrappedOps :: [[Operator]]
wrappedOps  = [[(+)], [(*)]]

wrappedOps' :: [[Operator]]
wrappedOps' = [[(+)], [(*)], [concat']]

crossN :: Int -> [[a]] -> [[a]]
crossN i xs = List.last $ take i $ iterate (liftA2 (++) xs) xs

solveable :: [[Operator]] -> UnfinishedEquation -> Bool
solveable ops (rhs, op1:operands) =
  let numOps = length operands
  in any (isSolution op1 . zip operands) (crossN numOps ops)
  where isSolution :: Integer -> [(Integer, Operator)] -> Bool
        isSolution start ops = foldl (\acc (i, op) -> op acc i) start ops == rhs

concat' :: Integer -> Integer -> Integer
concat' x y = x * (10 ^ (integerLogBase 10 y + 1)) + y

parseFile :: Parser [UnfinishedEquation]
parseFile  = parseEq `manyTill` eof

parseEq :: Parser UnfinishedEquation
parseEq = do
  rhs <- read <$> many1 digit
  _ <- string ": "
  operands <- (read <$> many1 digit) `sepBy1` char ' '
  _ <- char '\n'
  return (rhs, operands)
