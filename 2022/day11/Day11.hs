import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Printf

import Data.Array.IArray
import Data.List (sortOn)

data Monkey = Monkey { items :: [Int]
                     , operation :: Int -> Int
                     , test :: Int -> Bool
                     , ifTrue :: Int
                     , ifFalse :: Int
                     , itemCursor :: Int
                     }

instance Show Monkey where
  show (Monkey {items = is, itemCursor = ic}) = "Items: " <> (show . drop ic $ is)

main :: IO ()
main = do
  contents <- readFile "input"
  monkeyList <- either (fail . show) pure $ runParser parseFile () "input" contents
  let monkeys = listArray (0, (length monkeyList - 1)) monkeyList
      mb = monkeyBusiness . nTimes 20 Main.round $ monkeys
  printf "Monkey business after 20: %d\n" mb

-- Known as @iterate'@ in Agda
nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _fun a = a
nTimes n fun a = nTimes (n - 1) fun (fun a)

monkeyBusiness :: Array Int Monkey -> Int
monkeyBusiness monkeys =
  let top2 = take 2 . sortOn negate . map itemCursor . elems $ monkeys
  in foldl1 (*) top2

round :: Array Int Monkey -> Array Int Monkey
round monkeys =
  foldl takeTurn monkeys [(fst . bounds $ monkeys)..(snd . bounds $ monkeys)]

takeTurn :: Array Int Monkey -> Int -> Array Int Monkey
takeTurn monkeys monkeyIndex =
  let
    monkey = monkeys ! monkeyIndex
    itemsToProcess = drop (itemCursor monkey) $ items monkey
    updatedMs = foldl (processItem monkey) monkeys itemsToProcess
  in updatedMs // [(monkeyIndex, (monkey { itemCursor = itemCursor monkey + length itemsToProcess}))]

processItem :: Monkey -> Array Int Monkey -> Int -> Array Int Monkey
processItem monkey monkeys item =
  let newWorry = (operation monkey) item
      postInspection = newWorry `div` 3
      recipient = if (test monkey) postInspection then (ifTrue monkey) else (ifFalse monkey)
  in accum addItem monkeys [(recipient, postInspection)]

addItem :: Monkey -> Int -> Monkey
addItem monkey item =
  -- Yes, it pains me to use @++@
  monkey { items = (items monkey) ++ [item] }

-- Parser -- A particularly fun one today!

data Operand
  = Old
  | Val Int

data Operation
  = Add
  | Mul

parseFile :: Parser [Monkey]
parseFile = do
  ms <- sepEndBy parseMonkey (char '\n')
  return ms

parseMonkey :: Parser Monkey
parseMonkey = do
  _ <- string "Monkey "
  -- Ignoring index as they're (currently) in order
  _ <- many1 digit
  _ <- string ":\n"
  is <- parseItems
  op <- parseOperation
  t <- parseMTest
  it <- parseIfTrue
  if' <- parseIfFalse
  return (Monkey is op t it if' 0)

parseItems :: Parser [Int]
parseItems = do
  _ <- string "  Starting items: "
  itemValues <- sepBy (many1 digit) (string ", ")
  _ <- char '\n'
  return (map read itemValues)

parseOperation :: Parser (Int -> Int)
parseOperation = do
  _ <- string "  Operation: new = "
  exp <- parseExp
  _ <- char '\n'
  return exp

parseExp :: Parser (Int -> Int)
parseExp = do
  a <- parseOperand
  _ <- char ' '
  op <- parseOp
  _ <- char ' '
  b <- parseOperand
  return (toFun a (op) b)

toFun :: Operand -> (Int -> Int -> Int) -> Operand -> (Int -> Int)
toFun Old     op Old     = (\x -> op x x)
toFun Old     op (Val a) = (\x -> op x a)
toFun (Val a) op Old     = (\x -> op a x)
toFun (Val a) op (Val b) = (\x -> op a b)

parseOperand :: Parser Operand
parseOperand = parseOld <|> parseVal

parseOld :: Parser Operand
parseOld = (Old) <$ string "old"

parseVal :: Parser Operand
parseVal = fmap (Val . read) $ many1 digit

parseOp :: Parser (Int -> Int -> Int)
parseOp = parseAdd <|> parseMul

parseAdd :: Parser (Int -> Int -> Int)
parseAdd = (+) <$ char '+'

parseMul :: Parser (Int -> Int -> Int)
parseMul = (*) <$ char '*'

parseMTest :: Parser (Int -> Bool)
parseMTest = do
  _ <- string "  Test: divisible by "
  num <- many1 digit
  _ <- char '\n'
  return (\x -> x `rem` (read num) == 0)

parseIfTrue :: Parser Int
parseIfTrue = do
  _ <- string "    If true: throw to monkey "
  num <- many1 digit
  _ <- char '\n'
  return (read num)

parseIfFalse :: Parser Int
parseIfFalse = do
  _ <- string "    If false: throw to monkey "
  num <- many1 digit
  _ <- char '\n'
  return (read num)
