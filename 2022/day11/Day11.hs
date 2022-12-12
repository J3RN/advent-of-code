import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Printf

import Data.Array.IArray
import Data.List (find, sortOn)
import Data.Maybe (fromJust)

type MonkeyIx = Int
type Item = Int

data Operand
  = Old
  | Val Int

data Operation
  = Add
  | Mul

data Expression = Expression Operand Operation Operand

data Monkey = Monkey { items :: [Item]
                     , operation :: Expression
                     , factor :: Int
                     , ifTrue :: MonkeyIx
                     , ifFalse :: MonkeyIx
                     , numProcessed :: Int
                     }

instance Show Monkey where
  show m = printf "Num Processed: %d" (numProcessed m)

main :: IO ()
main = do
  contents <- readFile "input"
  monkeyList <- either (fail . show) pure $ runParser parseFile () "input" contents
  let monkeys = listArray (0, (length monkeyList - 1)) monkeyList
      mb = monkeyBusiness . nTimes 20 Main.round $ monkeys
  printf "Monkey business after 20: %d\n" mb

-- Known as 'iterate'' in Agda
nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _fun a = a
nTimes n fun a = nTimes (n - 1) fun (fun a)

monkeyBusiness :: Array MonkeyIx Monkey -> Int
monkeyBusiness monkeys =
  let top2 = take 2 . sortOn negate . map numProcessed . elems $ monkeys
  in foldl1 (*) top2

round :: Array MonkeyIx Monkey -> Array MonkeyIx Monkey
round monkeys =
  let (firstMonkeyIx, lastMonkeyIx) = bounds monkeys
  in foldl takeTurn monkeys [firstMonkeyIx..lastMonkeyIx]

takeTurn :: Array MonkeyIx Monkey -> MonkeyIx -> Array MonkeyIx Monkey
takeTurn monkeys monkeyIndex =
  let
    monkey = monkeys ! monkeyIndex
    itemsToProcess = items monkey
    updatedMs = foldl (processItem (\x -> div x 3) monkey) monkeys itemsToProcess
  in updatedMs // [(monkeyIndex, (monkey { numProcessed = numProcessed monkey + toEnum (length itemsToProcess), items = []}))]

processItem :: (Item -> Item) -> Monkey -> Array MonkeyIx Monkey -> Item -> Array MonkeyIx Monkey
processItem worryOp monkey monkeys item =
  let newWorry = applyOperation item (operation monkey)
      postInspection = worryOp newWorry
      recipient = if test monkey postInspection then (ifTrue monkey) else (ifFalse monkey)
  in accum addItem monkeys [(recipient, postInspection)]

addItem :: Monkey -> Item -> Monkey
addItem monkey item =
  -- Yes, it pains me to use '++'
  monkey { items = (items monkey) ++ [item] }

test :: Monkey -> Item -> Bool
test monkey i = i `rem` (factor monkey) == 0

applyOperation :: Item -> Expression -> Item
applyOperation _item (Expression (Val a) op (Val b)) = (toFun op) a b
applyOperation item  (Expression (Val a) op Old)     = (toFun op) a item
applyOperation item  (Expression Old     op (Val b)) = (toFun op) item b
applyOperation item  (Expression Old     op Old)     = (toFun op) item item

toFun :: Operation -> (Item -> Item -> Item)
toFun Add = (+)
toFun Mul = (*)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors x =
  let fac = fromJust $ find (\i -> x `rem` i == 0) [2..x]
  in fac:(primeFactors (x `div` fac))

-- Parser -- A particularly fun one today!

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
  f <- parseFactor
  it <- parseIfTrue
  if' <- parseIfFalse
  return (Monkey is op f it if' 0)

parseItems :: Parser [Int]
parseItems = do
  _ <- string "  Starting items: "
  itemValues <- sepBy (many1 digit) (string ", ")
  _ <- char '\n'
  return (map read itemValues)

parseOperation :: Parser Expression
parseOperation = do
  _ <- string "  Operation: new = "
  e <- parseExp
  _ <- char '\n'
  return e

parseExp :: Parser Expression
parseExp = do
  a <- parseOperand
  _ <- char ' '
  op <- parseOp
  _ <- char ' '
  b <- parseOperand
  return (Expression a op b)

parseOperand :: Parser Operand
parseOperand = parseOld <|> parseVal

parseOld :: Parser Operand
parseOld = (Old) <$ string "old"

parseVal :: Parser Operand
parseVal = fmap (Val . read) $ many1 digit

parseOp :: Parser Operation
parseOp = parseAdd <|> parseMul

parseAdd :: Parser Operation
parseAdd = Add <$ char '+'

parseMul :: Parser Operation
parseMul = Mul <$ char '*'

parseFactor :: Parser Int
parseFactor = do
  _ <- string "  Test: divisible by "
  num <- many1 digit
  _ <- char '\n'
  return (read num)

parseIfTrue :: Parser MonkeyIx
parseIfTrue = do
  _ <- string "    If true: throw to monkey "
  num <- many1 digit
  _ <- char '\n'
  return (read num)

parseIfFalse :: Parser MonkeyIx
parseIfFalse = do
  _ <- string "    If false: throw to monkey "
  num <- many1 digit
  _ <- char '\n'
  return (read num)
