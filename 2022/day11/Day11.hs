import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Printf

import Data.Array.IArray
import Data.List (singleton, sortOn)

type MonkeyIx = Int

data Operand
  = Old
  | Val Integer
  deriving Show

data Operation
  = Add
  | Mul
  deriving Show

data Expression = Expression Operation Operand deriving Show
type Item = [Expression]

data Monkey = Monkey { items :: [Item]
                     , operation :: Expression
                     , factor :: Integer
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
      afterMonkeys = nTimes 1000 Main.round $ monkeys
  putStrLn (show afterMonkeys)
  printf "Monkey business after 1000: %d\n" (monkeyBusiness afterMonkeys)

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
    updatedMs = foldl (processItem monkey) monkeys itemsToProcess
  in updatedMs // [(monkeyIndex, (monkey { numProcessed = numProcessed monkey + toEnum (length itemsToProcess), items = []}))]

processItem :: Monkey -> Array MonkeyIx Monkey -> Item -> Array MonkeyIx Monkey
processItem monkey monkeys item =
  let newWorry = applyOperation item (operation monkey)
      recipient = if test monkey newWorry then (ifTrue monkey) else (ifFalse monkey)
  in accum addItem monkeys [(recipient, newWorry)]

addItem :: Monkey -> Item -> Monkey
addItem monkey item =
  -- Yes, it pains me to use '++'
  monkey { items = (items monkey) ++ [item] }

test :: Monkey -> Item -> Bool
test monkey i = (expand i) `rem` (factor monkey) == 0

expand :: Item -> Integer
expand i = foldr expandOp 1 i

expandOp :: Expression -> Integer -> Integer
expandOp (Expression Add (Val b)) a = a + b
expandOp (Expression Add Old)     a = a + a
expandOp (Expression Mul (Val b)) a = a * b
expandOp (Expression Mul Old)     a = a * a

applyOperation :: Item -> Expression -> Item
applyOperation ((Expression Add (Val a)):rest) (Expression Add (Val b)) = (Expression Add (Val (a + b))):rest
applyOperation ((Expression Mul (Val a)):rest) (Expression Mul (Val b)) = (Expression Mul (Val (a * b))):rest
applyOperation item                            e                        = e:item

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

parseItems :: Parser [Item]
parseItems = do
  _ <- string "  Starting items: "
  itemValues <- sepBy (many1 digit) (string ", ")
  _ <- char '\n'
  return (map (singleton . Expression Mul . Val . read) itemValues)

parseOperation :: Parser Expression
parseOperation = do
  _ <- string "  Operation: new = "
  e <- parseExp
  _ <- char '\n'
  return e

parseExp :: Parser Expression
parseExp = do
  _ <- string "old "
  op <- parseOp
  _ <- char ' '
  b <- parseOperand
  return (Expression op b)

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

parseFactor :: Parser Integer
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
