import Data.Bifunctor (first)
import Data.Function (on)
import Data.List (group, partition, sortBy, sort)

import Text.Parsec
import Text.Parsec.String (Parser)

data Card = Ace
          | King
          | Queen
          | Jack
          | Ten
          | Nine
          | Eight
          | Seven
          | Six
          | Five
          | Four
          | Three
          | Two
          | One
          | Joker
          deriving (Show, Eq, Ord)

newtype Bid = Bid Int
            deriving Show

data Hand = FiveOf    [Card]
          | FourOf    [Card]
          | FullHouse [Card]
          | ThreeOf   [Card]
          | TwoPair   [Card]
          | OnePair   [Card]
          | High      [Card]
          deriving (Show, Eq, Ord)

main :: IO ()
main = do
  contents <- readFile "input"
  rows <- either (fail . show) pure $ runParser parseFile () "input" contents
  let sorted = sortBy (flip compare `on` (toHand groupings . fst)) rows
      totals = zipWith (\(_hand, Bid bid) rank -> bid * rank) sorted [1..]
  print $ sum totals
  let newRows =  map (first transformJacks) rows
      newSorted = sortBy (flip compare `on` (toHand groupings' . fst)) newRows
      newTotals = zipWith (\(_hand, Bid bid) rank -> bid * rank) newSorted [1..]
  print $ sum newTotals

toHand :: ([Card] ->[[Card]]) -> [Card] -> Hand
toHand groupFun cards =
  case groupFun cards of
    [[_, _, _, _, _]]         -> FiveOf cards
    [[_, _, _, _], [_]]       -> FourOf cards
    [[_, _, _], [_, _]]       -> FullHouse cards
    [[_, _, _], [_], [_]]     -> ThreeOf cards
    [[_, _], [_, _], [_]]     -> TwoPair cards
    [[_, _], [_], [_], [_]]   -> OnePair cards
    [[_], [_], [_], [_], [_]] -> High cards

groupings :: [Card] -> [[Card]]
groupings cards =
  sortBy (compare `on` (* (-1)) . length) $ group $ sort cards

transformJacks :: [Card] -> [Card]
transformJacks = map (\c -> if c == Jack then Joker else c)

groupings' :: [Card] -> [[Card]]
groupings' cards =
  let (jokers, others) = partition (== Joker) cards
      groups = groupings others
  in case groups of
    [] -> [jokers]
    _ -> (jokers ++ head groups) : tail groups

-- Parser

parseFile :: Parser [([Card], Bid)]
parseFile = many1 parseRow

parseRow :: Parser ([Card], Bid)
parseRow = do
  cards <- count 5 parseCard
  _ <- char ' '
  bid <- read <$> many1 digit
  _ <- char '\n'
  pure (cards, Bid bid)

parseCard :: Parser Card
parseCard = do
  cardChar <- anyChar
  case cardChar of
    'A' -> pure Ace
    'K' -> pure King
    'Q' -> pure Queen
    'J' -> pure Jack
    'T' -> pure Ten
    '9' -> pure Nine
    '8' -> pure Eight
    '7' -> pure Seven
    '6' -> pure Six
    '5' -> pure Five
    '4' -> pure Four
    '3' -> pure Three
    '2' -> pure Two
    '1' -> pure One
    _ -> fail "Invalid card"
