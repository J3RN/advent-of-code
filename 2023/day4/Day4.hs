import Data.List (intersect)
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.String (Parser)

data Card = Card { _winningNums :: [Int]
                 , _yourNums    :: [Int]
                 }
            deriving Show

main :: IO ()
main = do
  contents <- readFile "input"
  cards <- either (fail . show) pure $ runParser parseCards () "input" contents
  let cardScores = map scoreCard cards
  print $ sum cardScores
  let initialSet = Map.fromList $ zipWith (\i c -> (i, (c, 1))) [1..] cards
      processedSet = foldl processCard initialSet [1..length cards]
  print $ countCards processedSet

countCards :: Map Int (Card, Int) -> Int
countCards = sum . map snd . Map.elems

processCard :: Map Int (Card, Int) -> Int -> Map Int (Card, Int)
processCard cardSet index =
  let (card, c) = cardSet Map.! index
      correct = numCorrect card
      updatedSet = foldl (updateCount c) cardSet [index + 1..index + correct]
  in updatedSet
  where updateCount amount cardSet' i = Map.update (\(card, c') -> Just (card, c' + amount)) i cardSet'

scoreCard :: Card -> Int
scoreCard card =
  let correct = numCorrect card
  in if correct == 0
     then 0
     else 2 ^ (correct - 1)

numCorrect :: Card -> Int
numCorrect card = length $ intersect (_winningNums card) (_yourNums card)

-- Parser

parseCards :: Parser [Card]
parseCards = sepEndBy parseCard (char '\n')

parseCard :: Parser Card
parseCard = do
  _ <- string "Card" *> many1 (char ' ') *> many1 digit *> string ":" *> many1 (char ' ')
  winningNums <- sepEndBy (read <$> many1 digit) (many1 (char ' '))
  _ <- string "|" *> many1 (char ' ')
  yourNums <- sepEndBy (read <$> many1 digit) (many1 (char ' '))
  return Card {_winningNums = winningNums, _yourNums = yourNums}
