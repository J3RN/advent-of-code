import           Data.List        (sortBy)
import           Text.Parsec
import           Text.Parsec.Text (Parser, parseFromFile)

type Page = Int
-- newtype Page = Page Int
type Rule = (Page, Page)
type Update = [Page]

main :: IO ()
main = do
  (rules, updates) <- parseFromFile parseFile "input" >>= either (fail . show) pure
  print . sum . map middle . filter (correctUpdate rules) $ updates
  print . sum . map (middle . sortByRules rules) . filter (not . correctUpdate rules) $ updates

sortByRules :: [Rule] -> Update -> Update
sortByRules rules = sortBy (cmp rules)

middle :: Update -> Page
middle update = update !! (length update `div` 2)

correctUpdate :: [Rule] -> Update -> Bool
correctUpdate _ []            = True
correctUpdate _ [_]           = True
correctUpdate rules (h1:h2:t) = (h1 `cmp'` h2) == LT && correctUpdate rules (h2:t)
  where cmp' = cmp rules

cmp :: [Rule] -> Page -> Page -> Ordering
cmp rules a b
  | (a, b) `elem` rules = LT
  | (b, a) `elem` rules = GT
  | otherwise           = EQ

-- Parser

parseFile :: Parser ([Rule], [Update])
parseFile = do
  rules <- parseRules
  updates <- parseUpdates
  _ <- eof
  return (rules, updates)

parseRules :: Parser [Rule]
parseRules = many1 (try parseRule)

parseRule :: Parser Rule
parseRule = do
  p1 <- many1 digit
  _ <- char '|'
  p2 <- many1 digit
  _ <- many space
  return (read p1, read p2)

parseUpdates :: Parser [Update]
parseUpdates = many1 (parseUpdate <* char '\n')

parseUpdate :: Parser Update
parseUpdate = sepBy1 (read <$> many1 digit) (char ',')
