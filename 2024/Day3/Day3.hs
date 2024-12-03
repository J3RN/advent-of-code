import           Control.Applicative (liftA2, liftA3)
import           Data.Functor        (($>))
import           Data.Maybe          (catMaybes)
import           Data.Text           (Text)
import qualified Data.Text.IO        as T
import           Text.Parsec

type Parser = Parsec Text Bool

main :: IO ()
main = do
  mulResult <- parseFromFile parseFile "input"
  either
    (fail . show)
    (print . sum . map (uncurry (*)))
    mulResult
  mulResult' <- parseFromFile parseFile' "input"
  either
    (fail . show)
    (print . sum . map (uncurry (*)))
    mulResult'

-- Regex would've been nicer, but oh well.
parseFile :: Parser [(Int, Int)]
parseFile =
  many (satisfy (/= 'm'))
  *> (catMaybes
      <$> manyTill (((Just <$> try parseMul) <|> (char 'm' *> pure Nothing)) <* many (satisfy (/= 'm'))) eof)


parseFile' :: Parser [(Int, Int)]
parseFile' =
  many notInteresting
  *> (catMaybes
       <$> manyTill (((try parseEnable $> Nothing)
                        <|> (try parseDisable $> Nothing)
                        <|> liftA3 if' getState (Just <$> try parseMul) (pure Nothing)
                        <|> ((char 'd' $> Nothing) <|> (char 'm' $> Nothing))
                       ) <* many notInteresting) eof)
     where notInteresting = satisfy (liftA2 (&&) (/= 'm') (/= 'd'))
           -- I feel insane
           if' True a _  = a
           if' False _ a = a

parseEnable :: Parser ()
parseEnable = string "do()" *> putState True

parseDisable :: Parser ()
parseDisable = string "don't()" *> putState False

parseMul :: Parser (Int, Int)
parseMul = do
  _ <- string "mul("
  d1 <- many1 digit
  _ <- char ','
  d2 <- many1 digit
  _ <- char ')'
  return (read d1, read d2)

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- T.readFile fname
         return (runP p True fname input)
