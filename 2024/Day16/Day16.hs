import           Data.Text    (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec

type Point = (Int, Int)
data Direction = North | East | South | West

main :: IO ()
main = do
  _ <- parseFromFile parseFile "input" >>= either (fail . show) pure
  _

--

data ParseState = ParseState { _row   :: Int
                             , _col   :: Int
                             , _start :: Point
                             , _end   :: Point
                             }

type Parser = Parsec Text ParseState


parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p f = runParser p initialState f <$> TIO.readFile f
  where initialState = ParseState 0 0 (0,0) (0,0)

parseFile :: Parser a0
parseFile = _
