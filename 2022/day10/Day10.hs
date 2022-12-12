-- addx V : 2 clocks
-- noop   : 1 clock

import Data.Maybe (fromMaybe)

import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Printf (printf)

data Instruction
  = AddX Int
  | Noop
  deriving Show

data CpuState = CpuState { cycleNum :: Int
                         , registerX :: Int
                         }
                deriving Show

main :: IO ()
main = do
  contents <- readFile "input"
  instructions <- either (fail . show) pure $ runParser parseFile () "input" contents
  let states = scanl emulateOne (CpuState 1 1) instructions
      relevantStates = map (stateAt states) [20, 60..220]
      strengths = map signalStrength relevantStates
  printf "Sum: %d\n" (sum strengths)
  putStrLn . unlines . chunksOf 40 . map charAt . map (stateAt states) $ [1..240]

emulateOne :: CpuState -> Instruction -> CpuState
emulateOne (CpuState c x) Noop = (CpuState (c + 1) x)
emulateOne (CpuState c x) (AddX i) = (CpuState (c + 2) (x + i))

-- We only record the _updates_ to the CpuState, not it's value at every clock.
-- Therefore, we take the last update before the given cycle and apply the given
-- cycle to it.
stateAt :: [CpuState] -> Int -> CpuState
stateAt states c =
  (last . takeWhile ((<= c) . cycleNum) $ states) {cycleNum = c}

signalStrength :: CpuState -> Int
signalStrength (CpuState c x) = c * x

charAt :: CpuState -> Char
charAt (CpuState c x) =
  if overlap c x
  then '#'
  else '.'

overlap :: Int -> Int -> Bool
overlap c x = (c `rem` 40) `elem` [x..x+2]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _n [] = []
chunksOf n l =
  let (h, t) = splitAt n l
  in h:(chunksOf n t)

-- Parser

-- It occurs to me now that my parser functions are always named "parse<thing>", but
-- the stdlib ones are always just named "<thing>".  I'd like to change mine to
-- match, but I think the name collisions would get entirely out of hand.
parseFile :: Parser [Instruction]
parseFile = sepEndBy parseInstruction (char '\n')

parseInstruction :: Parser Instruction
parseInstruction = parseAddX <|> parseNoop

parseAddX :: Parser Instruction
parseAddX = do
  _ <- string "addx "
  sign <- optionMaybe (char '-')
  num <- many1 digit
  return (AddX (read ((fromMaybe ' ' sign):num)))

parseNoop :: Parser Instruction
parseNoop = do
  _ <- string "noop"
  return Noop
