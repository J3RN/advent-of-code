import qualified Data.List   as List
import           Debug.Trace (traceShowId)
import           GHC.Num     (integerLogBase)

main :: IO ()
main = do
  initialStones <- map read . words <$> readFile "input"
  print . sum . map (lengthAfterBlinks 25) $ initialStones
  print . sum . map (traceShowId . lengthAfterBlinks 75) $ initialStones

lengthAfterBlinks :: Int -> Integer -> Integer
lengthAfterBlinks 0 _ = 1
lengthAfterBlinks blinks 0 = lengthAfterBlinks (blinks - 1) 1
lengthAfterBlinks blinks s | even (numDigits s) = let (before, after) = splitNum s
                                                   in lengthAfterBlinks (blinks - 1) before + lengthAfterBlinks (blinks - 1) after
lengthAfterBlinks blinks s = lengthAfterBlinks (blinks - 1) (s * 2024)

-- blink :: [Integer] -> [Integer]
-- blink = concatMap blinkStone

-- blinkStone :: Integer -> [Integer]
-- blinkStone 0 = [1]
-- blinkStone s | even (numDigits s) = splitNum s
-- blinkStone s = [2024 * s]

splitNum :: Integer -> (Integer, Integer)
splitNum i = (i `div` midpoint, i `mod` midpoint)
  where midpoint = 10 ^ (numDigits i `div` 2)

numDigits :: Integer -> Word
numDigits = (+ 1) . integerLogBase 10

reapply :: Int -> (a -> a) -> a -> a
reapply n f x = iterate f x !! n
