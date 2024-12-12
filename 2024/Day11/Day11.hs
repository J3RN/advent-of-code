import qualified Data.List   as List
import           Debug.Trace (traceShowId)
import           GHC.Num     (integerLogBase)

main :: IO ()
main = do
  initialStones <- map read . words <$> readFile "input"
  print . sum . map (length . reapply 25 (traceShowId . blink) . List.singleton) $ initialStones
  -- print . sum . map (length . reapply 75 blink . List.singleton) $ initialStones

blink :: [Integer] -> [Integer]
blink = concatMap blinkStone

blinkStone :: Integer -> [Integer]
blinkStone 0 = [1]
blinkStone s | even (numDigits s) = splitNum s
blinkStone s = [2024 * s]

splitNum :: Integer -> [Integer]
splitNum i = [i `div` midpoint, i `mod` midpoint]
  where midpoint = 10 ^ (numDigits i `div` 2)

numDigits :: Integer -> Word
numDigits = (+ 1) . integerLogBase 10

reapply :: Int -> (a -> a) -> a -> a
reapply n f x = iterate f x !! n
