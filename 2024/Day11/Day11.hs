import           Control.Monad       (when)
import           Control.Monad.State
import           Data.Maybe          (fromMaybe)
-- import qualified Data.List           as List
import           Control.Applicative (liftA2)
import           Data.Map.Strict     (Map, (!?))
import qualified Data.Map.Strict     as Map
import           Debug.Trace         (traceShowId)
import           GHC.Num             (integerLogBase)

type MemoCache = Map (Int, Integer) Integer

main :: IO ()
main = do
  initialStones <- map read . words <$> readFile "input"
  print . sum . map (\x -> evalState (lengthAfterBlinks 25 x) Map.empty) $ initialStones
  print . sum . map (\x -> evalState (lengthAfterBlinks 75 x) Map.empty) $ initialStones

lengthAfterBlinks :: Int -> Integer -> State MemoCache Integer
lengthAfterBlinks n s = fromMaybe <$> (lengthAfterBlinks' n s >>= cacheRes (n, s)) <*> gets (!? (n, s))
  where cacheRes key res = do
          when (snd key < 10) (modify (Map.insert key res))
          return res

lengthAfterBlinks' :: Int -> Integer -> State MemoCache Integer
lengthAfterBlinks' 0 _ = return 1
lengthAfterBlinks' blinks 0 = lengthAfterBlinks (blinks - 1) 1
lengthAfterBlinks' blinks s | even (numDigits s) = do
                                let (before, after) = splitNum s
                                beforeRes <- lengthAfterBlinks (blinks - 1) before
                                afterRes <- lengthAfterBlinks (blinks - 1) after
                                return (beforeRes + afterRes)
lengthAfterBlinks' blinks s = lengthAfterBlinks (blinks - 1) (s * 2024)

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
