{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

-- Immutable array
import Data.Array.IArray
import Data.List (transpose)
import Data.Maybe (fromJust, isNothing)

data Move = Move {quantity :: Int, from :: Int, to :: Int} deriving Show
type Stack = [Char]

main :: IO ()
main = do
  contents <- TIO.readFile "input"
  let lines = T.lines contents
      (stacks, rest) = parseStacks lines
      withoutDivider = drop 2 rest
  moves <- either (\_ -> fail "Failed to parse moves") pure $ parseMoves withoutDivider
  let stacks' = solve stacks moves
      tops = elems . amap head $ stacks'
  printAnswer tops
  let stacks'' = solve' stacks moves
      tops' = elems . amap head $ stacks''
  printAnswer tops'

parseStacks :: [T.Text] -> (Array Int Stack, [T.Text])
parseStacks lines =
  let (boxLines, restLines) = break (T.isPrefixOf " 1 ") lines
      boxes = map parseBoxes boxLines
      boxes' = transpose boxes
      boxes'' = map (map fromJust) . map (dropWhile isNothing) $ boxes' in
    (listArray (1, (length boxes'')) boxes'', restLines)

parseBoxes :: T.Text -> [Maybe Char]
parseBoxes line =
  map (toMaybeChar . T.take 3) $ T.chunksOf 4 line
  where toMaybeChar "   " = Nothing
        toMaybeChar box = Just (T.index box 1)

parseMoves :: [T.Text] -> Either String [Move]
parseMoves lines =
  mapM parseMove lines

parseMove :: T.Text -> Either String Move
parseMove line = do
  let withoutMove = T.drop 5 line
  (q, afterQ) <- TR.decimal withoutMove
  let withoutFrom = T.drop 6 afterQ
  (f, afterFrom) <- TR.decimal withoutFrom
  let withoutTo = T.drop 4 afterFrom
  (t, _) <- TR.decimal withoutTo
  return (Move q f t)

solve :: Array Int Stack -> [Move] -> Array Int Stack
solve stacks moves =
  foldl applyMove stacks moves

solve' :: Array Int Stack -> [Move] -> Array Int Stack
solve' stacks moves =
  foldl applyMove' stacks moves

applyMove :: Array Int Stack -> Move -> Array Int Stack
applyMove stacks (Move q f t) =
      let (moved, newFrom) = splitAt q (stacks ! f)
          oldTo = (stacks ! t)
          newTo = foldl (flip (:)) oldTo moved in
        stacks // [(f, newFrom), (t, newTo)]

applyMove' :: Array Int Stack -> Move -> Array Int Stack
applyMove' stacks (Move q f t) =
      let (moved, newFrom) = splitAt q (stacks ! f)
          oldTo = (stacks ! t)
          newTo = moved ++ oldTo in
        stacks // [(f, newFrom), (t, newTo)]

printAnswer :: [Char] -> IO ()
printAnswer tops = do
  mapM_ putChar tops
  putStr "\n"
