import Data.Array.IArray
import Data.List (nub, transpose)

import Text.Printf


main :: IO ()
main = do
  contents <- readFile "input"
  let l = lines contents
      dims = (length l, length . head $ l)
      matrix = listArray ((1,1), dims) (map ((flip (-)) 48) . map fromEnum . concat $ l) :: Array (Int, Int) Int
  printf "Num visible: %d\n" (allVisible matrix)
  let maxViewScore = maximum . map (\x -> viewScore x matrix) . indices $ matrix
  printf "Max view score: %d\n" maxViewScore

allVisible :: Array (Int, Int) Int -> Int
allVisible matrix =
  let
    (_, dims) = bounds matrix
    rowIxs = rowIndices (fst dims) (snd dims)
    colIxs = transpose rowIxs
    myVisible = visibleOnAxis matrix
    fromEast = myVisible rowIxs
    fromWest = myVisible (map reverse rowIxs)
    fromNorth = myVisible colIxs
    fromSouth = myVisible (map reverse colIxs)
  in
    length . nub . foldl (++) [] $ [fromEast, fromWest, fromNorth, fromSouth]

-- Returns the (ix, value) pairs for all cells visible along their given axis
-- from the direction the indices are given.
visibleOnAxis :: Ord a => Array (Int, Int) a -> [[(Int, Int)]] -> [((Int, Int), a)]
visibleOnAxis matrix ixs =
  foldl (++) [] . map (visibleInRow) . map (\i -> takeBy i matrix) $ ixs

visibleInRow :: Ord a => [((Int, Int), a)] -> [((Int, Int), a)]
visibleInRow l =
  foldl addIfVisible [] l
  where addIfVisible [] (i, thisOne) = [(i, thisOne)]
        addIfVisible ((i1, tallest):rest) (i2, thisOne) =
          if thisOne > tallest
          then (i2, thisOne):(i1, tallest):rest
          else (i1, tallest):rest

-- Returns a list of lists, where each sub-list contains the indices of cells going across a row
rowIndices :: Int -> Int -> [[(Int, Int)]]
rowIndices numRows numCols =
  map (\row -> (map (\col -> (row, col)) [1..numCols])) [1..numRows]

takeBy :: Ix i => [i] -> Array i a -> [(i, a)]
takeBy ixs m =
  map (\i -> (i, m ! i)) ixs

viewScore :: (Int, Int) -> Array (Int, Int) Int -> Int
viewScore (row, col) matrix =
  let (_, dims) = bounds matrix
      height = matrix ! (row, col)
      myScore = scoreOf height matrix
      northScore = myScore [(i, col) | i <- [row-1, row-2..1]]
      southScore = myScore [(i, col) | i <- [row+1, row+2..(fst dims)]]
      eastScore = myScore [(row, i) | i <- [col-1, col-2..1]]
      westScore = myScore [(row, i) | i <- [col+1, col+2..(snd dims)]]
  in
    northScore * southScore * eastScore * westScore

scoreOf :: Int -> Array (Int, Int) Int -> [(Int, Int)] -> Int
scoreOf height matrix ixs =
  resolve . break (>= height) . heightsIn matrix $ ixs

heightsIn :: Array (Int, Int) Int -> [(Int, Int)] -> [Int]
heightsIn matrix ixs = map (matrix !) ixs

resolve :: ([a], [a]) -> Int
resolve (v, []) = length v
resolve (v, _rest) = length v + 1
