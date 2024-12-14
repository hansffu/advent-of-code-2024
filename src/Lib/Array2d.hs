module Lib.Array2d (
  getNeighbours,
  toArray2d,
  Array2d,
  replaceArr2D,
  Node,
) where

import Data.Array (Array, listArray, (!), (//))
import Data.Array.Base ((!?))
import Data.Maybe (catMaybes)

type Array2d a = Array Int (Array Int a)
type Node a = (a, (Int, Int))

toArray2d :: [[a]] -> Array2d a
toArray2d rows = listArray (0, length rows - 1) $ listArray (0, length (head rows) - 1) <$> rows

replaceArr2D :: Int -> Int -> a -> Array2d a -> Array2d a
replaceArr2D row col newVal arr2d =
  arr2d // [(row, newRow)]
 where
  currentRow = arr2d ! row
  newRow = currentRow // [(col, newVal)]

getNeighbours :: (Char -> Char -> Bool) -> Array2d Char -> Node Char -> [Node Char]
getNeighbours predicate arr (item, (y, x)) =
  filter (predicate item . fst) $
    catMaybes
      [ safe2dLookup arr (y + 1, x)
      , safe2dLookup arr (y - 1, x)
      , safe2dLookup arr (y, x + 1)
      , safe2dLookup arr (y, x - 1)
      ]

safe2dLookup :: Array2d a -> (Int, Int) -> Maybe (a, (Int, Int))
safe2dLookup arr (y, x) = do
  row <- arr !? y
  item <- row !? x
  return (item, (y, x))