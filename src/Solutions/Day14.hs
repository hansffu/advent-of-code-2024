module Solutions.Day14 (solution, test) where

import Text.Megaparsec.Char (char, eol, string)

import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many, optional)
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution Input Int String
solution = Solution 14 parser (part1 (101, 103)) part2

type RobotSpec = ((Int, Int), (Int, Int))
type Bounds = (Int, Int)
type Position = (Int, Int)

part1 :: Bounds -> Input -> IO Int
part1 bounds input = do
  print finalPositions
  return $ product $ length <$> qs
 where
  finalPositions = posAfter bounds 100 <$> input
  qs = groupByQuadrant bounds finalPositions

part2 :: Input -> IO String
part2 = todo

type Quadrant = ([Position], [Position], [Position], [Position])
groupByQuadrant :: Bounds -> [Position] -> [[Position]]
groupByQuadrant (bx, by) = t2l . foldr go ([], [], [], [])
 where
  t2l (a, b, c, d) = [a, b, c, d]
  go :: Position -> Quadrant -> Quadrant
  go p@(x, y) q@(a, b, c, d)
    | x < bx `div` 2 && y < by `div` 2 = (p : a, b, c, d)
    | x < bx `div` 2 && y > by `div` 2 = (a, b, p : c, d)
    | x > bx `div` 2 && y < by `div` 2 = (a, p : b, c, d)
    | x > bx `div` 2 && y > by `div` 2 = (a, b, c, p : d)
    | otherwise = q

posAfter :: Bounds -> Int -> RobotSpec -> Position
posAfter (bx, by) n ((px, py), (vx, vy)) = (x `mod` bx, y `mod` by)
 where
  (x, y) = (px + vx * n, py + vy * n)

type Input = [RobotSpec]
parser :: Parser Input
parser = many lineP
 where
  lineP = do
    _ <- string "p="
    px <- negativeDecimal <* char ','
    py <- negativeDecimal <* string " v="
    vx <- negativeDecimal <* char ','
    vy <- negativeDecimal <* eol
    return ((px, py), (vx, vy))
  negativeDecimal = do
    sign <- optional $ char '-'
    n <- decimal
    return $ case sign of
      Just _ -> negate n
      Nothing -> n

test :: IO (Int, String)
test = testSolution $ Solution 14 parser (part1 bounds) part2
 where
  bounds = (11, 7)
