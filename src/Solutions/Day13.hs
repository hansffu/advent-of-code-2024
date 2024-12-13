module Solutions.Day13 (solution, test) where

import Text.Megaparsec.Char (eol, newline, string)

import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (both)
import Lib.Parser (Parser)
import Lib.Solution hiding (solve)
import Text.Megaparsec (many, sepBy, try, (<|>))
import Text.Megaparsec.Char.Lexer (decimal)

solution :: Solution Input Int Int
solution = Solution 13 parser part1 part2

part1 :: Input -> IO Int
part1 input = return $ sum $ costToWin <$> filter lessThan101 winCombinations
 where
  winCombinations = mapMaybe (uncurry solve) input
  lessThan101 (a, b) = a <= 100 && b <= 100
  costToWin (a, b) = 3 * a + b

part2 :: Input -> IO Int
part2 input = return $ sum $ costToWin <$> winCombinations
 where
  winCombinations = mapMaybe (uncurry solve . both adjustPrize) input
  adjustPrize (a, b, c) = (a, b, c + 10000000000000)
  costToWin (a, b) = 3 * a + b

type Input = [(Equation, Equation)]
parser :: Parser Input
parser = groupP `sepBy` many newline
 where
  groupP :: Parser (Equation, Equation)
  groupP = do
    (a1, a2) <- buttonP
    (b1, b2) <- buttonP
    (c1, c2) <- priceP <* eol
    return ((a1, b1, c1), (a2, b2, c2))
  buttonP = do
    _ <- try (string "Button A: X+") <|> string "Button B: X+"
    x <- decimal <* string ", Y+"
    y <- decimal <* eol
    return (x, y)
  priceP = do
    _ <- string "Prize: X="
    x <- decimal <* string ", Y="
    y <- decimal
    return (x, y)

test :: IO (Int, Int)
test = testSolution solution

type Equation = (Double, Double, Double)

solve :: Equation -> Equation -> Maybe (Int, Int)
solve eq1@(a1, b1, c1) (a2, b2, c2) = if hasDecimals x || hasDecimals y then Nothing else Just (floor x, floor y)
 where
  {--
     a1x + b1y = c1
     a2x + b2y = c2
     x = (c1-b1y)/a1
     a2((c1-b1y)/a1) + b2y = c2
     a2(c1/a1 - b1y/a1) b2y = c2
     a2*c1/a1 - (a2*b1/a1)y + b2y = c2
     (b2 - (a2*b1/a1))y = c2-a2*c1/a1
     y = (c2-a2*c1/a1)/(b2-a2*b1/a1)
     y = (c2*a1-a2*c1)/(b2*a1-a2*b1)
  --}
  y = (c2 * a1 - a2 * c1) / (b2 * a1 - a2 * b1)
  x = solveForY eq1 y

hasDecimals :: Double -> Bool
hasDecimals n = n /= fromIntegral (floor n :: Int)

solveForY :: Equation -> Double -> Double
solveForY (a, b, c) x = (c - b * x) / a
