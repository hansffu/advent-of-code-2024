{-# LANGUAGE DataKinds #-}

module Solutions.Day6 (solution) where

import Text.Megaparsec.Char (char, eol)

import Data.Array (bounds, (!))
import Data.Foldable (find)
import Data.List.HT (lengthAtLeast, lengthAtMost)
import Data.List.Utils (join)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (Array2d, index2d, replaceArr2D, toArray2d)
import Text.Megaparsec (many, (<|>))

type Input = [[Tile]]
type GuardPosition = (Int, Int, Direction)

data Direction = U | R | D | L deriving (Show, Enum, Bounded, Eq)
data Tile = Empty | Obstacle | Guard Direction deriving (Show)

solution :: Solution Input Int Int
solution = Solution 6 parser part1 part2

part1 :: Input -> IO Int
part1 input =
  return $ length $ S.fromList $ (\(y, x, _) -> (y, x)) <$> guardPath
 where
  arr = toArray2d input
  (guard, _) = fromMaybe (error "") $ find (isGuard . snd) $ join mempty $ index2d input
  guardPath = shortestGuardPath arr guard

part2 :: Input -> IO Int
part2 input = do
  let possibleMaps =
        ( \(y, x) ->
            let path = shortestGuardPath (replaceArr2D y x Obstacle tiles) guard
             in if lengthAtLeast (length input * length (head input)) path then Just (y, x) else Nothing
        )
          <$> S.toList relevantTiles
  return $ length $ catMaybes possibleMaps
 where
  tiles = toArray2d input
  (guard, _) = fromMaybe (error "") $ find (isGuard . snd) $ join mempty $ index2d input
  guardPath = shortestGuardPath tiles guard
  relevantTiles = S.fromList $ filter (/= guard) $ (\(y, x, _) -> (y, x)) <$> guardPath

shortestGuardPath :: Array2d Tile -> (Int, Int) -> [GuardPosition]
shortestGuardPath arr (y, x) = moveGuard arr (y, x, U)

moveGuard :: Array2d Tile -> GuardPosition -> [GuardPosition]
moveGuard tiles (y, x, dir)
  | oob = [(y, x, dir)]
  | hitObstacle (next dir) = moveGuard tiles (y, x, wrapSucc dir)
  | otherwise = (y, x, dir) : let (y', x') = next dir in moveGuard tiles (y', x', dir)
 where
  next :: Direction -> (Int, Int)
  next U = (y - 1, x)
  next D = (y + 1, x)
  next L = (y, x - 1)
  next R = (y, x + 1)
  hitObstacle (y', x') = isObstacle $ tiles ! y' ! x'
  oob =
    let (y', x') = next dir
     in x' < 0 || y' < 0 || y' > snd (bounds tiles) || x' > snd (bounds (tiles ! 0))

isGuard :: Tile -> Bool
isGuard (Guard _) = True
isGuard _ = False

isObstacle :: Tile -> Bool
isObstacle Obstacle = True
isObstacle _ = False

wrapSucc :: (Eq a, Enum a, Bounded a) => a -> a
wrapSucc x
  | x == maxBound = minBound
  | otherwise = succ x

parser :: Parser Input
parser = many lineP
 where
  lineP = many (emp <|> obstacle <|> up <|> down <|> left <|> right) <* eol
  emp = Empty <$ char '.'
  obstacle = Obstacle <$ char '#'
  up = Guard U <$ char '^'
  down = Guard D <$ char 'v'
  left = Guard L <$ char '<'
  right = Guard R <$ char '>'

-- emp = char '.' >> return (const Empty)

test = testSolution solution
