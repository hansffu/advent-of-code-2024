{-# LANGUAGE DataKinds #-}

module Solutions.Day6 (solution) where

import Text.Megaparsec.Char (char, eol)

import Data.Array (bounds, (!))
import Data.Foldable (find)
import Data.List.Utils (join)
import Data.Maybe (fromMaybe)
import Data.Set (fromList)
import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (Array2d, index2d, toArray2d)
import Text.Megaparsec (many, (<|>))

type Input = [[Tile]]
type GuardPosition = (Int, Int, Direction)

data Direction = U | R | D | L deriving (Show, Enum, Bounded, Eq)
data Tile = Empty | Obstacle | Guard Direction deriving (Show)

solution :: Solution Input String String
solution = Solution 6 parser part1 part2

part1 :: Input -> IO String
part1 input = do
  print guard
  let a = moveGuard arr (fst (fst guard), snd (fst guard), U)
  print a
  print obstacles
  let locations = fromList $ (\(y, x, _) -> (y, x)) <$> a
  return $ show $ length locations
 where
  indexed = index2d input
  arr = toArray2d input
  guard = fromMaybe (error "") $ find (isGuard . snd) $ join mempty indexed
  obstacles = filter (isObstacle . snd) $ join mempty indexed

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

-- oob U = snd (next U) == 0
-- oob D = snd (next D) == length tiles - 1
-- oob L = fst (next L) == 0
-- oob R = fst (next R) == length (tiles ! 1) - 1

isGuard :: Tile -> Bool
isGuard (Guard _) = True
isGuard _ = False

isObstacle :: Tile -> Bool
isObstacle Obstacle = True
isObstacle _ = False

part2 :: Input -> IO String
part2 = todo

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
