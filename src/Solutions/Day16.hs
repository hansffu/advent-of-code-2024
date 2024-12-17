{-# LANGUAGE TupleSections #-}

module Solutions.Day16 (solution, test) where

import Text.Megaparsec.Char (char, eol)

import Control.Monad.State (MonadState (get, put), State, evalState)
import Data.Array ((!))
import Data.Bifunctor (first)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import GHC.Data.TrieMap (filterMaybe)
import Lib.Array2d (Array2d, findIndexes2d, safe2dLookup, toArray2d)
import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (wrapPred, wrapSucc)
import Text.Megaparsec (many, try, (<|>))

solution :: Solution Input Int String
solution = Solution 16 parser part1 part2

part1 :: Input -> IO Int
part1 input = do
  -- prettyPrint input
  -- print $ findIndexes2d isStart input
  -- print $ getNeighbours arr (E, snd start)
  -- print $ fst a
  -- print start
  -- print distances
  return $ evalState (shortestPath arr) (S.empty, distances)
 where
  arr = toArray2d input
  start = (E, snd $ head $ findIndexes2d isStart input)
  paths = snd <$> findIndexes2d (not . isWall) input
  vertexes = [(dir, pos) | dir <- [N ..], pos <- paths]
  distances' = S.fromList $ (999999,) <$> vertexes
  distances = S.insert (0, start) $ S.filter (not . (==) start . snd) distances'

part2 :: Input -> IO String
part2 = todo

type Pos = (Dir, (Int, Int))
data Dir = N | W | S | E deriving (Show, Eq, Enum, Bounded, Ord)

type DijkstraState = (S.Set Pos, S.Set (Int, Pos))

shortestPath :: Array2d Tile -> State DijkstraState Int
shortestPath arr = do
  (dist, next) <- nextNotVisited
  let (y, x) = snd next
  if isEnd (arr ! y ! x)
    then return dist
    else do
      addToVisited next
      let neighbours = first (dist +) <$> getNeighbours arr next
      updateDistances neighbours
      shortestPath arr

nextNotVisited :: State DijkstraState (Int, Pos)
nextNotVisited = do
  (visited, distances) <- get
  return $ S.findMin $ S.filter (\(_, pos) -> not $ S.member pos visited) distances

updateDistances :: [(Int, Pos)] -> State DijkstraState ()
updateDistances updates = do
  (visited, distances) <- get
  -- let distances' = S.filter (\(_, pos) -> pos `notElem` (snd <$> updates)) distances
  let newDistances = S.fromList updates <> distances
  put (visited, newDistances)

addToVisited :: Pos -> State DijkstraState ()
addToVisited pos = do
  (visited, distances) <- get
  put (S.insert pos visited, distances)

getNeighbours :: Array2d Tile -> Pos -> [(Int, Pos)]
getNeighbours arr (dir, p@(y, x)) =
  let forward = case dir of
        N -> (y - 1, x)
        S -> (y + 1, x)
        E -> (y, x + 1)
        W -> (y, x - 1)
      f = (1, (dir, forward)) <$ filterMaybe (not . isWall . fst) (safe2dLookup arr forward)
   in catMaybes
        [ f
        , Just (1000, (wrapSucc dir, p))
        , Just (1000, (wrapPred dir, p))
        ]
data Tile = Wall | Path | End | Start deriving (Show)

isWall :: Tile -> Bool
isWall Wall = True
isWall _ = False

isStart :: Tile -> Bool
isStart Start = True
isStart _ = False

isEnd :: Tile -> Bool
isEnd End = True
isEnd _ = False

type Input = [[Tile]]
parser :: Parser Input
parser = many lineP
 where
  lineP = many (try wallP <|> try pathP <|> try endP <|> startP) <* eol
  wallP = Wall <$ char '#'
  pathP = Path <$ char '.'
  endP = End <$ char 'E'
  startP = Start <$ char 'S'

test :: IO (Int, String)
test = testSolution solution
