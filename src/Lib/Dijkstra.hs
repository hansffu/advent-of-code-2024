{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Lib.Dijkstra (shortestPath) where

import Control.Monad.State (MonadState (put), State, get, modify, runState)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple.Extra (first)

shortestPath :: (Ord a, Eq a, Show a) => (a -> [(Int, a)]) -> [a] -> a -> a -> Int
shortestPath getNeighbours vertexes start end = fst a
 where
  a =
    runState
      (dijkstra getNeighbours end)
      DijkstraState
        { visited = S.empty
        , distances = M.fromList $ map (,maxBound) vertexes
        , queue = S.singleton (0, start)
        }
data DijkstraState a = DijkstraState
  { visited :: S.Set a
  , distances :: M.Map a Int
  , queue :: S.Set (Int, a)
  }
  deriving (Show)
type St a = State (DijkstraState a)

dijkstra :: (Eq a, Ord a) => (a -> [(Int, a)]) -> a -> St a Int
dijkstra getNeighbours end = do
  (dist, next) <- popQueue
  if next == end
    then return dist
    else do
      markAsVisited next
      updateDistance (dist, next)
      neighbours <- getNotVisitedNeighbours getNeighbours next
      addToQueue $ first (dist +) <$> neighbours
      dijkstra getNeighbours end

popQueue :: St a (Int, a)
popQueue = do
  s <- get
  put $ s{queue = S.drop 1 s.queue}
  return $ S.findMin s.queue

addToQueue :: (Ord a) => [(Int, a)] -> St a ()
addToQueue x = modify (\s -> s{queue = S.union (S.fromList x) s.queue})

updateDistance :: (Ord a) => (Int, a) -> St a ()
updateDistance (dist, pos) = modify (\s -> s{distances = M.insert pos dist $ s.distances})

markAsVisited :: (Ord a) => a -> St a ()
markAsVisited pos = modify (\s -> s{visited = S.insert pos s.visited})

getNotVisitedNeighbours :: (Ord a) => (a -> [(Int, a)]) -> a -> St a [(Int, a)]
getNotVisitedNeighbours getNeighbours pos = do
  s <- get
  return $ filter (\(_, x) -> not $ S.member x $ s.visited) $ getNeighbours pos
