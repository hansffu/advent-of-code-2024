module Solutions.Day23 (solution, test) where

import Text.Megaparsec.Char (alphaNumChar, char, eol)

import Data.List (intercalate, isPrefixOf, nub, sort, sortOn)
import Data.List.Extra (maximumOn)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Ord
import qualified Data.Set as S
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many)

solution :: Solution Input Int String
solution = Solution 23 parser part1 part2

part1 :: Input -> IO Int
part1 input = return $ length $ nub $ ts >>= findCycle3 m
 where
  m = toMap input
  ts = filter isT $ M.keys m

findCycle3 :: M.Map String [String] -> String -> [[String]]
findCycle3 m t = nub $ sort <$> findCycle'
 where
  findCycle' = do
    a <- m ! t
    b <- m ! a
    c <- m ! b
    if c == t
      then return [a, b, c]
      else []

part2 :: Input -> IO String
part2 input = do
  let (_, _, c) = foldr findLargest (0, S.empty, S.empty) keys
  return $ intercalate "," $ S.toList $ c
 where
  m = toMap input
  keys = sortOn (Down . length) (M.keys m)
  findLargest :: String -> (Int, S.Set String, S.Set String) -> (Int, S.Set String, S.Set String)
  findLargest key acc@(s, ignore, _) =
    let c = findLargestCluster m ignore key
        ignore' = S.insert key ignore
     in if length (m ! key) > s && S.size c > s
          then (S.size c, ignore', c)
          else acc

findLargestCluster :: M.Map String [String] -> S.Set String -> String -> S.Set String
findLargestCluster m ignore from = findCluster (S.difference (S.fromList (from : a)) ignore) [] from
 where
  a = m ! from
  findCluster :: S.Set String -> [String] -> String -> S.Set String
  findCluster potentialCluster currentChain cur
    | cur `elem` currentChain = S.fromList currentChain
    | not (S.member cur potentialCluster) = S.empty
    | S.member cur ignore = S.empty
    | null toCheck = S.insert cur $ S.fromList currentChain
    | otherwise = maximumOn S.size (findCluster nextCluster (cur : currentChain) <$> toCheck)
   where
    connectedTo = S.intersection potentialCluster $ S.fromList (m ! cur)
    connectedToAndSelf = S.intersection potentialCluster $ S.fromList $ cur : (m ! cur)
    nextCluster = S.intersection potentialCluster connectedToAndSelf
    toCheck = S.toList $ S.filter (`notElem` currentChain) connectedTo

toMap :: [(String, String)] -> M.Map String [String]
toMap xs = M.fromListWith (++) (xs >>= (\(a, b) -> [(a, [b]), (b, [a])]))

isT :: String -> Bool
isT = isPrefixOf "t"

type Input = [(String, String)]
parser :: Parser Input
parser = many lineP
 where
  lineP = do
    a <- many alphaNumChar <* char '-'
    b <- many alphaNumChar <* eol
    return (min a b, max a b)

test :: IO (Int, String)
test = testSolution solution
