module Solutions.Day23 (solution, test) where

import Text.Megaparsec.Char (alphaNumChar, char, eol)

import Data.List (intercalate, isPrefixOf, nub, sort)
import Data.List.Extra (maximumOn)
import Data.Map ((!))
import qualified Data.Map as M
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
part2 input = return $ intercalate "," $ S.toList $ maximumOn S.size $ findLargestCluster m <$> keys
 where
  m = toMap input
  keys = M.keys m

findLargestCluster :: M.Map String [String] -> String -> S.Set String
findLargestCluster m from = findCluster (S.fromList (from : a)) [] from
 where
  a = m ! from
  findCluster :: S.Set String -> [String] -> String -> S.Set String
  findCluster potentialCluster currentChain cur
    | cur `elem` currentChain = S.fromList currentChain
    | not (S.member cur potentialCluster) = S.empty
    | null toCheck = S.insert cur $ S.fromList currentChain
    | otherwise = maximumOn S.size (findCluster nextCluster (cur : currentChain) <$> toCheck)
   where
    connectedTo = S.intersection potentialCluster $ S.fromList (m ! cur)
    connectedToAndSelf = S.intersection potentialCluster $ S.fromList $ cur : (m ! cur)
    nextCluster = S.intersection potentialCluster connectedToAndSelf
    toCheck = S.toList $ S.filter (`notElem` currentChain) connectedTo

-- b = S.fromList . (\x -> x : (m ! x)) <$> a
-- c = foldr S.intersection (S.fromList (from : a)) (debug b)

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
