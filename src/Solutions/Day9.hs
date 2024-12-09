module Solutions.Day9 (solution, test) where

import Text.Megaparsec.Char (digitChar)

import Data.Char (digitToInt)
import Data.List.Extra (chunksOf)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many)

solution :: Solution Input Int String
solution = Solution 9 parser part1 part2

part1 :: Input -> IO Int
part1 input = do
  return $ sum $ (\(a, b) -> a * blockId b) <$> relevantBlocks
 where
  blocks = toBlocks input
  sorted = take numFileBlocks $ moveFiles blocks
  numFileBlocks = length $ filter isFile blocks
  indexed = zip [0 ..] sorted
  relevantBlocks = take numFileBlocks $ filter (isFile . snd) indexed

part2 :: Input -> IO String
part2 = todo

data Block = FilePart Int | Space
instance Show Block where
  show (FilePart n) = show n
  show Space = "."

isFile :: Block -> Bool
isFile (FilePart _) = True
isFile _ = False

blockId :: Block -> Int
blockId (FilePart n) = n
blockId Space = 0

moveFiles :: [Block] -> [Block]
moveFiles = go <*> reverse
 where
  go as@(Space : _) (Space : bs) = go as bs
  go (a@(FilePart _) : as) bs = a : go as bs
  go (Space : as) (b@(FilePart _) : bs) = b : go as bs
  go _ _ = []

toBlocks :: [Int] -> [Block]
toBlocks xs = foldr convert [] groups
 where
  groups = zip [0 ..] $ chunksOf 2 xs
  convert (n, a : b : _) acc = replicate a (FilePart n) ++ replicate b Space ++ acc
  convert (n, [a]) acc = replicate a (FilePart n) ++ acc
  convert (_, []) acc = acc

type Input = [Int]
parser :: Parser Input
parser = many $ digitToInt <$> digitChar

test :: IO (Int, String)
test = testSolution solution
