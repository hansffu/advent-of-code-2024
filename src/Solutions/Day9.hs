module Solutions.Day9 (solution, test) where

import Text.Megaparsec.Char (digitChar)

import Data.Char (digitToInt)
import Data.List (group)
import Data.List.Extra (chunksOf)
import Data.List.HT (takeUntil)
import Debug.Trace (traceShow)
import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (debug)
import System.Directory.Extra ()
import Text.Megaparsec (many)

solution :: Solution Input Int Int
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

part2 :: Input -> IO Int
part2 input = do
  -- print $ group blocks
  -- print $ moveFiles2 blocks
  -- return $ sum $ (\(a, b) -> a * blockId b) <$> relevantBlocks
  -- return 0
  -- print $ concatMap show $ moveFiles2 blocks
  return $ sum $ (\(a, b) -> a * blockId b) <$> relevantBlocks
 where
  blocks = toBlocks input
  sorted = moveFiles2 blocks
  numFileBlocks = length $ filter isFile blocks
  indexed = zip [0 ..] sorted
  relevantBlocks = take numFileBlocks $ filter (isFile . snd) indexed

data Block = FilePart Int | Space deriving (Eq)
instance Show Block where
  show (FilePart n) = show n
  show Space = "."

isFile :: Block -> Bool
isFile (FilePart _) = True
isFile _ = False

isSpace :: Block -> Bool
isSpace = not . isFile

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

moveFiles2 :: [Block] -> [Block]
moveFiles2 blocks = go (reverse $ group blocks) blocks
 where
  go :: [[Block]] -> [Block] -> [Block]
  go ((Space : _) : bs) disk = go bs disk
  go (bs@((FilePart _) : _) : nextBs) disk = go nextBs $ moveFile bs disk
  go [] disk = disk
  go a disk = error $ show a

moveFile :: [Block] -> [Block] -> [Block]
moveFile file disk
  | length disk < length file = disk
  | chunk == file = disk
  | all isSpace chunk = file ++ removeFile file rest
  -- \| null nextPlace = []
  | otherwise = head disk : moveFile file (tail disk)
 where
  fileLen = length file
  (chunk, rest) = splitAt fileLen disk

removeFile :: [Block] -> [Block] -> [Block]
removeFile file disk
  | chunk == file = replicate fileLen Space ++ rest
  | null disk = []
  | otherwise = head disk : removeFile file (tail disk)
 where
  fileLen = length file
  (chunk, rest) = splitAt fileLen disk

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

test :: IO (Int, Int)
test = testSolution solution
