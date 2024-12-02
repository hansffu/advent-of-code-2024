-- | yo
module Lib.TaskRunner (readInput, InputType (..)) where

import Lib.Parser

readInput :: InputType -> Parser i -> IO i
readInput inputType parser = do
  let fileName = getFilename inputType
  content <- readFile fileName
  return $ parseInput parser fileName content

data InputType = Input Int | Sample Int

-- type Task a = [String] -> IO a

-- run :: Task a -> InputType -> IO a
-- run solver inputType = do
--   input <- readInput inputType
--   solver input

getFilename :: InputType -> String
getFilename (Input n) = "input/input." <> show n <> ".txt"
getFilename (Sample n) = "input/sample." <> show n <> ".txt"
