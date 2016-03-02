-- Count how many times each word occurs in a document.
-- Usage:
-- stack build
-- echo "test one test two test three" | stack exec wordcount
{-# Language OverloadedStrings #-}

module Main where

import MapReduce

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.IO as Text
import System.Exit

-- Main - parse input file, then pretty-print the result
main :: IO ()
main = do
    -- take in input
    --lines <- fmap Text.lines (Text.readFile "test.txt")
    fileContents <- B.getContents
    let words = B.splitWith (\char -> char ==' ' || char =='\n') fileContents

    -- run word count on the input
    let wordCount = mapReduce countWords sumCounts [words]
    putStrLn (show wordCount) >> exitSuccess


-- Mapper takes input, outputs list of (key, value) tuples
countWords :: [B.ByteString] -> [(B.ByteString, Int)]
countWords = map (\word -> (word, 1))

-- Reducer takes key and list of values as input
sumCounts :: B.ByteString -> [Int] -> [Int]
sumCounts _ counts = [sum counts]
