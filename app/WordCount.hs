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
    -- split input into words based on whitespace delimiter
    --let docsAndWords = zip (repeat (0::Int)) (B.words fileContents)
    let docsAndWords = [(0::Int, fileContents)]

    -- run word count on the input
    let wordCount = mapReduce countWords sumCounts docsAndWords
    putStrLn (show wordCount) >> exitSuccess


-- Mapper takes input of (docid, docContents); outputs list of (word, count=1) tuples
countWords :: (Int, B.ByteString) -> [(B.ByteString, Int)]
countWords (fileIndex, fileContents) = map (\word -> (word, 1)) (B.words fileContents)

-- Reducer takes key=word and list of counts as input, outputs total count of that word
sumCounts :: B.ByteString -> [Int] -> Int
sumCounts _ counts = sum counts
