-- Count how many times each word occurs in a document.
-- Usage:
-- stack build
-- echo "test one test two test three" | stack exec wordcount
{-# LANGUAGE TemplateHaskell #-}
{-# Language OverloadedStrings #-}

module WordCount where

import Control.Distributed.Process.Closure

import MapReduce

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import System.Exit

type Docid = Int
type Term = B.ByteString
type Count = Int

-- Mapper takes input of (docid, docContents); outputs list of (word, count=1) tuples
countWords :: (Docid, B.ByteString) -> [(Term, Count)]
countWords (fileIndex, fileContents) = map (\word -> (word, 1)) (B.words fileContents)


-- Reducer takes key=word and list of counts as input, outputs total count of that word
sumCounts :: Term -> [Count] -> Count
sumCounts _ counts = sum counts


-- Wrappers for passing closure and types dictionary to distributed-process
countWordsWrapper :: () -> MapReduce.Mapper Int B.ByteString B.ByteString Int
countWordsWrapper () = countWords

remotable ['countWordsWrapper]

countWordsMapperClosure = ($(mkClosure 'countWordsWrapper) ())


-- Main - parse input file, then pretty-print the result using single-node map-reduce
main :: IO ()
main = do
    -- take in input
    --lines <- fmap Text.lines (Text.readFile "test.txt")
    fileContents <- B.getContents
    -- split input into words based on whitespace delimiter
    --let docsAndWords = zip (repeat (0::Int)) (B.words fileContents)
    let docsAndWords = Map.fromList [(0::Int, fileContents)]

    -- run word count on the input
    let wordCount = mapReduce countWords sumCounts docsAndWords
    putStrLn (show wordCount) >> exitSuccess
