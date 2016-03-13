-- Term-Frequency Inverse-Document-Frequency
-- Usage:
-- stack build
-- echo "test one test two test three" | stack exec wordcount
{-# LANGUAGE TemplateHaskell #-}
{-# Language OverloadedStrings #-}

module TfIdf where

import Control.Distributed.Process
import Control.Distributed.Process.Closure

import MapReduce

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import System.Exit

type Document = B.ByteString
type Word = B.ByteString
type Count = Int

-- 1: Word frequency in document
-- Mapper takes input of (docid, docContents); outputs list of ((word, docid), count=1) tuples
wordDocCount :: (Document, B.ByteString) -> [((Word, Document), Count)]
wordDocCount (fileIndex, fileContents) = map (\word -> ((word, fileIndex), 1)) (B.words fileContents)

wordDocCountWrapper :: () -> MapReduce.Mapper Document B.ByteString (Word, Document) Count
wordDocCountWrapper () = wordDocCount

remotable ['wordDocCountWrapper]

wordDocCountMapperClosure = ($(mkClosure 'wordDocCountWrapper) ())

-- Reducer takes key=(word, docid) and list of counts as input, outputs total count of word in document
sumCounts :: (Word, Document) -> [Count] -> Int
sumCounts _ counts = sum counts


-- 2: Word counts for documents.
-- Mapper takes input of ((word, docid), count); outputs list of (docid, (word, count)) tuples
-- Rearrange so that key is document
docWordCount :: ((Word, Document), Count) -> [(Document, (Word, Count))]
docWordCount ((word, doc), count) = [(doc, (word, count))]

-- Reducer sums frequency of counts in each doc, outputting (word++docid, (wordcount, doccount))
-- output (word count, total number of words in document)
sumCountsPerDoc :: Document -> [(Word, Count)] -> (Count, Count)
sumCounts _ wordCountTuples = sum counts


-- Main - parse input file, then pretty-print the result using single-node map-reduce
--main :: IO ()
--main = do
--    -- take in input
--    --lines <- fmap Text.lines (Text.readFile "test.txt")
--    fileContents <- B.getContents
--    -- split input into words based on whitespace delimiter
--    --let docsAndWords = zip (repeat (0::Int)) (B.words fileContents)
--    let docsAndWords = Map.fromList [(0::Int, fileContents)]
--
--    -- run word count on the input
--    let wordCount = mapReduce countWords sumCounts docsAndWords
--    putStrLn (show wordCount) >> exitSuccess
