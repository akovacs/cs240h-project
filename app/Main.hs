{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment (getArgs)
import System.Exit
import System.Directory (getDirectoryContents, canonicalizePath)
import System.FilePath 
import System.FilePath.Posix (takeExtension)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (mapM)
import qualified DistributedMapReduce hiding (__remoteTable)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified DistributedMapReduce
import qualified MapReduce
import qualified WordCount

myRemoteTable :: RemoteTable
myRemoteTable = DistributedMapReduce.__remoteTable 
              . WordCount.__remoteTable
              $ initRemoteTable

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir = getDirectoryContents dir >>= mapM (canonicalizePath . (dir </>))

-- Read text corpus from filesystem
readFiles :: FilePath -> IO (Map.Map Int B.ByteString)
readFiles dirPath = do
  corpus <- getAbsDirectoryContents dirPath
  let txtFiles = filter (( ==".txt") . takeExtension) corpus
  --putStrLn (show txtFiles)
  fileContents <- mapM B.readFile txtFiles
  return $ Map.fromList (zip (map fromIntegral [0..]) fileContents)


-- Different ways to run MapReduce
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      docsAndWords <- liftIO $ readFiles "corpus"
      liftIO . putStrLn $ "Read corpus containing " ++ (show (Map.size docsAndWords)) ++ " documents."
      backend <- initializeBackend host port myRemoteTable
      startMaster backend $ \slaves -> do
        result <- DistributedMapReduce.master backend slaves WordCount.countWordsMapperClosure WordCount.sumCounts docsAndWords
        liftIO $ print result
    ["slave", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startSlave backend
    ["standalone"] -> do
      -- take in input
      -- split input into words based on whitespace delimiter
      docsAndWords <- readFiles "corpus"
      liftIO . putStrLn $ "Read corpus containing " ++ (show (Map.size docsAndWords)) ++ " documents."

      -- run word count on the input
      let wordCount = MapReduce.mapReduce WordCount.countWords WordCount.sumCounts docsAndWords
      putStrLn (show wordCount) >> exitSuccess
