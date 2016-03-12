{-# LANGUAGE TemplateHaskell #-}

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import qualified DistributedMapReduce hiding (__remoteTable)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified DistributedMapReduce
import qualified WordCount

myRemoteTable :: RemoteTable
myRemoteTable = DistributedMapReduce.__remoteTable 
              . WordCount.__remoteTable
              $ initRemoteTable

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startMaster backend $ \slaves -> do
        result <- DistributedMapReduce.master backend slaves WordCount.countWordsMapperClosure WordCount.sumCounts
        liftIO $ print result
    ["slave", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startSlave backend
