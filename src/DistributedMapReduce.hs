{-# LANGUAGE TemplateHaskell #-}

module DistributedMapReduce where

import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, forM_, replicateM)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified MapReduce


-- Mapper takes input of (docid, docContents); outputs list of (word, count=1) tuples
countWords :: (Int, B.ByteString) -> [(B.ByteString, Int)]
countWords (fileIndex, fileContents) = map (\word -> (word, 1)) (B.words fileContents)

countWordsWrapper :: () -> MapReduce.Mapper Int B.ByteString B.ByteString Int
countWordsWrapper () = countWords


-- Forward the provided number to "recipient"
-- TODO: make Mapper take more generic types, eg: Serializable
mapperWorker :: (ProcessId, ProcessId, Closure (MapReduce.Mapper Int B.ByteString B.ByteString Int)) -> Process ()
mapperWorker (master, workQueue, mapperClosure) = do
  me <- getSelfPid
  mapper <- unClosure mapperClosure
  requestWork me master workQueue mapper

requestWork me master workQueue mapper = do
  -- request work from master's queue
  send workQueue me
  -- execute work task, otherwise terminate
  receiveWait
    [ match $ \(key, value) -> do
        liftIO . putStrLn $ "Slave executing mapper for " ++ (show (key::Int))
        let result = mapper (key, value)
        send master result >> requestWork me master workQueue mapper
    , match $ \() -> return ()
    , matchAny $ \message -> do
        liftIO . putStrLn $ "Slave received unknown message " ++ (show message)
        return ()
    ]

logMessage :: String -> Process ()
logMessage msg = do 
  liftIO $ putStrLn msg
  say $ "handling " ++ msg


remotable ['mapperWorker, 'countWordsWrapper]


master :: Backend -> [NodeId] -> Process Int
master backend slaves = do
  me <- getSelfPid
  -- Print list of slaves
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  let numTasks = 10::Int
  let inputs = zip [1 .. numTasks] (cycle $ map B.pack ["a","b","c"])
  liftIO . putStrLn $ "Initialize WorkQueue"
  workQueue <- spawnLocal $ do
    -- generate list of tasks
    forM_ inputs $ \(key, value) -> do
      -- wait for worker to request next task
      worker <- expect
      liftIO . putStrLn $ "Send task " ++ (show (key, value)) ++ " to worker " ++ (show worker)
      send worker ((key, value)::(Int,B.ByteString))

    -- when workers finished processing work, kill them
    forever $ do
      worker <- expect
      send worker ()
      
  -- Start mapperWorker process on all slaves
  let mapperClosure = ($(mkClosure 'countWordsWrapper) ())
  spawnLocal $ forM_ slaves $ startListener me workQueue mapperClosure
    
  -- Terminate the slaves when the master terminates (this is optional)
  --liftIO $ threadDelay 2000000
  --terminateAllSlaves backend
  getReplies numTasks []
  --partials <- replicateM numTasks expect
  --forM_ partials $ \result -> do
  --  liftIO $ print result
  --return 0


-- start mapperWorker process on slave which pulls tasks from master's workQueue
startListener me workQueue mapperClosure slave = do
  them <- spawn slave ($(mkClosure 'mapperWorker) (me, workQueue, mapperClosure))
  reconnect them


-- Wait for reply from all slaves
getReplies :: Int -> [(B.ByteString, [Int])] -> Process Int
getReplies repliesRemaining accumulated = wait repliesRemaining accumulated
  where
    wait :: Int -> [(B.ByteString, [Int])]-> Process Int
    wait 0 _ = return 0
      -- reduce reducer . groupByKey
    wait repliesRemaining sofar = do
      keyValuePairs <- expect :: Process [(B.ByteString, Int)]
      liftIO . putStrLn $ "Master Received Reply:" ++ (show keyValuePairs)
      let valuesToList = asList keyValuePairs
      wait (repliesRemaining - 1) (sofar ++ valuesToList)

asList keyValuePairs = [(key, [value]) | (key, value) <- keyValuePairs]
