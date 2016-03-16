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


-- Request new work unit from the master's workQueue, apply the
-- mapper closure to it, then send back to master the result of
-- executing the mapper.
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

remotable ['mapperWorker]


master :: Backend -> [NodeId]
                  -> Closure (MapReduce.Mapper Int B.ByteString B.ByteString Int)
                  -> MapReduce.Reducer B.ByteString Int
                  -> Map.Map Int B.ByteString
                  -> Process (Map.Map B.ByteString Int)
master backend slaves mapperClosure reducer inputs = do
  me <- getSelfPid
  -- Print list of slaves
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  let numTasks = Map.size inputs
  liftIO . putStrLn $ "Initialize WorkQueue"
  workQueue <- spawnLocal $ do
    -- generate list of tasks
    forM_ (Map.toList inputs) $ \(key, value) -> do
      -- wait for worker to request next task
      worker <- expect
      liftIO . putStrLn $ "Send task " ++ (show key) ++ " to worker " ++ (show worker)
      send worker ((key, value)::(Int,B.ByteString))

    -- when workers finished processing work, kill them
    forever $ do
      worker <- expect
      send worker ()
      
  -- Start mapperWorker process on all slaves
  spawnLocal $ forM_ slaves $ startListener me workQueue mapperClosure
    
  -- Terminate the slaves when the master terminates (this is optional)
  groupedByKey <- getReplies numTasks []
  -- return $ reduce reducer groupedByKey
  let result = reduce reducer groupedByKey
  liftIO . putStrLn $ "Master reduces result to " ++ (show result)
  return $ result


-- start mapperWorker process on slave which pulls tasks from master's workQueue
startListener me workQueue mapperClosure slave = do
  them <- spawn slave ($(mkClosure 'mapperWorker) (me, workQueue, mapperClosure))
  reconnect them


-- Wait for reply from all slaves
getReplies :: Int -> [(B.ByteString, [Int])] -> Process (Map.Map B.ByteString [Int])
getReplies repliesRemaining accumulated = wait repliesRemaining accumulated
  where
    wait :: Int -> [(B.ByteString, [Int])]-> Process (Map.Map B.ByteString [Int])
    wait 0 fullResults = return $ groupByKey fullResults
      -- reduce reducer . groupByKey
    wait repliesRemaining partialResults = do
      keyValuePairs <- expect :: Process [(B.ByteString, Int)]
      liftIO . putStrLn $ "Master Received Reply:" ++ (show keyValuePairs)
      let addedValues = asList keyValuePairs
      wait (repliesRemaining - 1) (partialResults ++ addedValues)


reduce reducer = Map.mapWithKey reducer

groupByKey = Map.fromListWith (++)

asList keyValuePairs = [(key, [value]) | (key, value) <- keyValuePairs]
