{-# LANGUAGE TemplateHaskell #-}

module DistributedMapReduce where

import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, forM_)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Network.Transport.TCP (createTransport, defaultTCPParameters)

-- Forward the provided number to "recipient"
replyBack :: (ProcessId, ProcessId) -> Process ()
replyBack (master, workQueue) = do
  me <- getSelfPid
  requestWork me workQueue master

requestWork me workQueue master = do
  -- request work from master's queue
  send workQueue me
  -- execute work task, otherwise terminate
  receiveWait
    [ match $ \num -> do
        liftIO $ print ("Slave forwarding " ++ (show (num::Integer)))
        send master num >> requestWork me workQueue master
    , match $ \() -> return ()
    ]

logMessage :: String -> Process ()
logMessage msg = do 
  liftIO $ putStrLn msg
  say $ "handling " ++ msg


remotable ['replyBack]


master :: Backend -> [NodeId] -> Process Integer
master backend slaves = do
  me <- getSelfPid
  -- Print list of slaves
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  let numTasks = 10::Integer
  workQueue <- spawnLocal $ do
    -- generate list of tasks
    forM_ [1 .. numTasks] $ \num -> do
      -- wait for worker to request next task
      worker <- expect
      send worker num

    -- when workers finished processing work, kill them
    forever $ do
      worker <- expect
      send worker ()
      
  -- Start replyBack process on all slaves
  spawnLocal $ forM_ slaves $ startListener me workQueue
    
  -- Terminate the slaves when the master terminates (this is optional)
  --liftIO $ threadDelay 2000000
  --terminateAllSlaves backend
  getReplies numTasks

-- start replyBack process on slave which pulls tasks from master's workQueue
startListener me workQueue slave = do
  them <- spawn slave ($(mkClosure 'replyBack) (me, workQueue))
  reconnect them


-- Wait for reply from all slaves
getReplies :: Integer -> Process Integer
getReplies numSlaves = wait numSlaves
  where
    wait :: Integer -> Process Integer
    wait 0 = return 0
    wait repliesRemaining = do
      result <- expect
      liftIO $ print ("Master Received Reply:" ++ (show (result::Integer)))
      wait (repliesRemaining - 1)
