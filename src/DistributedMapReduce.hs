{-# LANGUAGE TemplateHaskell #-}

module DistributedMapReduce where

import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, forM_)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Network.Transport.TCP (createTransport, defaultTCPParameters)

-- Forward the provided number to "recipient"
replyBack :: (ProcessId, Integer) -> Process ()
replyBack (recipient, num) = do
  liftIO $ print ("Slave forwarding " ++ (show num))
  send recipient num

logMessage :: String -> Process ()
logMessage msg = do 
  liftIO $ putStrLn msg
  say $ "handling " ++ msg


remotable ['replyBack, 'logMessage]


master :: Backend -> [NodeId] -> Process Integer
master backend slaves = do
  me <- getSelfPid
  -- Print list of slaves
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  -- Get index of each slave
  let numSlaves = length slaves
  let slavesAndIndices = zip [1 .. numSlaves] slaves
  liftIO $ print slavesAndIndices

  -- Start replyBack process on all slaves
  spawnLocal $ forM_ slavesAndIndices $ execRemote me
  --echoPids <- mapM execRemote slavesAndIndices
  
  -- Terminate the slaves when the master terminates (this is optional)
  --liftIO $ threadDelay 2000000
  --terminateAllSlaves backend
  getReplies numSlaves


-- Send the slave a number which they will return to me.
execRemote me (number, slave) = do
  liftIO $ print number
  them <- spawn slave ($(mkClosure 'replyBack) (me, number))
  reconnect them

-- Wait for reply from all slaves
getReplies :: Int -> Process Integer
getReplies numSlaves = wait numSlaves
  where
    wait :: Int -> Process Integer
    wait 0 = return 0
    wait repliesRemaining = do
      result <- expect
      liftIO $ print ("Master Received Reply:" ++ (show (result::Integer)))
      wait (repliesRemaining - 1)
