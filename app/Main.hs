{-# LANGUAGE TemplateHaskell #-}

import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Network.Transport.TCP (createTransport, defaultTCPParameters)


sampleTask :: (Int, String) -> Process ()
sampleTask (secondsDelay, outputString) = liftIO (threadDelay (secondsDelay * 1000000)) >> say outputString

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = do 
  liftIO $ putStrLn msg
  say $ "handling " ++ msg


remotable ['sampleTask, 'logMessage]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable


main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startSlave backend


master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  -- Print list of slaves
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  -- Start heartbeat process on all slaves
  heartbeatPids <- mapM startHeartbeatListener slaves
  _ <- mapM sendHeartbeat heartbeatPids
  -- Terminate the slaves when the master terminates (this is optional)
  liftIO $ threadDelay 2000000
  terminateAllSlaves backend

-- Heartbeat process just echoes message
startHeartbeatListener :: NodeId -> Process ProcessId
startHeartbeatListener nodeId = spawnLocal $ logMessage "using spawnLocal"
--startHeartbeatListener nodeId = spawn nodeId $ $(mkClosure 'logMessage) ("using spawn")

-- Send heartbeat
sendHeartbeat :: ProcessId -> Process()
sendHeartbeat receiverProcessId = send receiverProcessId "hello"

-- Initially Slave just echoes heartbeat message
slave :: Backend -> Process()
slave backend = forever $ do
  receiveWait [match logMessage, match replyBack]
