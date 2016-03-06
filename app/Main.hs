import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified DistributedMapReduce

myRemoteTable :: RemoteTable
myRemoteTable = DistributedMapReduce.__remoteTable initRemoteTable

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startMaster backend $ \slaves -> do
        result <- DistributedMapReduce.master backend slaves
        liftIO $ print result
    ["slave", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startSlave backend
