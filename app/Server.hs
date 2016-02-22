-- | Run server which accepts user-specified patterns described as
-- | strings and prints them in a loop.

-- compile: stack build
-- run: stack exec tidal-server
-- test: echo "Test" | nc --verbose --udp localhost 8000
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List

type HandlerFunc = SockAddr -> String -> IO ()

server :: String          -- Port number or name.
       -> HandlerFunc   -- Function to handle incoming messages.
       -> IO ()
server port handlerfunc = withSocketsDo $ do
  -- Look up the port.  Either raises an exception or returns
  -- a nonempty list.  
    addrinfos <- getAddrInfo 
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing (Just port)
    let serveraddr = head addrinfos

    -- Create a socket
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

    -- Bind it to the address we're listening to
    bindSocket sock (addrAddress serveraddr)

    -- Loop forever processing incoming data.  Ctrl-C to abort.
    processMessages sock
  where
    processMessages sock = do
      -- Receive one UDP packet, maximum length 1024 bytes,
      -- save its content into msg and its source IP and port into addr
      (msg, _, addr) <- recvFrom sock 1024
      -- Handle it
      handlerfunc addr msg
      -- And process more messages
      processMessages sock

-- A simple handler that prints incoming packets
printHandler :: HandlerFunc
printHandler addr msg = 
  putStrLn $ "From " ++ show addr ++ ": " ++ msg

-- |Main - kick off the tidal server
main :: IO ()
main = do server "8000" printHandler
