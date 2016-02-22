-- | Run server which accepts user-specified patterns described as
-- | strings (and maybe OSC messages in the future) and plays them in a loop.

-- compile: stack build
-- run: stack exec tidal-server
-- test: echo "[bd bd] cp" | nc --verbose --udp localhost 8000
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List

import qualified Sound.Tidal.Context as Tidal

type HandlerFunc = SockAddr -> String -> IO ()


setupTidalStream :: IO (Tidal.OscPattern -> IO ())
setupTidalStream = do
  putStrLn "Starting Tidal Dirt Software Synth"
  tidalStream <- Tidal.dirtStream
  return tidalStream


playSimplePattern :: (Tidal.OscPattern -> t) -> String -> t
playSimplePattern stream patternString = stream $ Tidal.sound (Tidal.p patternString)


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

    tidalStream <- setupTidalStream

    -- Loop forever processing incoming data.  Ctrl-C to abort.
    processMessages sock tidalStream
  where
    processMessages sock tidalStream = do
      -- Receive one UDP packet, maximum length 1024 bytes,
      -- save its content into msg and its source IP and port into addr
      (message, _, address) <- recvFrom sock 1024
      -- Handle it
      handlerfunc address message 
      playSimplePattern tidalStream message
      -- And process more messages
      processMessages sock tidalStream

-- A simple handler that prints incoming packets
printHandler :: HandlerFunc
printHandler addr msg = 
  putStrLn $ "From " ++ show addr ++ ": " ++ msg

-- |Main - kick off the tidal server
main :: IO ()
main = do server "8000" printHandler
