#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Test script for QUIC-based networking
-- This script tests the QUIC-based networking implementation
-- by starting a server and connecting to it as a client.

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Network.Socket (SockAddr(..), tupleToHostAddress)
import Polysemy (Sem, runM, interpret, makeSem, Member, Embed, embed)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Trace (Trace, traceToStdout)
import qualified Polysemy.Trace as Trace
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import TimeBandits.Core (Actor(..), ActorRole(..))
import TimeBandits.Crypto (PubKey(..), PrivKey(..))
import TimeBandits.NetworkQUIC

-- | Main entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server"] -> runServer
    ["client"] -> runClient
    _ -> do
      hPutStrLn stderr "Usage: test-quic [server|client]"
      exitFailure

-- | Run a QUIC server
runServer :: IO ()
runServer = do
  putStrLn "Starting QUIC server..."
  
  -- Create the certificate directory
  createDirectoryIfMissing True "certs"
  
  -- Run the server
  result <- runM $ runError @NetworkError $ traceToStdout $ do
    -- Create a server actor
    now <- embed getCurrentTime
    let pubKey = PubKey "server-public-key"
        actorId = "server-actor-id"
        serverActor = Actor actorId TimeTraveler
    
    -- Create a QUIC configuration
    let config = defaultQuicConfig {
          qcBindAddress = SockAddrInet 8443 (tupleToHostAddress (127, 0, 0, 1)),
          qcBindPort = 8443,
          qcNetworkMode = ServerMode
        }
    
    -- Start the QUIC server
    server <- startQuicServer config serverActor pubKey
    
    -- Keep the server running
    embed $ do
      putStrLn "Server started. Press Ctrl+C to stop."
      forever $ threadDelay 1000000  -- 1 second
  
  case result of
    Left err -> do
      putStrLn $ "Server error: " ++ show err
      exitFailure
    Right _ -> do
      putStrLn "Server stopped."
      exitSuccess

-- | Run a QUIC client
runClient :: IO ()
runClient = do
  putStrLn "Starting QUIC client..."
  
  -- Create the certificate directory
  createDirectoryIfMissing True "certs"
  
  -- Run the client
  result <- runM $ runError @NetworkError $ traceToStdout $ do
    -- Create a client actor
    now <- embed getCurrentTime
    let pubKey = PubKey "client-public-key"
        actorId = "client-actor-id"
        clientActor = Actor actorId TimeKeeper
    
    -- Create a QUIC configuration
    let config = defaultQuicConfig {
          qcBindAddress = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)),  -- Any port
          qcBindPort = 0,  -- Any port
          qcNetworkMode = ClientMode,
          qcBootstrapNodes = [SockAddrInet 8443 (tupleToHostAddress (127, 0, 0, 1))]
        }
    
    -- Start the QUIC client
    server <- startQuicServer config clientActor pubKey
    
    -- Connect to the server
    Trace.trace "Connecting to server..."
    conn <- connectToQuicPeer server (SockAddrInet 8443 (tupleToHostAddress (127, 0, 0, 1)))
    
    -- Send a message
    Trace.trace "Sending message to server..."
    now <- embed getCurrentTime
    sendQuicMessage server conn (QuicPing now)
    
    -- Wait for a response
    Trace.trace "Waiting for response..."
    embed $ threadDelay 1000000  -- 1 second
    
    -- Disconnect
    Trace.trace "Disconnecting from server..."
    disconnectFromQuicPeer server conn
    
    -- Stop the client
    Trace.trace "Stopping client..."
    stopQuicServer server
    
    Trace.trace "Client test completed successfully."
  
  case result of
    Left err -> do
      putStrLn $ "Client error: " ++ show err
      exitFailure
    Right _ -> do
      putStrLn "Client test completed."
      exitSuccess 