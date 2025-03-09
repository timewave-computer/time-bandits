{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module implements the Local Multi-Process mode for Time Bandits.
In this mode, each actor runs in its own process on the same machine,
communicating via Unix sockets.

The LocalMultiProcess module:
1. Manages process lifecycle (spawning, monitoring, termination)
2. Implements inter-process communication using Unix sockets
3. Coordinates execution across multiple processes
4. Handles process failures and recovery
-}
module Execution.LocalMultiProcess 
  ( -- * Core Types
    ProcessConfig(..)
  , ProcessId
  , ProcessState(..)
  , ProcessError(..)
  
  -- * Process Management
  , startProcess
  , stopProcess
  , monitorProcess
  , restartProcess
  
  -- * Communication
  , createSocket
  , sendToProcess
  , receiveFromProcess
  , broadcastToProcesses
  
  -- * Coordination
  , synchronizeProcesses
  , waitForProcesses
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (IOException, try)
import Control.Monad (when, void, forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.Socket (Socket, SockAddr(..), SocketType(Stream), Family(AF_INET, AF_INET6, AF_UNIX, AF_UNSPEC), socket, bind, connect, close, defaultProtocol)
import Network.Socket.ByteString (sendAll, recv)
import Polysemy (Member, Sem, embed)
import Polysemy.Error (Error, throw, catch)
import Polysemy.Embed (Embed)
import System.Exit (ExitCode)
import System.IO (Handle, hPutStr, hGetLine, hFlush)
import System.Process (ProcessHandle, createProcess, proc, waitForProcess, terminateProcess, cwd, env)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Show (showString, showParen, showsPrec)

-- Import from TimeBandits modules
import Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  )
import Core.ActorId
  ( ActorId
  )
import Actors.ActorCommunication
  ( ActorMessage(..)
  , ActorAddress(..)
  , ChannelError(..)
  , serializeMessage
  , deserializeMessage
  )

-- | Process identifier
type ProcessId = Text

-- | Configuration for a local process
data ProcessConfig = ProcessConfig
  { pcCommand :: Text
  , pcArgs :: [Text]
  , pcWorkingDir :: FilePath
  , pcEnvironment :: [(String, String)]
  , pcSocketPath :: FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | State of a process
data ProcessState
  = ProcessStarting
  | ProcessRunning ProcessHandle ThreadId Socket
  | ProcessFailed ExitCode
  | ProcessStopped

-- | Manual instances for ProcessState since ProcessHandle and ThreadId don't have Eq/Show
instance Eq ProcessState where
  ProcessStarting == ProcessStarting = True
  ProcessStopped == ProcessStopped = True
  (ProcessFailed e1) == (ProcessFailed e2) = e1 == e2
  _ == _ = False  -- ProcessRunning can't be compared

-- | Simplified Show instance for ProcessState
instance Show ProcessState where
  showsPrec _ ProcessStarting = showString "ProcessStarting"
  showsPrec _ (ProcessRunning _ _ _) = showString "ProcessRunning"
  showsPrec d (ProcessFailed exitCode) = showParen (d > 10) $
    showString "ProcessFailed " . showsPrec 11 exitCode
  showsPrec _ ProcessStopped = showString "ProcessStopped"

-- | Errors that can occur during process management
data ProcessError
  = ProcessStartError Text
  | ProcessCommunicationError Text
  | ProcessTerminationError Text
  | ProcessNotRunning ProcessId
  | SocketError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Start a new process with the given configuration
startProcess :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ProcessId -> 
  ProcessConfig -> 
  Sem r ProcessState
startProcess processId config = do
  -- Create the process
  result <- embed $ try $ createProcess (proc 
                                         (T.unpack $ pcCommand config) 
                                         (map T.unpack $ pcArgs config))
    { cwd = Just (pcWorkingDir config)
    , env = Just (pcEnvironment config)
    }
  
  case result of
    Left (e :: IOException) ->
      throw $ NetworkError $ T.pack $ "Failed to start process: " <> show e
    
    Right (_, _, _, processHandle) -> do
      -- Create a socket for communication
      socketResult <- createSocket (pcSocketPath config)
      
      case socketResult of
        Left err ->
          throw $ NetworkError $ "Failed to create socket: " <> err
        
        Right socket -> do
          -- Start a monitoring thread
          monitorThreadId <- embed $ forkIO $ void $ do
            -- Monitor the process and handle termination
            exitCode <- waitForProcess processHandle
            -- In a real implementation, this would update a shared state
            -- or notify a supervisor
            putStrLn $ "Process " <> T.unpack processId <> " terminated with exit code: " <> show exitCode
          
          -- Return the running process state
          pure $ ProcessRunning processHandle monitorThreadId socket

-- | Stop a running process
stopProcess :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ProcessId -> 
  ProcessState -> 
  Sem r ()
stopProcess processId state = case state of
  ProcessRunning processHandle monitorThread socket -> do
    -- Close the socket
    socketResult <- embed $ try $ close socket
    case socketResult of
      Left (e :: IOException) ->
        liftIO $ putStrLn $ "Warning: Failed to close socket: " <> show e
      Right _ ->
        pure ()
    
    -- Terminate the process
    processResult <- embed $ try $ terminateProcess processHandle
    case processResult of
      Left (e :: IOException) ->
        throw $ NetworkError $ T.pack $ "Failed to terminate process: " <> show e
      Right _ -> do
        -- Kill the monitoring thread
        embed $ killThread monitorThread
        pure ()
  
  _ ->
    throw $ NetworkError $ "Process not running: " <> processId

-- | Monitor a process and restart it if it fails
monitorProcess :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ProcessId -> 
  ProcessConfig -> 
  ProcessState -> 
  Sem r ProcessState
monitorProcess processId config state = case state of
  ProcessRunning processHandle _ _ -> do
    -- Check if the process is still running
    exitCodeMaybe <- embed $ try $ getProcessExitCode processHandle
    
    case exitCodeMaybe of
      Left (e :: IOException) ->
        throw $ NetworkError $ T.pack $ "Failed to check process status: " <> show e
      
      Right Nothing ->
        -- Process is still running
        pure state
      
      Right (Just exitCode) ->
        -- Process has terminated, restart it
        if shouldRestart exitCode
          then restartProcess processId config
          else pure $ ProcessFailed exitCode
  
  _ ->
    pure state

-- | Restart a failed process
restartProcess :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ProcessId -> 
  ProcessConfig -> 
  Sem r ProcessState
restartProcess processId config = do
  -- Start a new process with the same configuration
  startProcess processId config

-- | Create a socket for inter-process communication
createSocket :: 
  (Member (Embed IO) r) => 
  FilePath -> 
  Sem r (Either Text Socket)
createSocket path = do
  -- Create a Unix domain socket
  socketResult <- embed $ try $ do
    sock <- socket AF_UNIX Stream defaultProtocol
    bind sock (SockAddrUnix path)
    pure sock
  
  case socketResult of
    Left (e :: IOException) ->
      pure $ Left $ "Socket creation failed: " <> show e
    Right socket ->
      pure $ Right socket

-- | Send a message to a process
sendToProcess :: 
  (Member (Error AppError) r, Member (Embed IO) r, Serialize a) => 
  ProcessState -> 
  a -> 
  Sem r ()
sendToProcess state message = case state of
  ProcessRunning _ _ socket -> do
    -- Serialize the message
    let serialized = encode message
    
    -- Send the message
    sendResult <- embed $ try $ sendAll socket serialized
    
    case sendResult of
      Left (e :: IOException) ->
        throw $ NetworkError $ T.pack $ "Failed to send message: " <> show e
      Right _ ->
        pure ()
  
  _ ->
    throw $ NetworkError "Cannot send to non-running process"

-- | Receive a message from a process
receiveFromProcess :: 
  (Member (Error AppError) r, Member (Embed IO) r, Serialize a) => 
  ProcessState -> 
  Sem r a
receiveFromProcess state = case state of
  ProcessRunning _ _ socket -> do
    -- Receive the message
    receiveResult <- embed $ try $ recv socket 4096
    
    case receiveResult of
      Left (e :: IOException) ->
        throw $ NetworkError $ "Failed to receive message: " <> T.pack (show e)
      
      Right bytes ->
        -- Deserialize the message
        case decode bytes of
          Left err ->
            throw $ NetworkError $ "Failed to deserialize message: " <> T.pack err
          Right message ->
            pure message
  
  _ ->
    throw $ NetworkError "Cannot receive from non-running process"

-- | Broadcast a message to multiple processes
broadcastToProcesses :: 
  (Member (Error AppError) r, Member (Embed IO) r, Serialize a) => 
  Map ProcessId ProcessState -> 
  a -> 
  Sem r ()
broadcastToProcesses processes message = do
  -- Send the message to each running process
  for_ (Map.toList processes) $ \(processId, state) ->
    catch
      (sendToProcess state message)
      (\(err :: AppError) -> liftIO $ putStrLn $ "Failed to send to process " <> T.unpack processId <> ": " <> show err)

-- | Synchronize multiple processes
synchronizeProcesses :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  Map ProcessId ProcessState -> 
  Sem r ()
synchronizeProcesses processes = do
  -- In a real implementation, this would use a barrier or other synchronization primitive
  -- For now, just wait a bit to simulate synchronization
  embed $ threadDelay 100000 -- 100ms

-- | Wait for all processes to reach a certain state
waitForProcesses :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  Map ProcessId ProcessState -> 
  (ProcessState -> Bool) -> 
  Int -> 
  Sem r Bool
waitForProcesses processes predicate timeoutMs = do
  -- Check if all processes satisfy the predicate
  let allSatisfy = all predicate (Map.elems processes)
  
  if allSatisfy
    then pure True
    else if timeoutMs <= 0
      then pure False
      else do
        -- Wait a bit and try again
        embed $ threadDelay 10000 -- 10ms
        waitForProcesses processes predicate (timeoutMs - 10)

-- Helper functions

-- | Check if a process should be restarted based on its exit code
shouldRestart :: ExitCode -> Bool
shouldRestart _ = True -- Always restart for now

-- | Get the exit code of a process if it has terminated
getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode processHandle = do
  -- waitForProcess blocks until the process exits, so we need to use a non-blocking approach
  -- In a real implementation, this would use a non-blocking check
  -- For now, just return Nothing to indicate the process is still running
  pure Nothing 