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
{-# LANGUAGE TypeFamilies #-}

{- |
Module: Actors.Actor
Description: Actor system framework for the Time-Bandits architecture.

This module provides the Actor abstraction, which defines the interface for
actors in the Time Bandits system. Actors are responsible for:

- Submitting transition messages to the controller
- Receiving and processing responses
- Managing their own state and resources
- Communicating with other actors

The Actor abstraction is implemented differently depending on the simulation mode:
- In-memory: Actors are Haskell functions in the same process
- Local multi-process: Actors run in separate processes with Unix socket messaging
- Geo-distributed: Actors run on remote machines with TCP/RPC messaging

In the Time-Bandits architecture, Actors play several critical roles:

1. Autonomous agents: Actors encapsulate autonomous behavior, with each type
   (TimeTraveler, TimeKeeper, TimeBandit) having specific capabilities and responsibilities.

2. Security boundaries: Each actor maintains its own cryptographic identity and state,
   enabling secure, authenticated communications.

3. Distributed execution: Actors can be distributed across multiple machines,
   enabling robust, fault-tolerant execution of programs.

4. Resource management: Actors claim, transfer, and verify ownership of resources
   across timelines, enabling complex cross-timeline operations.

The Actor module connects with the Programs module for program execution,
the Execution module for effect handling, and the Core module for basic
primitives like cryptographic functions and event handling.
-}
module Actors.Actor 
  ( -- * Core Types
    Actor(..)
  , ActorSpec(..)
  , ActorRole(..)
  , ActorCapability(..)
  , ActorState(..)
  , ActorError(..)
  , ActorHandle
  
  -- * Actor Operations
  , createActor
  , runActor
  , sendMessage
  , receiveMessage
  , getActorId
  , getActorRole
  , getActorCapabilities
  
  -- * Deployment Operations
  , deployActor
  , deployInMemoryActor
  , deployLocalActor
  , deployRemoteActor
  
  -- * Actor Communication
  , ActorMessage(..)
  , MessageQueue
  , createMessageQueue
  , enqueueMessage
  , dequeueMessage
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.STM (TQueue, readTQueue, writeTQueue)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever, void, when, replicateM)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Socket (Socket, SockAddr(..), PortNumber)
import Polysemy (Member, Sem, Embed, embed, runM)
import Polysemy.Error (Error, throw, runError)
import System.Process (ProcessHandle)
import qualified Data.ByteString.Char8 as BS
import qualified Data.IORef as IORef
import System.Directory (createDirectoryIfMissing)
import Polysemy.Trace qualified as PT
import Adapters.NetworkQUIC (startQuicServer, p2pConfigToQuicConfig)
import Adapters.Network (P2PConfig(..))
import Data.Time.Clock (UTCTime, getCurrentTime)

-- Import from TimeBandits modules
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  , PubKey(..)
  , ActorType(..)
  )
import Core.Resource 
  ( Resource(..)
  , ResourceId
  )
import Programs.Program 
  ( ProgramId
  )
import Actors.TransitionMessage
  ( TransitionMessage
  )
import Core.Common
  ( SimulationMode(..)
  )
import Actors.ActorTypes
  ( ActorType(..)
  , ActorRole(..)
  , ActorCapability(..)
  , ActorSpec(..)
  )

-- Import specialized actor role modules (forward references)
import qualified Actors.TimeTraveler as TimeTraveler
import qualified Actors.TimeKeeper as TimeKeeper
import qualified Actors.TimeBandit as TimeBandit
import qualified Actors.ActorCoordination as ActorCoordination

-- | Type alias for deployment mode
type DeploymentMode = SimulationMode

-- | Type alias for actor address
type Address = Text

-- | Message queue for actors
type MessageQueue a = TQueue a

-- | Actor message data type
data ActorMessage = 
    TransitionMsg TransitionMessage
  | ControlMsg Text
  | DataMsg ByteString
  deriving (Show, Generic)
  deriving anyclass (Serialize)

-- | Actor error type
data ActorError = 
    ActorNotFound Text
  | InvalidActorState Text
  | CommunicationError Text
  | AuthorizationError Text
  | ResourceError Text
  | DeploymentError Text
  deriving (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Actor state
data ActorState = ActorState
  { actorStateId :: Address
  , actorStateRole :: ActorRole
  , actorStateCapabilities :: [ActorCapability]
  , actorStatePrograms :: Map ProgramId ByteString
  , actorStateResources :: [ResourceId]
  }
  deriving (Show, Generic)
  deriving anyclass (Serialize)

-- | In-memory actor handle
data InMemoryHandle = InMemoryHandle
  { inMemoryId :: Address
  , inMemoryState :: MVar ActorState
  , inMemoryQueue :: MessageQueue ActorMessage
  }

-- | Local process actor handle
data LocalProcessHandle = LocalProcessHandle
  { localProcessId :: Address
  , localProcessHandle :: ProcessHandle
  , localProcessAddress :: SockAddr
  }

-- | Remote actor handle
data RemoteHandle = RemoteHandle
  { remoteId :: Address
  , remoteHost :: String
  , remotePort :: PortNumber
  , remoteSocket :: Maybe Socket
  , remoteActor :: Text
  , remoteState :: ActorState
  , remoteAddress :: SockAddr
  , remoteNetworkConfig :: P2PConfig
  , remoteLastSeen :: UTCTime
  , remoteIsConnected :: Bool
  }

-- | Actor handle that combines all handle types
data ActorHandle = 
    InMemoryActorHandle InMemoryHandle
  | LocalProcessActorHandle LocalProcessHandle
  | RemoteActorHandle RemoteHandle

-- | Create actor state from spec
createActorState :: ActorSpec -> Sem r ActorState
createActorState spec = pure $ ActorState
  { actorStateId = actorSpecId spec
  , actorStateRole = actorSpecRole spec
  , actorStateCapabilities = actorSpecCapabilities spec
  , actorStatePrograms = Map.empty  -- Initialize with empty map
  , actorStateResources = []
  }

-- | Create a message queue
createMessageQueue :: IO (MessageQueue ActorMessage)
createMessageQueue = STM.newTQueueIO

-- | Enqueue a message
enqueueMessage :: MessageQueue ActorMessage -> ActorMessage -> IO ()
enqueueMessage queue msg = STM.atomically $ writeTQueue queue msg

-- | Dequeue a message
dequeueMessage :: MessageQueue ActorMessage -> IO (Maybe ActorMessage)
dequeueMessage queue = STM.atomically $ Just <$> readTQueue queue

-- | Helper functions for in-memory actors
runInMemoryActor :: IORef.IORef ActorState -> IO ()
runInMemoryActor stateRef = forever $ do
  state <- IORef.readIORef stateRef
  -- In a real implementation, this would process messages and update state
  threadDelay 1000000  -- Sleep for 1 second

sendToInMemoryActor :: MessageQueue ActorMessage -> ActorMessage -> IO ()
sendToInMemoryActor = enqueueMessage

receiveFromInMemoryActor :: MessageQueue ActorMessage -> IO (Maybe ActorMessage)
receiveFromInMemoryActor queue = do
  -- Try to read a message, but don't block if none available
  msg <- STM.atomically $ do
    isEmpty <- STM.isEmptyTQueue queue
    if isEmpty
      then return Nothing
      else Just <$> readTQueue queue
  return msg

-- | Actor class defines the interface for all actors
class Actor a where
  -- | Run the actor
  runActor :: a -> IO ()
  
  -- | Get the actor's ID
  getActorId :: a -> Address
  
  -- | Get the actor's role
  getActorRole :: a -> ActorRole
  
  -- | Get the actor's capabilities
  getActorCapabilities :: a -> [ActorCapability]
  
  -- | Send a message to the actor
  sendMessage :: a -> ActorMessage -> IO ()
  
  -- | Receive a message from the actor
  receiveMessage :: a -> IO (Maybe ActorMessage)

-- | Create an actor from a specification
createActor :: (Member (Error AppError) r, Member (Error ActorError) r) => ActorSpec -> Sem r ActorState
createActor spec = do
  -- Create the initial actor state
  let state = ActorState
        { actorStateId = actorSpecId spec
        , actorStateRole = actorSpecRole spec
        , actorStateCapabilities = actorSpecCapabilities spec
        , actorStatePrograms = Map.empty  -- Initialize with empty map
        , actorStateResources = []
        }
  
  return state

-- | Deploy an actor in the specified mode
deployActor :: (Member (Error ActorError) r, Member (Embed IO) r)
           => ActorSpec
           -> DeploymentMode
           -> Sem r ActorHandle
deployActor spec mode = case mode of
  InMemory -> deployInMemoryActor spec
  MultiProcess -> deployLocalActor spec
  Distributed -> deployRemoteActor spec

-- | Deploy an actor in in-memory mode
deployInMemoryActor :: (Member (Error ActorError) r, Member (Embed IO) r)
                   => ActorSpec
                   -> Sem r ActorHandle
deployInMemoryActor spec = do
  -- Create initial actor state
  initialState <- createActorState spec
  
  -- Create a message queue for the actor
  messageQueue <- embed createMessageQueue
  
  -- Create an MVar to hold the actor state
  stateMVar <- embed $ MVar.newMVar initialState
  
  -- Create the handle
  let handle = InMemoryHandle
        { inMemoryId = actorSpecId spec
        , inMemoryState = stateMVar
        , inMemoryQueue = messageQueue
        }
  
  -- Start the actor in a separate thread
  _ <- embed $ forkIO $ runInMemoryActor' handle
  
  -- Return the handle
  pure (InMemoryActorHandle handle)
  where
    -- Run the in-memory actor
    runInMemoryActor' :: InMemoryHandle -> IO ()
    runInMemoryActor' handle = forever $ do
      -- Process messages
      mMsg <- receiveFromInMemoryActor (inMemoryQueue handle)
      case mMsg of
        Just msg -> do
          -- Process the message
          -- In a real implementation, this would update the actor state
          putStrLn $ "Actor " ++ T.unpack (inMemoryId handle) ++ " received message"
        Nothing -> 
          -- No message, sleep for a bit
          threadDelay 100000  -- 100ms

-- | Deploy an actor in remote mode (for geo-distributed deployment)
deployRemoteActor :: (Member (Error ActorError) r, Member (Embed IO) r)
                 => ActorSpec
                 -> Sem r ActorHandle
deployRemoteActor spec = do
  -- Create initial actor state
  state <- createActorState spec
  
  -- Use a fixed ID for now instead of random generation
  let remoteId = actorSpecId spec <> "-remote"
  
  -- Get the current time for tracking
  now <- embed getCurrentTime
  
  -- Create network configuration for this actor
  let networkConfig = P2PConfig
        { pcBindAddress = SockAddrInet 0 0  -- Will be assigned by the system
        , pcSeedNodes = []
        , pcReplicationFactor = 3  -- Default value
        , pcNodeCapacity = 50  -- Default value
        , pcRefreshInterval = 30  -- 30 seconds
        , pcHealthCheckInterval = 300  -- 5 minutes
        , pcConnectionTimeout = 5000  -- 5 seconds
        , pcMaxMessageSize = 1048576  -- 1MB
        }
  
  -- Convert to QUIC configuration
  let quicConfig = p2pConfigToQuicConfig networkConfig
  
  -- Start the QUIC server for this actor
  _ <- embed $ do
    -- Create the certificate directory if it doesn't exist
    createDirectoryIfMissing True "certs"
    
    -- In a real implementation, we would start a separate process or connect to a remote machine
    -- For now, we'll simulate this with a thread
    forkIO $ do
      result <- runM $ runError @AppError $ PT.traceToStdout $ do
        -- Start the QUIC server (commented out for now)
        -- startQuicServer quicConfig actor pubKey
        
        -- Log that the server started
        PT.trace $ "Started QUIC server for actor " <> T.unpack remoteId
        
        -- Keep the server running
        embed $ forever $ threadDelay 1000000  -- 1 second
      
      -- Handle the result
      case result of
        Left err -> putStrLn $ "Error starting QUIC server: " ++ show err
        Right _ -> return ()
  
  -- Create a handle for the remote actor
  pure $ RemoteActorHandle $ RemoteHandle
    { remoteId = remoteId
    , remoteHost = ""
    , remotePort = 8443
    , remoteSocket = Nothing
    , remoteActor = actorSpecId spec
    , remoteState = state
    , remoteAddress = SockAddrInet 8443 0  -- Default QUIC port if not specified
    , remoteNetworkConfig = networkConfig
    , remoteLastSeen = now
    , remoteIsConnected = True
    }

-- | Deploy an actor in local process mode (for local multi-process deployment) 
deployLocalActor :: (Member (Error ActorError) r, Member (Embed IO) r)
                => ActorSpec
                -> Sem r ActorHandle
deployLocalActor spec = do
  -- This would normally spawn a separate process
  -- For now, we'll simulate it with a simplified handle
  pure $ LocalProcessActorHandle $ LocalProcessHandle
    { localProcessId = actorSpecId spec
    , localProcessHandle = error "Local process not implemented"
    , localProcessAddress = SockAddrInet 0 0
    } 