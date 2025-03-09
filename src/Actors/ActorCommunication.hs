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
This module provides the actor communication protocol for distributed execution.
It defines message formats, channels, and addressing for actors across network boundaries.

The communication protocol supports:
- Actor-to-actor messaging across processes
- Program deployment and execution commands
- State queries and responses
- System control messages
-}
module Actors.ActorCommunication
  ( -- * Core Types
    ActorMessage(..)
  , ActorChannel(..)
  , ActorAddress(..)
  , ChannelError(..)
  , NodeId
  
  -- * Channel Operations
  , createChannel
  , closeChannel
  , sendMessage
  , receiveMessage
  , createLocalChannel
  , createNetworkChannel
  
  -- * Serialization
  , serializeMessage
  , deserializeMessage
  
  -- * Actor Discovery
  , discoverActors
  , registerActor
  , lookupActor
  
  -- * Message Routing
  , routeMessage
  , broadcastMessage
  ) where

import Control.Concurrent (MVar, newMVar)
import qualified Control.Concurrent as Concurrent (takeMVar, putMVar)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, newTVarIO)
import Control.Exception (IOException, try)
import Control.Monad (when, void, forever, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics (Generic)
import Network.Socket (Socket, SockAddr(..), SocketType(Stream), Family(AF_INET, AF_INET6, AF_UNIX, AF_UNSPEC), socket, bind, connect, close, defaultProtocol)
import Network.Socket.ByteString (sendAll, recv)
import System.IO (Handle, hPutStr, hGetLine, hFlush)
import Polysemy (Member, Sem, embed)
import Polysemy.Error (Error, throw, catch)
import Polysemy.Embed (Embed)
import Relude (newIORef)

-- Import from TimeBandits modules
import Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  )
import Actors.Actor
  ( ActorRole
  , Actor(..)
  )
import Actors.TransitionMessage
  ( TransitionMessage(..)
  )
import Core.Resource 
  ( Resource
  , ResourceHash
  )
import Programs.Program 
  ( ProgramId
  , ProgramDefinition
  , ProgramState
  )
import Core.TimeMap
  ( TimeMap
  )

-- | Node identifier for network addressing
type NodeId = Text

-- | Actor address for message routing
data ActorAddress 
  = LocalAddress Text               -- ^ In-memory or local process address
  | NetworkAddress Text SockAddr    -- ^ Network socket address
  | ProcessAddress Text FilePath    -- ^ Unix socket or process ID
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Channel error types
data ChannelError
  = ConnectionFailed Text
  | MessageSendFailed Text
  | MessageReceiveFailed Text
  | DeserializationFailed Text
  | ChannelClosed Text
  | AddressUnreachable ActorAddress
  | AuthenticationFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Actor message protocol
data ActorMessage
  = DeployProgram ProgramDefinition TimeMap [Resource]
  | ExecuteTransition TransitionMessage
  | QueryState ProgramId
  | QueryResource ResourceHash
  | SystemCommand Text
  | QueryResponse ByteString  -- Generic response container
  | ErrorResponse Text        -- Error message
  | Acknowledgment Hash       -- Acknowledgment with message hash
  | Heartbeat LamportTime     -- Heartbeat with logical timestamp
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Actor communication channel
data ActorChannel = ActorChannel
  { channelSend :: ActorMessage -> IO (Either ChannelError ())
  , channelReceive :: IO (Either ChannelError ActorMessage)
  , channelClose :: IO ()
  , channelAddress :: ActorAddress
  , channelRemoteAddress :: ActorAddress
  }

-- | Create a communication channel between actors
createChannel :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ActorAddress -> 
  ActorAddress -> 
  Sem r ActorChannel
createChannel localAddr remoteAddr = do
  case (localAddr, remoteAddr) of
    -- Local in-memory channel
    (LocalAddress _, LocalAddress _) ->
      createLocalChannel localAddr remoteAddr
    
    -- Network socket channel
    (NetworkAddress _ localSock, NetworkAddress _ remoteSock) ->
      createNetworkChannel localAddr remoteAddr
    
    -- Process channel (Unix socket or stdio)
    (ProcessAddress _ _, ProcessAddress _ _) ->
      createProcessChannel localAddr remoteAddr
    
    -- Unmatched address types
    _ -> throw $ AppError "Incompatible address types for channel creation"

-- | Close a communication channel
closeChannel :: 
  (Member (Embed IO) r) => 
  ActorChannel -> 
  Sem r ()
closeChannel channel = embed $ channelClose channel

-- | Send a message through a channel
sendMessage :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ActorChannel -> 
  ActorMessage -> 
  Sem r ()
sendMessage channel msg = do
  result <- embed $ channelSend channel msg
  case result of
    Left err -> throw $ AppError $ "Failed to send message: " <> T.pack (show err)
    Right () -> pure ()

-- | Receive a message from a channel
receiveMessage :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ActorChannel -> 
  Sem r ActorMessage
receiveMessage channel = do
  result <- embed $ channelReceive channel
  case result of
    Left err -> throw $ AppError $ "Failed to receive message: " <> T.pack (show err)
    Right msg -> pure msg

-- | Create a local in-memory channel
createLocalChannel :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ActorAddress -> 
  ActorAddress -> 
  Sem r ActorChannel
createLocalChannel localAddr remoteAddr = do
  -- Create message queues for bidirectional communication
  sendQueue <- embed $ newMVar []
  recvQueue <- embed $ newMVar []
  
  -- Create the channel with appropriate send/receive functions
  let channel = ActorChannel
        { channelSend = \msg -> do
            encodedMsg <- pure $ serializeMessage msg
            case encodedMsg of
              Left err -> pure $ Left $ DeserializationFailed $ T.pack $ show err
              Right bytes -> do
                -- In a real implementation, this would use proper IPC
                -- For now, just append to the queue
                msgs <- Concurrent.takeMVar sendQueue
                Concurrent.putMVar sendQueue (bytes : msgs)
                pure $ Right ()
        
        , channelReceive = do
            msgs <- Concurrent.takeMVar recvQueue
            case msgs of
              [] -> do
                Concurrent.putMVar recvQueue []
                pure $ Left $ MessageReceiveFailed "No messages available"
              (bytes:rest) -> do
                Concurrent.putMVar recvQueue rest
                case deserializeMessage bytes of
                  Left err -> pure $ Left $ DeserializationFailed $ T.pack $ show err
                  Right msg -> pure $ Right msg
        
        , channelClose = do
            -- Clear the queues
            void $ Concurrent.takeMVar sendQueue
            Concurrent.putMVar sendQueue []
            void $ Concurrent.takeMVar recvQueue
            Concurrent.putMVar recvQueue []
        
        , channelAddress = localAddr
        , channelRemoteAddress = remoteAddr
        }
  
  pure channel

-- | Create a network socket channel
createNetworkChannel :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ActorAddress -> 
  ActorAddress -> 
  Sem r ActorChannel
createNetworkChannel localAddr@(NetworkAddress _ localSock) remoteAddr@(NetworkAddress _ remoteSock) = do
  -- Create a socket for communication
  sock <- embed $ socket (getAddressFamily remoteSock) Stream defaultProtocol
  
  -- Bind to the local address
  embed $ bind sock localSock
  
  -- Connect to the remote address
  result <- embed $ try $ connect sock remoteSock
  case result of
    Left (err :: IOException) -> 
      throw $ AppError $ "Failed to connect to remote address: " <> T.pack (show err)
    Right () -> do
      -- Create the channel with socket send/receive functions
      let channel = ActorChannel
            { channelSend = \msg -> do
                encodedMsg <- pure $ serializeMessage msg
                case encodedMsg of
                  Left err -> pure $ Left $ DeserializationFailed $ T.pack $ show err
                  Right bytes -> do
                    -- Send the message size first, then the message
                    let size = BS.length bytes
                        sizeBytes = encode (fromIntegral size :: Int)
                    
                    -- Send in a try block to handle network errors
                    sendResult <- try $ do
                      sendAll sock sizeBytes
                      sendAll sock bytes
                    
                    case sendResult of
                      Left (err :: IOException) -> 
                        pure $ Left $ MessageSendFailed $ T.pack $ show err
                      Right () -> 
                        pure $ Right ()
            
            , channelReceive = do
                -- Receive the message size first
                sizeBytes <- try $ recv sock 4
                case sizeBytes of
                  Left (err :: IOException) -> 
                    pure $ Left $ MessageReceiveFailed $ T.pack $ show err
                  Right bytes -> 
                    if BS.null bytes 
                      then pure $ Left $ ChannelClosed "Connection closed by peer"
                      else do
                        let Right size = decode bytes :: Either String Int
                        
                        -- Receive the message
                        msgBytes <- try $ recvExactly sock size
                        case msgBytes of
                          Left (err :: IOException) -> 
                            pure $ Left $ MessageReceiveFailed $ T.pack $ show err
                          Right bytes' ->
                            case deserializeMessage bytes' of
                              Left err -> pure $ Left $ DeserializationFailed $ T.pack $ show err
                              Right msg -> pure $ Right msg
            
            , channelClose = close sock
            
            , channelAddress = localAddr
            , channelRemoteAddress = remoteAddr
            }
      
      pure channel
  where
    -- Helper to get the address family from a SockAddr
    getAddressFamily (SockAddrInet _ _) = AF_INET
    getAddressFamily (SockAddrInet6 _ _ _ _) = AF_INET6
    getAddressFamily (SockAddrUnix _) = AF_UNIX
    getAddressFamily _ = AF_UNSPEC
    
    -- Helper to receive exactly n bytes
    recvExactly sock n = do
      let loop remaining acc
            | remaining <= 0 = pure $ BS.concat $ reverse acc
            | otherwise = do
                chunk <- recv sock remaining
                if BS.null chunk
                  then pure $ BS.concat $ reverse acc
                  else loop (remaining - BS.length chunk) (chunk : acc)
      loop n []
    
    -- Wrapper for try to handle IO exceptions
    try :: IO a -> IO (Either IOException a)
    try = Control.Exception.try

-- | Create a process channel using Unix sockets or stdio
createProcessChannel :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ActorAddress -> 
  ActorAddress -> 
  Sem r ActorChannel
createProcessChannel localAddr@(ProcessAddress _ localPath) remoteAddr@(ProcessAddress _ remotePath) = do
  -- For Unix socket communication
  sock <- embed $ socket AF_UNIX Stream defaultProtocol
  
  -- Bind to the local path
  embed $ bind sock (SockAddrUnix localPath)
  
  -- Connect to the remote path
  result <- embed $ try $ connect sock (SockAddrUnix remotePath)
  case result of
    Left (err :: IOException) -> 
      throw $ AppError $ "Failed to connect to remote process: " <> T.pack (show err)
    Right () -> do
      -- Create the channel with socket send/receive functions
      let channel = ActorChannel
            { channelSend = \msg -> do
                encodedMsg <- pure $ serializeMessage msg
                case encodedMsg of
                  Left err -> pure $ Left $ DeserializationFailed $ T.pack $ show err
                  Right bytes -> do
                    -- Send the message size first, then the message
                    let size = BS.length bytes
                        sizeBytes = encode (fromIntegral size :: Int)
                    
                    -- Send in a try block to handle errors
                    sendResult <- try $ do
                      sendAll sock sizeBytes
                      sendAll sock bytes
                    
                    case sendResult of
                      Left (err :: IOException) -> 
                        pure $ Left $ MessageSendFailed $ T.pack $ show err
                      Right () -> 
                        pure $ Right ()
            
            , channelReceive = do
                -- Receive the message size first
                sizeBytes <- try $ recv sock 4
                case sizeBytes of
                  Left (err :: IOException) -> 
                    pure $ Left $ MessageReceiveFailed $ T.pack $ show err
                  Right bytes -> 
                    if BS.null bytes 
                      then pure $ Left $ ChannelClosed "Connection closed by peer"
                      else do
                        let Right size = decode bytes :: Either String Int
                        
                        -- Receive the message
                        msgBytes <- try $ recvExactly sock size
                        case msgBytes of
                          Left (err :: IOException) -> 
                            pure $ Left $ MessageReceiveFailed $ T.pack $ show err
                          Right bytes' ->
                            case deserializeMessage bytes' of
                              Left err -> pure $ Left $ DeserializationFailed $ T.pack $ show err
                              Right msg -> pure $ Right msg
            
            , channelClose = close sock
            
            , channelAddress = localAddr
            , channelRemoteAddress = remoteAddr
            }
      
      pure channel
  where
    -- Helper to receive exactly n bytes
    recvExactly sock n = do
      let loop remaining acc
            | remaining <= 0 = pure $ BS.concat $ reverse acc
            | otherwise = do
                chunk <- recv sock remaining
                if BS.null chunk
                  then pure $ BS.concat $ reverse acc
                  else loop (remaining - BS.length chunk) (chunk : acc)
      loop n []
    
    -- Wrapper for try to handle IO exceptions
    try :: IO a -> IO (Either IOException a)
    try = Control.Exception.try

-- | Serialize an actor message to bytes
serializeMessage :: ActorMessage -> Either String ByteString
serializeMessage = Right . encode

-- | Deserialize bytes to an actor message
deserializeMessage :: ByteString -> Either String ActorMessage
deserializeMessage = decode

-- | Discover actors on the network
discoverActors :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ActorRole -> 
  Sem r [ActorAddress]
discoverActors role = do
  -- In a real implementation, this would use mDNS or a discovery service
  -- For now, return an empty list
  pure []

-- | Register an actor with the discovery service
registerActor :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  Actor -> 
  ActorAddress -> 
  ActorRole -> 
  Sem r ()
registerActor actor addr role = do
  -- In a real implementation, this would register with a discovery service
  -- For now, do nothing
  pure ()

-- | Look up an actor by ID
lookupActor :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  EntityHash Actor -> 
  Sem r (Maybe ActorAddress)
lookupActor actorId = do
  -- In a real implementation, this would query a discovery service
  -- For now, return Nothing
  pure Nothing

-- | Route a message to the appropriate actor
routeMessage :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ActorMessage -> 
  ActorAddress -> 
  Sem r ()
routeMessage msg addr = do
  -- Look up or create a channel to the destination
  remoteActor <- lookupActor (EntityHash $ Hash "dummy")  -- This would be the actual actor ID
  case remoteActor of
    Nothing -> throw $ AppError $ "Actor not found: " <> T.pack (show addr)
    Just remoteAddr -> do
      -- Create a channel to the remote actor
      channel <- createChannel addr remoteAddr
      
      -- Send the message
      sendMessage channel msg
      
      -- Close the channel
      closeChannel channel

-- | Broadcast a message to multiple actors
broadcastMessage :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ActorMessage -> 
  [ActorAddress] -> 
  ActorAddress -> 
  Sem r ()
broadcastMessage msg addrs localAddr = do
  -- Send the message to each actor
  for_ addrs $ \addr -> do
    -- Try to route the message, but continue on failure
    catch (routeMessage msg addr) $ \(_ :: AppError) -> pure () 