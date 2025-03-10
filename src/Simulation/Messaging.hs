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
This module defines the messaging protocol used for communication between actors
in the Time Bandits system. It provides:

- Message types for different kinds of inter-actor communication
- Serialization/deserialization of messages
- Routing and addressing for messages
- Message validation and error handling

All actor-to-actor communication should go through this module to ensure
consistent messaging patterns across the system.
-}
module Simulation.Messaging
  ( -- * Types
    ActorID
  , ActorRole(..)
  , ActorSpec(..)
  , MessageId
  
  -- * Messaging
  , sendMessage
  , receiveCallback
  
  -- * Message Processing
  , generateMessageId
  
  -- * Core Types
  , Message(..)
  , MessageType(..)
  , MessageEnvelope(..)
  , ActorID
  , ActorRole(..)
  , ActorSpec(..)
  
  -- * Message Creation
  , createMessage
  , wrapMessage
  
  -- * Message Handling
  , handleMessage
  , routeMessage
  , validateMessage
  
  -- * Actor Specifications
  , actorSpecID
  , actorSpecRole
  , actorSpecName
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)

import Core.Timeline (TimelineHash)
import Core.ActorId (ActorId(..))
import Core.ProgramId (ProgramId, programIdToText)
import Core.Effect (Effect(..))
import Core.AccountProgram (AccountMessage(..), AccountProgram(..))
import qualified Core.AccountProgram as AccountProgram
import Execution.EffectInterpreter (EffectInterpreter, interpretEffect, EffectResult(..))
import Core.TimeMap (TimeMap, getCurrentTimeMap)
import Core.Resource (ResourceId)

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time (getCurrentTime)

-- | Uniquely identifies an actor in the system
type ActorID = Text

-- | The role an actor plays in the system
data ActorRole
  = TimeTravelerRole  -- ^ Can manipulate the timeline
  | TimeKeeperRole    -- ^ Maintains timeline consistency
  | TimeBanditRole    -- ^ Attempts to exploit the system
  deriving (Show, Eq, Generic, Serialize)

-- | Specification for an actor to be deployed
data ActorSpec = ActorSpec
  { _actorSpecID :: ActorID
  , _actorSpecRole :: ActorRole
  , _actorSpecName :: Text
  , _actorSpecConfig :: Map Text Text  -- ^ Role-specific configuration
  , _actorId :: ActorId  -- ^ Actor ID for account program
  , _initialBalances :: Map ResourceId Integer  -- ^ Initial balances for account program
  }
  deriving (Show, Eq, Generic, Serialize)

-- | The type of message being sent
data MessageType
  = EffectProposal      -- ^ Proposal to apply an effect
  | EffectConfirmation  -- ^ Confirmation of effect application
  | TimelineUpdate      -- ^ Update to a timeline's state
  | ProgramStateQuery   -- ^ Request for program state
  | ProgramStateResponse -- ^ Response with program state
  | ActorError          -- ^ Error message from an actor
  | SystemControl       -- ^ System-level control message
  deriving (Show, Eq, Generic, Serialize)

-- | The content of a message between actors
data Message = Message
  { messageType :: MessageType
  , messageContent :: ByteString  -- ^ Serialized message content
  , messageTimestamp :: Integer
  , messageSignature :: ByteString -- ^ Message authenticity
  }
  deriving (Show, Eq, Generic, Serialize)

-- | An envelope containing a message and routing information
data MessageEnvelope = MessageEnvelope
  { envelopeFrom :: ActorID      -- ^ Sender
  , envelopeTo :: ActorID        -- ^ Recipient
  , envelopeMessage :: Message
  , envelopeTimelineHash :: TimelineHash  -- ^ For timeline-specific messages
  }
  deriving (Show, Eq, Generic, Serialize)

-- | Create a new message
createMessage :: MessageType -> ByteString -> Integer -> ByteString -> Message
createMessage = Message

-- | Wrap a message in an envelope for delivery
wrapMessage :: ActorID -> ActorID -> Message -> TimelineHash -> MessageEnvelope
wrapMessage from to msg timelineHash = MessageEnvelope
  { envelopeFrom = from
  , envelopeTo = to
  , envelopeMessage = msg
  , envelopeTimelineHash = timelineHash
  }

-- | Handle an incoming message (placeholder implementation)
handleMessage :: MessageEnvelope -> IO ()
handleMessage _ = pure ()  -- Placeholder

-- | Route a message to its destination (placeholder implementation)
routeMessage :: MessageEnvelope -> IO ()
routeMessage _ = pure ()  -- Placeholder

-- | Validate a message (placeholder implementation)
validateMessage :: MessageEnvelope -> Bool
validateMessage _ = True  -- Placeholder

-- | Get the actor ID from a spec
actorSpecID :: ActorSpec -> ActorID
actorSpecID = _actorSpecID

-- | Get the actor role from a spec
actorSpecRole :: ActorSpec -> ActorRole
actorSpecRole = _actorSpecRole

-- | Get the actor name from a spec
actorSpecName :: ActorSpec -> Text
actorSpecName = _actorSpecName

-- | Send a message from one program to another via account programs
sendMessage :: EffectInterpreter -> ProgramId -> ProgramId -> AccountMessage -> IO (Either Text MessageId)
sendMessage interpreter fromProgram toProgramId message = do
  -- Get the current time map
  currentTimeMap <- getCurrentTimeMap
  
  -- Create a message ID
  let messageId = generateMessageId (fromProgram, toProgramId, message)
  
  -- Log the message
  liftIO $ putStrLn $ "Sending message from " <> T.unpack (programIdToText fromProgram) <> " to " <> T.unpack (programIdToText toProgramId)
  
  -- In a real implementation, we would create a proper effect and interpret it
  -- For now, we'll just return success
  pure $ Right messageId

-- | Receive a callback from a program
receiveCallback :: EffectInterpreter -> ProgramId -> ProgramId -> BS.ByteString -> IO (Either Text MessageId)
receiveCallback interpreter fromProgram toProgramId payload = do
  -- Get the current time map
  currentTimeMap <- getCurrentTimeMap
  
  -- Create a callback message
  let callbackMessage = ReceiveCallback
        { AccountProgram.callbackSource = toProgramId
        , AccountProgram.callbackPayload = payload
        }
  
  -- Create a message ID
  let messageId = generateMessageId (fromProgram, toProgramId, callbackMessage)
  
  -- Log the message
  liftIO $ putStrLn $ "Receiving callback from " <> T.unpack (programIdToText toProgramId) <> " to " <> T.unpack (programIdToText fromProgram)
  
  -- In a real implementation, we would create a proper effect and interpret it
  -- For now, we'll just return success
  pure $ Right messageId

-- | Generate a message ID from a proposed effect
generateMessageId :: (ProgramId, ProgramId, AccountMessage) -> MessageId
generateMessageId (fromProgram, toProgramId, message) = "msg-" <> T.pack (show (hashTuple (fromProgram, toProgramId, message)))

-- | Message ID type
type MessageId = T.Text

-- | Hash function (placeholder)
hashTuple :: (ProgramId, ProgramId, AccountMessage) -> Int
hashTuple (fromProgram, toProgramId, _) = 
  length (T.unpack (programIdToText fromProgram)) + 
  length (T.unpack (programIdToText toProgramId))  -- Placeholder, would use a real hash function
