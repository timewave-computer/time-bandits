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
  ( -- * Core Types
    Message(..)
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
  
  -- * Message Routing
  , sendMessage
  , receiveMessage
  , receiveCallback
  
  -- * Message Processing
  , processMessage
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)

import Core.Timeline (TimelineHash)
import Core.ActorId (ActorId)
import Core.ProgramId (ProgramId)
import Core.Effect (Effect(..))
import Core.AccountProgram (AccountMessage(..), AccountProgram(..))
import Execution.EffectInterpreter (EffectInterpreter, interpretEffect, ProposedEffect(..))
import Core.TimeMap (TimeMap, getCurrentTimeMap)

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
sendMessage :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProgramId -> 
  AccountMessage -> 
  m (Either T.Text MessageId)
sendMessage interpreter fromProgram message = liftIO $ do
  -- Get the current time map
  currentTimeMap <- getCurrentTimeMap
  
  -- Create a proposed effect
  let proposedEffect = ProposedEffect
        { effect = AccountMessageEffect fromProgram message
        , observedTimeMap = currentTimeMap
        , readSet = []  -- Would be populated based on message type
        , writeSet = []  -- Would be populated based on message type
        }
  
  -- Interpret the effect
  result <- interpretEffect interpreter proposedEffect
  
  -- Generate a message ID based on the effect
  let messageId = generateMessageId proposedEffect
  
  -- Return the message ID if successful, otherwise an error
  case result of
    EffectApplied _ _ -> pure $ Right messageId
    EffectFailed err -> pure $ Left err
    _ -> pure $ Left "Message could not be sent"

-- | Receive a callback from a program
receiveCallback :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProgramId -> 
  T.Text -> 
  m (Either T.Text MessageId)
receiveCallback interpreter toProgramId payload = liftIO $ do
  -- Get the current time map
  currentTimeMap <- getCurrentTimeMap
  
  -- Create a callback message
  let callbackMessage = ReceiveCallbackMessage
        { sourceProgram = toProgramId
        , payload = payload
        }
  
  -- Create a proposed effect
  let proposedEffect = ProposedEffect
        { effect = AccountMessageEffect toProgramId callbackMessage
        , observedTimeMap = currentTimeMap
        , readSet = []  -- Would be populated based on message type
        , writeSet = []  -- Would be populated based on message type
        }
  
  -- Interpret the effect
  result <- interpretEffect interpreter proposedEffect
  
  -- Generate a message ID based on the effect
  let messageId = generateMessageId proposedEffect
  
  -- Return the message ID if successful, otherwise an error
  case result of
    EffectApplied _ _ -> pure $ Right messageId
    EffectFailed err -> pure $ Left err
    _ -> pure $ Left "Callback could not be received"

-- | Generate a message ID from a proposed effect
generateMessageId :: ProposedEffect -> MessageId
generateMessageId _ = "msg-" <> T.pack (show (hash "placeholder"))  -- Placeholder

-- | Message ID type
type MessageId = T.Text

-- | Hash function (placeholder)
hash :: String -> Int
hash s = length s  -- Placeholder, would use a real hash function
