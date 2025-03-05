{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Core.AccountProgram
Description: Defines the AccountProgram type for actor gateway programs

This module defines the AccountProgram type, which serves as an actor's gateway
into the Time-Bandits system. Each actor (Time Traveler) has exactly one Account Program,
which holds their resources and handles all communication with other programs.
-}
module Core.AccountProgram 
  ( -- * Core AccountProgram Type
    AccountProgram(..)
  , ReceivedMessage(..)
  , SentMessage(..)
  , MessageStatus(..)
  , MessageType(..)
  , AccountMessage(..)
  ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Core.ActorId (ActorId)
import Core.ProgramId (ProgramId)
import Core.ResourceId (ResourceId)
import Core.TimelineId (TimelineId)

-- | Status of a message in the inbox/outbox
data MessageStatus = 
    Pending  -- ^ Message is waiting for processing
  | Delivered  -- ^ Message has been delivered but not yet processed
  | Processed  -- ^ Message has been fully processed
  | Failed T.Text  -- ^ Message processing failed with error
  deriving (Eq, Show, Generic)

-- | Type of messages that can be sent between programs
data MessageType =
    Deposit  -- ^ Move assets into a program
  | Withdraw  -- ^ Retrieve assets from a program
  | Invoke  -- ^ Call a program function
  | Transfer  -- ^ Transfer assets to a program
  | SendCallback  -- ^ Return result to an actor
  | ReceiveCallback  -- ^ Actor retrieves program responses
  | Watch  -- ^ Watch for external deposit
  | CustomMessage  -- ^ Custom structured message
  deriving (Eq, Show, Generic)

-- | AccountMessage represents messages that can be sent to or from account programs
data AccountMessage
  = DepositMessage { resource :: ResourceId, amount :: Integer, to :: ProgramId }
  | WithdrawMessage { resource :: ResourceId, amount :: Integer, from :: ProgramId }
  | InvokeMessage { targetProgram :: ProgramId, entrypoint :: T.Text, arguments :: [T.Text] }
  | ReceiveCallbackMessage { sourceProgram :: ProgramId, payload :: T.Text }
  | CustomAccountMessage { messageType :: T.Text, payload :: T.Text }
  deriving (Eq, Show, Generic)

-- | A message received from another program
data ReceivedMessage = ReceivedMessage
  { rcvMessageId :: T.Text
  , rcvMessageType :: MessageType
  , fromProgram :: ProgramId
  , rcvPayload :: T.Text
  , rcvStatus :: MessageStatus
  , rcvTimestamp :: Int
  }
  deriving (Eq, Show, Generic)

-- | A message sent to another program
data SentMessage = SentMessage
  { sentMessageId :: T.Text
  , sentMessageType :: MessageType
  , toProgram :: ProgramId
  , sentPayload :: T.Text
  , sentStatus :: MessageStatus
  , sentTimestamp :: Int
  }
  deriving (Eq, Show, Generic)

-- | AccountProgram serves as an actor's gateway into the system
data AccountProgram = AccountProgram
  { owner :: ActorId
  , balances :: Map.Map ResourceId Integer
  , inbox :: [ReceivedMessage]
  , outbox :: [SentMessage]
  , timelineAccounts :: Map.Map TimelineId T.Text
  }
  deriving (Eq, Show, Generic) 