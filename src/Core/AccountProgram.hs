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
Module: Core.AccountProgram
Description: Account Program for actor-system interactions

This module implements the Account Program abstraction, which serves as a gateway
for actors to interact with the system. Each actor has exactly one Account Program,
which is responsible for:

1. Holding all resources owned by the actor
2. Sending deposits to external programs
3. Receiving withdrawals from programs
4. Communicating with other programs
5. Maintaining a structured inbox/outbox for all actor communication

Account Programs are a crucial part of the system's security model, ensuring that:
- All asset transfers flow through a consistent model
- All actor-program interactions are tracked
- Resources are never directly owned by actors
-}
module Core.AccountProgram 
  ( -- * Core Types
    AccountProgram(..)
  , AccountMessage(..)
  , MessageDirection(..)
  , InboxMessage(..)
  , OutboxMessage(..)
  
  -- * Account Operations
  , createAccountProgram
  , depositResource
  , withdrawResource
  , transferResource
  , invokeProgram
  , receiveCallback
  , watchExternalDeposit
  
  -- * Message Management
  , sendMessage
  , receiveMessage
  , getInbox
  , getOutbox
  , clearInbox
  , clearOutbox
  ) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

-- Import from Core modules
import Core.Common (EntityHash, Hash)
import Core.Types (AppError(..), FactSnapshot, EffectId, emptyFactSnapshot)
import Core.ActorId (ActorId)
import Core.ResourceId (ResourceId)
import Core.Resource (Resource)
import Core.ProgramId (ProgramId)
import Types.Effect (Effect)
import Types.EffectPayload (EffectPayload(..))

-- | Direction of a message
data MessageDirection = Inbound | Outbound
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A message in the account program inbox
data InboxMessage = InboxMessage
  { inboxMessageId :: Text
  , inboxTimestamp :: UTCTime
  , inboxSender :: ProgramId
  , inboxContents :: ByteString
  , inboxProcessed :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A message in the account program outbox
data OutboxMessage = OutboxMessage
  { outboxMessageId :: Text
  , outboxTimestamp :: UTCTime
  , outboxRecipient :: ProgramId
  , outboxContents :: ByteString
  , outboxDelivered :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Message types that can be sent through an account program
data AccountMessage
  = Deposit 
      { depositResource :: ResourceId
      , depositAmount :: Integer
      , depositTarget :: ProgramId
      }
  | Withdraw
      { withdrawResource :: ResourceId
      , withdrawAmount :: Integer
      , withdrawSource :: ProgramId
      }
  | Transfer
      { transferResource :: ResourceId
      , transferAmount :: Integer
      , transferTarget :: ActorId
      }
  | Invoke
      { invokeTarget :: ProgramId
      , invokeFunction :: Text
      , invokeArguments :: [ByteString]
      }
  | ReceiveCallback
      { callbackSource :: ProgramId
      , callbackPayload :: ByteString
      }
  | Watch
      { watchResource :: ResourceId
      , watchCondition :: ByteString
      }
  | CustomMessage
      { customMessageType :: Text
      , customMessagePayload :: ByteString
      }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | The Account Program is a specialized program that serves as a gateway
-- for actors to interact with the system
data AccountProgram = AccountProgram
  { -- Basic account information
    accountId :: ActorId
  , accountProgramId :: ProgramId
  , accountVersion :: Integer
  
  -- Resources and effects
  , accountResources :: Map ResourceId Resource
  , accountEffects :: Map EffectId Effect
  , accountRootEffect :: Maybe EffectId
  
  -- Communication
  , accountInbox :: [InboxMessage]
  , accountOutbox :: [OutboxMessage]
  , accountPendingMessages :: [AccountMessage]
  
  -- Capabilities
  , accountAcceptDeposits :: Bool
  , accountAllowWithdrawals :: Bool
  , accountAllowCalls :: Bool
  , accountKnownPrograms :: Map ProgramId UTCTime  -- Last interaction time
  
  -- Internal state
  , accountLastUpdated :: UTCTime
  , accountCurrentFacts :: FactSnapshot
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new account program for an actor
createAccountProgram :: ActorId -> ProgramId -> UTCTime -> AccountProgram
createAccountProgram actorId programId timestamp = AccountProgram
  { accountId = actorId
  , accountProgramId = programId
  , accountVersion = 1
  , accountResources = Map.empty
  , accountEffects = Map.empty
  , accountRootEffect = Nothing
  , accountInbox = []
  , accountOutbox = []
  , accountPendingMessages = []
  , accountAcceptDeposits = True
  , accountAllowWithdrawals = True
  , accountAllowCalls = True
  , accountKnownPrograms = Map.empty
  , accountLastUpdated = timestamp
  , accountCurrentFacts = emptyFactSnapshot
  }

-- | Deposit a resource into another program
depositResource :: AccountProgram -> ResourceId -> Integer -> ProgramId -> (AccountProgram, Effect)
depositResource account resource amount target =
  -- Create the deposit effect
  let payload = TransferResourcePayload resource (toAddress target)
      effectParents = maybe [] (\root -> [root]) (accountRootEffect account)
      effect = error "depositResource not fully implemented" -- Would create effect with proper ID
      
      -- Add to outbox
      message = OutboxMessage
        { outboxMessageId = "msg-" <> showT (length (accountOutbox account))
        , outboxTimestamp = accountLastUpdated account
        , outboxRecipient = target
        , outboxContents = error "serialization not implemented"
        , outboxDelivered = False
        }
      newOutbox = message : accountOutbox account
      
      -- Update the account
      newAccount = account 
        { accountOutbox = newOutbox
        , accountLastUpdated = accountLastUpdated account  -- Would be updated with current time
        }
  in (newAccount, effect)

-- | Withdraw a resource from another program
withdrawResource :: AccountProgram -> ResourceId -> Integer -> ProgramId -> (AccountProgram, Effect)
withdrawResource account resource amount source =
  error "withdrawResource not implemented"

-- | Transfer a resource to another actor (through their account program)
transferResource :: AccountProgram -> ResourceId -> Integer -> ActorId -> (AccountProgram, Effect)
transferResource account resource amount targetActor =
  error "transferResource not implemented"

-- | Invoke a function on another program
invokeProgram :: AccountProgram -> ProgramId -> Text -> [ByteString] -> (AccountProgram, Effect)
invokeProgram account target function args =
  error "invokeProgram not implemented"

-- | Receive a callback from another program
receiveCallback :: AccountProgram -> ProgramId -> ByteString -> AccountProgram
receiveCallback account source payload =
  error "receiveCallback not implemented"

-- | Watch for an external deposit (for cross-chain ingress)
watchExternalDeposit :: AccountProgram -> ResourceId -> ByteString -> (AccountProgram, Effect)
watchExternalDeposit account resource condition =
  error "watchExternalDeposit not implemented"

-- | Send a message through the account program
sendMessage :: AccountProgram -> AccountMessage -> AccountProgram
sendMessage account message =
  error "sendMessage not implemented"

-- | Receive a message from the account program inbox
receiveMessage :: AccountProgram -> Maybe (AccountMessage, AccountProgram)
receiveMessage account =
  error "receiveMessage not implemented"

-- | Get all messages in the inbox
getInbox :: AccountProgram -> [InboxMessage]
getInbox = accountInbox

-- | Get all messages in the outbox
getOutbox :: AccountProgram -> [OutboxMessage]
getOutbox = accountOutbox

-- | Clear processed messages from the inbox
clearInbox :: AccountProgram -> AccountProgram
clearInbox account = account { accountInbox = filter (not . inboxProcessed) (accountInbox account) }

-- | Clear delivered messages from the outbox
clearOutbox :: AccountProgram -> AccountProgram
clearOutbox account = account { accountOutbox = filter (not . outboxDelivered) (accountOutbox account) }

-- | Helper to convert a program ID to a text address
toAddress :: ProgramId -> Text
toAddress = error "toAddress not implemented"

-- | Helper to show any showable value as Text
showT :: Show a => a -> Text
showT = error "showT not implemented" 