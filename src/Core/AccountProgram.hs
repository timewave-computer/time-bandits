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
import qualified Data.Text.Encoding as TE
import qualified Data.Serialize as S

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

-- | A message in the account program inbox
data InboxMessage = InboxMessage
  { inboxMessageId :: Text
  , inboxTimestamp :: UTCTime
  , inboxSender :: ProgramId
  , inboxContents :: ByteString
  , inboxProcessed :: Bool
  }
  deriving (Eq, Show, Generic)

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize InboxMessage where
  put msg = do
    S.put (TE.encodeUtf8 $ inboxMessageId msg)
    S.put (inboxTimestamp msg)
    S.put (inboxSender msg)
    S.put (inboxContents msg)
    S.put (inboxProcessed msg)
    
  get = do
    msgId <- TE.decodeUtf8 <$> S.get
    timestamp <- S.get
    sender <- S.get
    contents <- S.get
    processed <- S.get
    return $ InboxMessage msgId timestamp sender contents processed

-- | A message in the account program outbox
data OutboxMessage = OutboxMessage
  { outboxMessageId :: Text
  , outboxTimestamp :: UTCTime
  , outboxRecipient :: ProgramId
  , outboxContents :: ByteString
  , outboxDelivered :: Bool
  }
  deriving (Eq, Show, Generic)

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize OutboxMessage where
  put msg = do
    S.put (TE.encodeUtf8 $ outboxMessageId msg)
    S.put (outboxTimestamp msg)
    S.put (outboxRecipient msg)
    S.put (outboxContents msg)
    S.put (outboxDelivered msg)
    
  get = do
    msgId <- TE.decodeUtf8 <$> S.get
    timestamp <- S.get
    recipient <- S.get
    contents <- S.get
    delivered <- S.get
    return $ OutboxMessage msgId timestamp recipient contents delivered

-- | Message types that can be sent through an account program
data AccountMessage
  = Deposit 
      { depositResourceId :: ResourceId
      , depositAmount :: Integer
      , depositTarget :: ProgramId
      }
  | Withdraw
      { withdrawResourceId :: ResourceId
      , withdrawAmount :: Integer
      , withdrawSource :: ProgramId
      }
  | Transfer
      { transferResourceId :: ResourceId
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

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize AccountMessage where
  put (Deposit resId amt target) = do
    S.put (0 :: Word8)  -- Tag for Deposit
    S.put resId
    S.put amt
    S.put target
  put (Withdraw resId amt source) = do
    S.put (1 :: Word8)  -- Tag for Withdraw
    S.put resId
    S.put amt
    S.put source
  put (Transfer resId amt target) = do
    S.put (2 :: Word8)  -- Tag for Transfer
    S.put resId
    S.put amt
    S.put target
  put (Invoke target func args) = do
    S.put (3 :: Word8)  -- Tag for Invoke
    S.put target
    S.put (TE.encodeUtf8 func)
    S.put args
  put (ReceiveCallback source payload) = do
    S.put (4 :: Word8)  -- Tag for ReceiveCallback
    S.put source
    S.put payload
  put (Watch resource condition) = do
    S.put (5 :: Word8)  -- Tag for Watch
    S.put resource
    S.put condition
  put (CustomMessage msgType payload) = do
    S.put (6 :: Word8)  -- Tag for CustomMessage
    S.put (TE.encodeUtf8 msgType)
    S.put payload
    
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> Deposit <$> S.get <*> S.get <*> S.get
      1 -> Withdraw <$> S.get <*> S.get <*> S.get
      2 -> Transfer <$> S.get <*> S.get <*> S.get
      3 -> do
        target <- S.get
        funcBytes <- S.get
        args <- S.get
        return $ Invoke target (TE.decodeUtf8 funcBytes) args
      4 -> ReceiveCallback <$> S.get <*> S.get
      5 -> Watch <$> S.get <*> S.get
      6 -> do
        typeBytes <- S.get
        payload <- S.get
        return $ CustomMessage (TE.decodeUtf8 typeBytes) payload
      _ -> fail "Invalid AccountMessage tag"

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

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize AccountProgram where
  put prog = do
    S.put (accountId prog)
    S.put (accountProgramId prog)
    S.put (accountVersion prog)
    S.put (accountResources prog)
    S.put (accountEffects prog)
    S.put (accountRootEffect prog)
    S.put (accountInbox prog)
    S.put (accountOutbox prog)
    S.put (accountPendingMessages prog)
    S.put (accountAcceptDeposits prog)
    S.put (accountAllowWithdrawals prog)
    S.put (accountAllowCalls prog)
    S.put (accountKnownPrograms prog)
    S.put (accountLastUpdated prog)
    S.put (accountCurrentFacts prog)
    
  get = do
    actId <- S.get
    progId <- S.get
    ver <- S.get
    res <- S.get
    effs <- S.get
    root <- S.get
    inbox <- S.get
    outbox <- S.get
    pending <- S.get
    acceptDeposits <- S.get
    allowWithdrawals <- S.get
    allowCalls <- S.get
    knownPrograms <- S.get
    lastUpdated <- S.get
    currentFacts <- S.get
    return $ AccountProgram 
      actId progId ver 
      res effs root 
      inbox outbox pending 
      acceptDeposits allowWithdrawals allowCalls knownPrograms
      lastUpdated currentFacts

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
  let payload = TransferResourcePayload (showT resource) (toAddress target)
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