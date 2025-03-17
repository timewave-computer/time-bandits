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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: TimeBandits.Programs.AccountProgram
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

SERIALIZATION STRATEGY:
-----------------------
This module follows the standardized serialization approach:
1. It imports TimeBandits.Core.Common.Serialize for standardized serialization instances (UTCTime, etc.)
2. It uses manual Serialize instances for all complex types to avoid ambiguity and conflicts
3. All Text values are explicitly encoded/decoded using TE.encodeUtf8/decodeUtf8
4. All compound types use field-by-field serialization rather than deriving Serialize

This ensures that:
- Serialization is consistent across the codebase
- There are no conflicts with other modules' Serialize instances
- Serialization format remains stable even if the underlying implementation changes

@since 0.1.0
-}
module TimeBandits.Programs.AccountProgram 
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

-- Import documentation of standard extensions
import TimeBandits.Core.Common.Extensions


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

-- Import standardized serialization first to ensure consistent instances
import TimeBandits.Core.Common.Serialize ()

-- Import from TimeBandits modules
import TimeBandits.Core.Common.Types (Hash, EntityHash)
import TimeBandits.Core.Types (AppError(..), EffectId)
import TimeBandits.Core.Effect (FactSnapshot)
import TimeBandits.Actors.ActorId (ActorId)
import TimeBandits.Core.ResourceId (ResourceId)
import TimeBandits.Core.Resource (Resource)
import TimeBandits.Core.ProgramId (ProgramId)
import TimeBandits.Core.Effect (Effect)

-- | Direction of a message
data MessageDirection = Inbound | Outbound
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A message in the account program inbox
data InboxMessage = InboxMessage
  { inboxMessageId :: Text
  , inboxTimestamp :: UTCTime
  , inboxSender :: ProgramId
  , inboxContents :: ByteString
  , inboxProcessed :: Bool
  }
  deriving stock (Eq, Show, Generic)

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize InboxMessage where
  put msg = do
    S.put (TE.encodeUtf8 $ inboxMessageId msg)
    S.put (inboxTimestamp msg)
    S.put (inboxSender msg)
    S.put (inboxContents msg)
    S.put (inboxProcessed msg)
    
  get = do
    messageId <- TE.decodeUtf8 <$> S.get
    timestamp <- S.get
    sender <- S.get
    contents <- S.get
    processed <- S.get
    return $ InboxMessage messageId timestamp sender contents processed

-- | A message in the account program outbox
data OutboxMessage = OutboxMessage
  { outboxMessageId :: Text
  , outboxTimestamp :: UTCTime
  , outboxRecipient :: ProgramId
  , outboxContents :: ByteString
  , outboxDelivered :: Bool
  }
  deriving stock (Eq, Show, Generic)

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize OutboxMessage where
  put msg = do
    S.put (TE.encodeUtf8 $ outboxMessageId msg)
    S.put (outboxTimestamp msg)
    S.put (outboxRecipient msg)
    S.put (outboxContents msg)
    S.put (outboxDelivered msg)
    
  get = do
    messageId <- TE.decodeUtf8 <$> S.get
    timestamp <- S.get
    recipient <- S.get
    contents <- S.get
    delivered <- S.get
    return $ OutboxMessage messageId timestamp recipient contents delivered

-- | Messages that can be sent to an Account Program
data AccountMessage
  = AccountDeposit
      { depositResourceId :: ResourceId
      , depositAmount :: Integer
      }
  | AccountWithdraw
      { withdrawResourceId :: ResourceId
      , withdrawAmount :: Integer
      , withdrawToProgramId :: ProgramId
      }
  | AccountTransfer
      { transferResourceId :: ResourceId
      , transferAmount :: Integer
      , transferToProgramId :: ProgramId
      }
  | AccountInvoke
      { invokeTargetProgramId :: ProgramId
      , invokeEntrypoint :: Text
      , invokeArguments :: [ByteString]
      }
  | AccountCallback
      { callbackFromProgramId :: ProgramId
      , callbackPayload :: ByteString
      }
  | ExternalDepositNotification
      { externalDepositResourceId :: ResourceId
      , externalDepositAmount :: Integer
      , externalDepositSource :: Text
      }
  deriving stock (Eq, Show, Generic)

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize AccountMessage where
  put (AccountDeposit resId amt) = do
    S.put (0 :: Int)
    S.put resId
    S.put amt
  put (AccountWithdraw resId amt progId) = do
    S.put (1 :: Int)
    S.put resId
    S.put amt
    S.put progId
  put (AccountTransfer resId amt progId) = do
    S.put (2 :: Int)
    S.put resId
    S.put amt
    S.put progId
  put (AccountInvoke progId entrypoint args) = do
    S.put (3 :: Int)
    S.put progId
    S.put (TE.encodeUtf8 entrypoint)
    S.put args
  put (AccountCallback progId payload) = do
    S.put (4 :: Int)
    S.put progId
    S.put payload
  put (ExternalDepositNotification resId amt source) = do
    S.put (5 :: Int)
    S.put resId
    S.put amt
    S.put (TE.encodeUtf8 source)
    
  get = do
    tag <- S.get :: S.Get Int
    case tag of
      0 -> AccountDeposit <$> S.get <*> S.get
      1 -> AccountWithdraw <$> S.get <*> S.get <*> S.get
      2 -> AccountTransfer <$> S.get <*> S.get <*> S.get
      3 -> do
        progId <- S.get
        entrypointBs <- S.get
        args <- S.get
        return $ AccountInvoke progId (TE.decodeUtf8 entrypointBs) args
      4 -> AccountCallback <$> S.get <*> S.get
      5 -> do
        resId <- S.get
        amt <- S.get
        sourceBs <- S.get
        return $ ExternalDepositNotification resId amt (TE.decodeUtf8 sourceBs)
      _ -> fail "Invalid AccountMessage tag"

-- | The Account Program data structure
data AccountProgram = AccountProgram
  { accountActorId :: ActorId
  , accountProgramId :: ProgramId
  , accountResources :: Map ResourceId Resource
  , accountInbox :: Map Text InboxMessage
  , accountOutbox :: Map Text OutboxMessage
  , accountState :: Map Text ByteString
  , accountLastEffectId :: Maybe EffectId
  }
  deriving stock (Eq, Show, Generic)

-- Manual Serialize instance to ensure consistent serialization and avoid ambiguity
instance Serialize AccountProgram where
  put prog = do
    S.put (accountActorId prog)
    S.put (accountProgramId prog)
    S.put (accountResources prog)
    S.put (accountInbox prog)
    S.put (accountOutbox prog)
    S.put (accountState prog)
    S.put (accountLastEffectId prog)
  
  get = AccountProgram 
    <$> S.get
    <*> S.get
    <*> S.get
    <*> S.get
    <*> S.get
    <*> S.get
    <*> S.get

-- | Create a new Account Program for an actor
createAccountProgram :: ActorId -> ProgramId -> AccountProgram
createAccountProgram actorId programId = AccountProgram
  { accountActorId = actorId
  , accountProgramId = programId
  , accountResources = Map.empty
  , accountInbox = Map.empty
  , accountOutbox = Map.empty
  , accountState = Map.empty
  , accountLastEffectId = Nothing
  }

-- | Deposit a resource into an Account Program
depositResource :: AccountProgram -> ResourceId -> Integer -> Either AppError AccountProgram
depositResource program _ _ = 
  -- Placeholder implementation
  Right program

-- | Withdraw a resource from an Account Program
withdrawResource :: AccountProgram -> ResourceId -> Integer -> ProgramId -> Either AppError AccountProgram
withdrawResource program _ _ _ = 
  -- Placeholder implementation
  Right program

-- | Transfer a resource from one Account Program to another
transferResource :: AccountProgram -> ResourceId -> Integer -> ProgramId -> Either AppError AccountProgram
transferResource program _ _ _ = 
  -- Placeholder implementation
  Right program

-- | Invoke another program from an Account Program
invokeProgram :: AccountProgram -> ProgramId -> Text -> [ByteString] -> Either AppError AccountProgram
invokeProgram program _ _ _ = 
  -- Placeholder implementation
  Right program

-- | Receive a callback from another program
receiveCallback :: AccountProgram -> ProgramId -> ByteString -> Either AppError AccountProgram
receiveCallback program _ _ = 
  -- Placeholder implementation
  Right program

-- | Watch for external deposits
watchExternalDeposit :: AccountProgram -> Either AppError AccountProgram
watchExternalDeposit program = 
  -- Placeholder implementation
  Right program

-- | Send a message from an Account Program
sendMessage :: AccountProgram -> ProgramId -> ByteString -> Either AppError AccountProgram
sendMessage program _ _ = 
  -- Placeholder implementation
  Right program

-- | Receive a message into an Account Program
receiveMessage :: AccountProgram -> ProgramId -> ByteString -> Either AppError AccountProgram
receiveMessage program _ _ = 
  -- Placeholder implementation
  Right program

-- | Get all messages in the inbox
getInbox :: AccountProgram -> Map Text InboxMessage
getInbox = accountInbox

-- | Get all messages in the outbox
getOutbox :: AccountProgram -> Map Text OutboxMessage
getOutbox = accountOutbox

-- | Clear the inbox
clearInbox :: AccountProgram -> AccountProgram
clearInbox program = program { accountInbox = Map.empty }

-- | Clear the outbox
clearOutbox :: AccountProgram -> AccountProgram
clearOutbox program = program { accountOutbox = Map.empty }

-- | Create an empty fact snapshot for use in this module
emptyFactSnapshot :: FactSnapshot
emptyFactSnapshot = undefined  -- This is a placeholder that will be replaced when needed 
