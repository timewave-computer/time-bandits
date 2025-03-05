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
Module: Programs.AccountProgram
Description: Account program implementation

This module defines the AccountProgram type and related functions for managing
actor accounts in the Time-Bandits system. Account programs serve as mandatory
gateways for all actor interactions, ensuring proper resource tracking and
transaction validation.

Key responsibilities:
- Resource balance tracking
- Transaction validation
- Message routing
- Effect application
-}
module Programs.AccountProgram
  ( -- * Core types
    AccountProgram(..)
  , AccountMessage(..)
  , AccountOperation(..)
  , ResourceLedger(..)
  , LockTable(..)
  
  -- * Creation and initialization
  , createAccountProgram
  , initializeAccountProgram
  
  -- * Message handling
  , applyAccountMessage
  , routeMessage
  
  -- * Resource operations
  , getBalance
  , updateBalance
  , lockResource
  , unlockResource
  , isResourceLocked
  
  -- * Effect handling
  , applyEffect
  , validateEffect
  ) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- Import from Core modules
import Core.Common (ActorId, EntityHash)
import Core.Resource (ResourceId)
import Core.Types (LamportTime)

-- | Resource ledger tracks balances of resources
newtype ResourceLedger = ResourceLedger
  { balances :: Map ResourceId Integer }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Lock table tracks which resources are currently locked
newtype LockTable = LockTable
  { locks :: Map ResourceId LamportTime }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Account message types for interacting with account programs
data AccountMessage
  = Transfer ResourceId ActorId Integer  -- ^ Transfer resources to another actor
  | Deposit ResourceId Integer           -- ^ Deposit resources into account
  | Withdraw ResourceId Integer          -- ^ Withdraw resources from account
  | Query ResourceId                     -- ^ Query balance of a resource
  | LockResources [ResourceId]           -- ^ Lock resources for an operation
  | UnlockResources [ResourceId]         -- ^ Unlock resources after operation
  | ApplyEffect ByteString               -- ^ Apply an effect to the account
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Account operations that can be performed
data AccountOperation
  = BalanceUpdate ResourceId Integer     -- ^ Update balance of a resource
  | ResourceLock ResourceId LamportTime  -- ^ Lock a resource
  | ResourceUnlock ResourceId            -- ^ Unlock a resource
  | EffectApplication ByteString         -- ^ Apply an effect
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Account program for managing actor resources
data AccountProgram = AccountProgram
  { accountId :: ActorId                 -- ^ Actor ID associated with this account
  , resourceLedger :: ResourceLedger     -- ^ Resource balances
  , lockTable :: LockTable               -- ^ Resource locks
  , lastOperation :: LamportTime         -- ^ Timestamp of last operation
  , operationLog :: [AccountOperation]   -- ^ Log of operations
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new account program for an actor
createAccountProgram :: ActorId -> Map ResourceId Integer -> IO AccountProgram
createAccountProgram actorId initialBalances = do
  -- Create the account program with initial balances
  pure AccountProgram
    { accountId = actorId
    , resourceLedger = ResourceLedger initialBalances
    , lockTable = LockTable Map.empty
    , lastOperation = 0
    , operationLog = []
    }

-- | Initialize an account program with initial balances
initializeAccountProgram :: AccountProgram -> Map ResourceId Integer -> AccountProgram
initializeAccountProgram program initialBalances =
  program { resourceLedger = ResourceLedger initialBalances }

-- | Apply an account message to an account program
applyAccountMessage :: AccountProgram -> AccountMessage -> LamportTime -> (AccountProgram, Maybe ByteString)
applyAccountMessage program message timestamp =
  case message of
    Transfer resourceId recipient amount ->
      let (program', result) = transferResource program resourceId recipient amount timestamp
      in (program', result)
    
    Deposit resourceId amount ->
      let program' = updateBalance program resourceId amount timestamp
      in (program', Just $ "Deposited " <> encodeInteger amount)
    
    Withdraw resourceId amount ->
      let (program', result) = withdrawResource program resourceId amount timestamp
      in (program', result)
    
    Query resourceId ->
      let balance = getBalance program resourceId
      in (program, Just $ encodeInteger balance)
    
    LockResources resources ->
      let program' = foldl (\p r -> lockResource p r timestamp) program resources
      in (program', Just "Resources locked")
    
    UnlockResources resources ->
      let program' = foldl (\p r -> unlockResource p r) program resources
      in (program', Just "Resources unlocked")
    
    ApplyEffect effect ->
      let (program', result) = applyEffect program effect timestamp
      in (program', result)

-- | Route a message through an account program
routeMessage :: AccountProgram -> ByteString -> LamportTime -> (AccountProgram, Maybe ByteString)
routeMessage program message timestamp =
  -- Placeholder for message routing
  -- In a real implementation, this would:
  -- 1. Decode the message
  -- 2. Convert it to an AccountMessage
  -- 3. Apply the message
  -- 4. Return the result
  (program, Just "Message routed")

-- | Get the balance of a resource
getBalance :: AccountProgram -> ResourceId -> Integer
getBalance program resourceId =
  fromMaybe 0 $ Map.lookup resourceId (balances $ resourceLedger program)

-- | Update the balance of a resource
updateBalance :: AccountProgram -> ResourceId -> Integer -> LamportTime -> AccountProgram
updateBalance program resourceId delta timestamp =
  let ledger = resourceLedger program
      currentBalance = fromMaybe 0 $ Map.lookup resourceId (balances ledger)
      newBalance = currentBalance + delta
      newBalances = Map.insert resourceId newBalance (balances ledger)
      newLedger = ledger { balances = newBalances }
      operation = BalanceUpdate resourceId delta
      newLog = operation : operationLog program
  in program
      { resourceLedger = newLedger
      , lastOperation = timestamp
      , operationLog = newLog
      }

-- | Transfer resources to another actor
transferResource :: AccountProgram -> ResourceId -> ActorId -> Integer -> LamportTime -> (AccountProgram, Maybe ByteString)
transferResource program resourceId recipient amount timestamp =
  if amount <= 0
    then (program, Just "Invalid transfer amount")
    else
      let currentBalance = getBalance program resourceId
      in if currentBalance < amount
        then (program, Just "Insufficient balance")
        else
          let program' = updateBalance program resourceId (-amount) timestamp
          in (program', Just $ "Transferred " <> encodeInteger amount <> " to " <> encodeActorId recipient)

-- | Withdraw resources from an account
withdrawResource :: AccountProgram -> ResourceId -> Integer -> LamportTime -> (AccountProgram, Maybe ByteString)
withdrawResource program resourceId amount timestamp =
  if amount <= 0
    then (program, Just "Invalid withdrawal amount")
    else
      let currentBalance = getBalance program resourceId
      in if currentBalance < amount
        then (program, Just "Insufficient balance")
        else
          let program' = updateBalance program resourceId (-amount) timestamp
          in (program', Just $ "Withdrew " <> encodeInteger amount)

-- | Lock a resource
lockResource :: AccountProgram -> ResourceId -> LamportTime -> AccountProgram
lockResource program resourceId timestamp =
  let table = lockTable program
      newLocks = Map.insert resourceId timestamp (locks table)
      newTable = table { locks = newLocks }
      operation = ResourceLock resourceId timestamp
      newLog = operation : operationLog program
  in program
      { lockTable = newTable
      , lastOperation = timestamp
      , operationLog = newLog
      }

-- | Unlock a resource
unlockResource :: AccountProgram -> ResourceId -> AccountProgram
unlockResource program resourceId =
  let table = lockTable program
      newLocks = Map.delete resourceId (locks table)
      newTable = table { locks = newLocks }
      operation = ResourceUnlock resourceId
      newLog = operation : operationLog program
  in program
      { lockTable = newTable
      , operationLog = newLog
      }

-- | Check if a resource is locked
isResourceLocked :: AccountProgram -> ResourceId -> Bool
isResourceLocked program resourceId =
  isJust $ Map.lookup resourceId (locks $ lockTable program)

-- | Apply an effect to an account program
applyEffect :: AccountProgram -> ByteString -> LamportTime -> (AccountProgram, Maybe ByteString)
applyEffect program effect timestamp =
  -- Placeholder for effect application
  -- In a real implementation, this would:
  -- 1. Decode the effect
  -- 2. Validate the effect
  -- 3. Apply the effect to the account state
  -- 4. Return the result
  let operation = EffectApplication effect
      newLog = operation : operationLog program
      program' = program
        { lastOperation = timestamp
        , operationLog = newLog
        }
  in (program', Just "Effect applied")

-- | Validate an effect before applying it
validateEffect :: AccountProgram -> ByteString -> Bool
validateEffect _ _ =
  -- Placeholder for effect validation
  -- In a real implementation, this would:
  -- 1. Decode the effect
  -- 2. Check if the effect is valid for the current account state
  -- 3. Return the validation result
  True

-- Helper functions for encoding/decoding

-- | Encode an integer as a ByteString
encodeInteger :: Integer -> ByteString
encodeInteger = error "encodeInteger: Not implemented"  -- Placeholder

-- | Encode an ActorId as a ByteString
encodeActorId :: ActorId -> ByteString
encodeActorId = error "encodeActorId: Not implemented"  -- Placeholder 