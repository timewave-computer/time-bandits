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
Module: Execution.EffectInterpreter
Description: Core effect application logic with resource-centric concurrency

This module provides the EffectInterpreter, which is responsible for the full lifecycle
of effects in the Time Bandits system. It centralizes precondition checking, effect application,
and ensures causal determinism across the system.

The EffectInterpreter:
1. Locks only the write set of each effect
2. Validates effect preconditions against the current time map
3. Ensures resources have a single owner
4. Applies effects to program state or resource ledger
5. Appends to per-resource logs
6. Releases locks
-}
module Execution.EffectInterpreter 
  ( -- * Core Types
    EffectInterpreter(..)
  , EffectResult(..)
  , EffectContext(..)
  , EffectError(..)
  , ProposedEffectType
  , ProposedEffect(..)
  
  -- * Interpreter Operations
  , createInterpreter
  , interpretEffect
  , applyEffect
  , lockResources
  , releaseResources
  
  -- * Effect Application
  , applyResourceTransfer
  , applyInternalStateUpdate
  , applyAccountMessage
  
  -- * Causal Enforcement
  , enforceResourceOwnership
  , enforceCausalOrder
  , enforceTimeMapConsistency

  -- * Effect Interpreters
  , interpretResourceOp
  , interpretLogicalClock
  , interpretKeyManagement
  , interpretP2PNetwork
  , interpretTransactionEffect
  , interpretAppEffects
  
  -- * Interpreter Configuration
  , TraceConfig(..)
  , InterpreterConfig(..)
  , defaultConfig
  , verboseConfig
  , silentConfig
  , interpretWithConfig
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
-- Replace problematic crypto imports with our own implementations
-- import Crypto.Error (CryptoFailable (..))
import Crypto.Hash.SHA256 qualified as SHA256
-- import Crypto.PubKey.Ed25519 qualified as Ed25519
-- import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.IORef qualified as IORef
import Data.Map.Strict qualified as MapStrict
import Data.Maybe (fromMaybe, isNothing)
import Data.Serialize (Serialize, encode)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Text (Text)
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed, interpret, run, runM)
import Polysemy.Embed (Embed)
import Polysemy.Error (Error, catch, throw, runError)
import Polysemy.Output (Output)
import Polysemy.State qualified as PS
import Polysemy.Trace (Trace(..))
import qualified Polysemy.Trace as PTrace
import System.Random qualified as Random
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as ByteString
import Data.Hashable (Hashable)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TVar (newTVar)
import Control.Monad qualified as M
import qualified Data.Text.Encoding as TE

-- Import from Core modules
import Core.Common (Hash(..), Signature(..), PubKey(..), PrivKey(..))
import qualified Core.Common as Common
import qualified Core.Effect as CoreEffect
import Core.Effect (Effect(..), EffectId)
import Core.Effects (AppEffects,
                    ResourceOps(..),
                    LogicalClock(..),
                    KeyManagement(..),
                    P2PNetwork(..),
                    TransactionEffect(..),
                    EffectHandler(..),
                    ResourceInfo(..))
import qualified Core.Effects as Effects
import Core.ResourceId (ResourceId(..))
import Core.TimelineId (TimelineId(..))
import Core.ProgramId (ProgramId(..))
import Core.TimeMap (TimeMap)
import Core.ResourceLedger (ResourceLedger, ResourceState(..))
import Core.ActorId (ActorId)
import Core.AccountProgram (AccountProgram, AccountMessage(..))
import qualified Core.AccountProgram as AccountProgram
import Core.Types (ResourceInfo(..), ActorType)
import qualified Core.Types as Types

import Execution.ExecutionLog (ExecutionLog)
import qualified Execution.ExecutionLog as ExecutionLog
import Execution.PreconditionEvaluator (PreconditionEvaluator, PreconditionResult(..))
import qualified Execution.PreconditionEvaluator as PE
import Execution.EffectLogger (EffectLogger, LogResult(..))

-- | Extended effect type that includes additional fields needed for interpretation
-- This is an extension of the Core.Effect.Effect type
data ExtendedEffect =
    EIResourceEffect 
      { effectResourceId :: ResourceId
      , effectResourceData :: ByteString
      }
  | EITimelineEffect 
      { effectTimelineId :: TimelineId
      , effectTimelineData :: ByteString
      }
  | EIProgramEffect 
      { effectProgramId :: ProgramId
      , effectProgramData :: ByteString
      }
  | EIAccountMessageEffect 
      { effectActorId :: ActorId
      , effectMessage :: AccountMessage
      }
  | EIDepositEffect
      { depositResourceId :: ResourceId
      , depositAmount :: Integer
      , depositToProgramId :: ProgramId
      }
  | EIWithdrawEffect
      { withdrawResourceId :: ResourceId
      , withdrawAmount :: Integer
      , withdrawFromProgramId :: ProgramId
      }
  | EITransferEffect
      { transferResourceId :: ResourceId
      , transferAmount :: Integer
      , transferFromProgram :: ProgramId
      , transferToProgram :: ProgramId
      }
  | EICreateResourceEffect
      { createResourceData :: ByteString
      , createResourceOwner :: ActorHash
      , createResourceTimeline :: TimelineId
      }
  | EIConsumeResourceEffect
      { consumeResourceId :: ResourceId
      }
  deriving (Eq, Show, Generic)

-- | Convert an ExtendedEffect to a standard Effect
convertToEffect :: ExtendedEffect -> Effect
convertToEffect (EIResourceEffect resId resData) = 
  ResourceEffect resId resData
convertToEffect (EITimelineEffect tlId tlData) = 
  TimelineEffect tlId tlData
convertToEffect (EIProgramEffect progId progData) = 
  ProgramEffect progId progData
convertToEffect (EIAccountMessageEffect actorId msg) = 
  AccountMessageEffect actorId msg
convertToEffect (EIDepositEffect resId amount toProgId) = 
  DepositEffect resId amount toProgId
convertToEffect (EIWithdrawEffect resId amount fromProgId) = 
  WithdrawEffect resId amount fromProgId
convertToEffect (EITransferEffect resId amount fromProg toProg) = 
  TransferEffect resId amount fromProg toProg
convertToEffect (EICreateResourceEffect data' owner timeline) = 
  ResourceEffect (ResourceId "new-resource") data'  -- Use ResourceEffect as a fallback
convertToEffect (EIConsumeResourceEffect resId) = 
  ResourceEffect resId BS.empty  -- Use ResourceEffect as a fallback

-- | Convert a standard Effect to an ExtendedEffect
convertFromEffect :: Effect -> ExtendedEffect
convertFromEffect (ResourceEffect resId resData) = 
  EIResourceEffect resId resData
convertFromEffect (TimelineEffect tlId tlData) = 
  EITimelineEffect tlId tlData
convertFromEffect (ProgramEffect progId progData) = 
  EIProgramEffect progId progData
convertFromEffect (AccountMessageEffect actorId msg) = 
  EIAccountMessageEffect actorId msg
convertFromEffect (DepositEffect resId amount toProgId) = 
  EIDepositEffect resId amount toProgId
convertFromEffect (WithdrawEffect resId amount fromProgId) = 
  EIWithdrawEffect resId amount fromProgId
convertFromEffect (TransferEffect resId amount fromProg toProg) = 
  EITransferEffect resId amount fromProg toProg

-- | Type alias for actor hash
type ActorHash = T.Text

-- | Type for resource log
type ResourceLog = Map.Map ResourceId [T.Text]

-- | Lock status for resources
data LockStatus = 
    Unlocked
  | LockedBy EffectId
  deriving (Eq, Show, Generic)

-- | Table mapping resources to their lock status
type LockTable = Map.Map ResourceId LockStatus

-- | Result of interpreting an effect
data EffectResult =
    EffectApplied Effect BS.ByteString  -- ^ Effect and resulting state hash
  | EffectFailed Text                   -- ^ Effect failed with error message
  | EffectRejected EffectError          -- ^ Effect was rejected due to error
  deriving (Eq, Show, Generic)

-- | Message status
data MessageStatus = Pending | Delivered | Failed
  deriving (Eq, Show, Generic)

-- | Message type for program communication
data MessageType = DepositType | WithdrawType | InvokeType | TransferType | CallbackType | CustomType
  deriving (Eq, Show, Generic)

-- | Convert between our local MessageType and Core.AccountProgram.AccountMessage
messageTypeFromAccountMessage :: AccountProgram.AccountMessage -> MessageType
messageTypeFromAccountMessage (AccountProgram.Deposit _ _ _) = DepositType
messageTypeFromAccountMessage (AccountProgram.Withdraw _ _ _) = WithdrawType
messageTypeFromAccountMessage (AccountProgram.Transfer _ _ _) = TransferType
messageTypeFromAccountMessage (AccountProgram.Invoke _ _ _) = InvokeType
messageTypeFromAccountMessage (AccountProgram.ReceiveCallback _ _) = CallbackType
messageTypeFromAccountMessage _ = CustomType

-- | Convert a string to a MessageType
messageTypeFromString :: String -> MessageType
messageTypeFromString "deposit" = DepositType
messageTypeFromString "withdraw" = WithdrawType
messageTypeFromString "invoke" = InvokeType
messageTypeFromString "transfer" = TransferType
messageTypeFromString "callback" = CallbackType
messageTypeFromString _ = CustomType

-- | A message sent from a program
data SentMessage = SentMessage
  { sentMessageId :: T.Text
  , sentMessageType :: MessageType
  , toProgram :: ProgramId
  , sentPayload :: T.Text
  , sentStatus :: MessageStatus
  , sentTimestamp :: Integer
  }
  deriving (Eq, Show, Generic)

-- | A message received by a program
data ReceivedMessage = ReceivedMessage
  { rcvMessageId :: T.Text
  , rcvMessageType :: MessageType
  , fromProgram :: ProgramId
  , rcvPayload :: T.Text
  , rcvStatus :: MessageStatus
  , rcvTimestamp :: Integer
  }
  deriving (Eq, Show, Generic)

-- | Context for effect interpretation
data EffectContext = EffectContext
  { currentTimeMap :: TimeMap
  , programMemory :: Map.Map ProgramId (Map.Map T.Text BS.ByteString)
  , accountPrograms :: Map.Map ProgramId AccountProgram
  }
  deriving (Show, Generic)

-- | Error types for effect interpretation
data EffectError =
    PreconditionFailed T.Text
  | ResourceLocked ResourceId EffectId
  | TimeMapInconsistency T.Text
  | ResourceOwnershipError T.Text
  | InternalStateError T.Text
  | LoggingError T.Text
  deriving (Eq, Show, Generic)

-- | Application error type
data AppError = 
    ResourceError Text
  | AuthorizationError Text
  | NetworkError Text
  | PersistenceError Text
  | ValidationError Text
  | ConfigurationError Text
  | GenericError Text
  deriving (Eq, Show, Generic)

-- | Modified version of AccountProgram with the fields needed for this module
data LocalAccountProgram = LocalAccountProgram
  { owner :: ProgramId
  , balances :: Map.Map ResourceId Integer
  , inbox :: [ReceivedMessage]
  , outbox :: [SentMessage]
  -- Other fields would go here in a real implementation
  }
  deriving (Eq, Show, Generic)

-- | EffectInterpreter handles the full lifecycle of effects
data EffectInterpreter = EffectInterpreter
  { resourceLedger :: TVar ResourceLedger
  , lockTable :: TVar LockTable
  , preconditionEvaluator :: PreconditionEvaluator
  , effectLogger :: EffectLogger
  , executionContext :: TVar EffectContext
  }
  deriving (Generic)

-- | Create a new effect interpreter
createInterpreter :: 
  ResourceLedger -> 
  PreconditionEvaluator -> 
  EffectLogger -> 
  EffectContext -> 
  IO EffectInterpreter
createInterpreter ledger evaluator logger context = do
  ledgerVar <- STM.atomically $ newTVar ledger
  lockTableVar <- STM.atomically $ newTVar Map.empty
  contextVar <- STM.atomically $ newTVar context
  
  pure $ EffectInterpreter
    { resourceLedger = ledgerVar
    , lockTable = lockTableVar
    , preconditionEvaluator = evaluator
    , effectLogger = logger
    , executionContext = contextVar
    }

-- | Proposed effect with metadata
data ProposedEffectType
  = ProposedEffect_ResourceTransfer ResourceId ProgramId ProgramId Integer
  | ProposedEffect_InternalStateUpdate ProgramId Text ByteString
  | ProposedEffect_AccountMessage ActorId AccountMessage
  | ProposedEffect_CreateResource ByteString ActorHash TimelineId
  | ProposedEffect_ConsumeResource ResourceId
  deriving (Eq, Show, Generic)

-- | Type alias for ProposedEffectType to maintain backward compatibility
type ProposedEffect = ProposedEffectType

-- | Extract the extended effect from a proposed effect
extractEffect :: ProposedEffectType -> ExtendedEffect
extractEffect = \case
  ProposedEffect_ResourceTransfer resId fromProg toProg amount ->
    EITransferEffect resId amount fromProg toProg
  ProposedEffect_InternalStateUpdate progId key value ->
    EIProgramEffect progId (BS.append (TE.encodeUtf8 key) value)
  ProposedEffect_AccountMessage actorId msg ->
    EIAccountMessageEffect actorId msg
  ProposedEffect_CreateResource data' owner timeline ->
    EICreateResourceEffect data' owner timeline
  ProposedEffect_ConsumeResource resId ->
    EIConsumeResourceEffect resId

-- | Get the write set resources that an effect will modify
writeSetResources :: ProposedEffectType -> [ResourceId]
writeSetResources = \case
  ProposedEffect_ResourceTransfer resId _ _ _ -> [resId]
  ProposedEffect_InternalStateUpdate _ _ _ -> []
  ProposedEffect_AccountMessage _ _ -> []
  ProposedEffect_CreateResource _ _ _ -> [] -- New resource doesn't exist yet
  ProposedEffect_ConsumeResource resId -> [resId]

-- | Interpret a proposed effect
interpretEffect :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffectType -> 
  m EffectResult
interpretEffect interpreter proposedEffect = liftIO $ do
  -- 1. Lock resources in the write set
  let extEffect = extractEffect proposedEffect
  let coreEffect = convertToEffect extEffect
  lockResult <- STM.atomically $ lockResources interpreter (writeSetResources proposedEffect) coreEffect
  case lockResult of
    Left err -> pure $ EffectRejected err
    Right _ -> do
      -- 2. Get current context
      context <- STM.readTVarIO (executionContext interpreter)
      
      -- 3. Validate preconditions
      ledger <- STM.readTVarIO (resourceLedger interpreter)
      preconditionResult <- liftIO $ evaluatePreconditions 
        (preconditionEvaluator interpreter) 
        proposedEffect 
        ledger 
        (currentTimeMap context)
      
      case preconditionResult of
        PreconditionsNotSatisfied errs -> do
          -- Release locks if preconditions fail
          STM.atomically $ releaseResources interpreter (writeSetResources proposedEffect)
          pure $ EffectRejected $ PreconditionFailed $ T.pack $ show errs
          
        PreconditionsSatisfied -> do
          -- 4. Apply the effect
          applyResult <- applyEffect interpreter proposedEffect
          
          -- 5. Release locks regardless of application result
          STM.atomically $ releaseResources interpreter (writeSetResources proposedEffect)
          
          -- 6. Return the result
          pure applyResult

-- | Lock resources for an effect
lockResources :: 
  EffectInterpreter -> 
  [ResourceId] -> 
  Effect -> 
  STM.STM (Either EffectError ())
lockResources interpreter resources effect = do
  -- Get current lock table
  locks <- STM.readTVar (lockTable interpreter)
  
  -- Check if any resources are already locked
  let lockedResources = filter (\r -> case Map.lookup r locks of
                                       Just (LockedBy _) -> True
                                       _ -> False) resources
  
  -- If any resources are locked, fail
  if not (null lockedResources)
    then do
      -- Get the first locked resource and its locking effect
      let lockedResource = case lockedResources of
                             (r:_) -> r
                             [] -> error "Impossible: lockedResources is not empty"
          lockingEffect = case Map.lookup lockedResource locks of
                            Just (LockedBy effectId) -> effectId
                            _ -> error "Impossible: resource is locked but no locking effect"
      pure $ Left $ ResourceLocked lockedResource lockingEffect
    else do
      -- Lock all resources
      let effectId = getEffectId effect
          newLocks = foldr (\r acc -> MapStrict.insert r (LockedBy effectId) acc) locks resources
      
      -- Update lock table
      STM.writeTVar (lockTable interpreter) newLocks
      pure $ Right ()

-- | Release resources after effect application
releaseResources :: 
  EffectInterpreter -> 
  [ResourceId] -> 
  STM.STM ()
releaseResources interpreter resources = do
  -- Get current lock table
  locks <- STM.readTVar (lockTable interpreter)
  
  -- Release all locks
  let newLocks = foldr (\r acc -> MapStrict.insert r Unlocked acc) locks resources
  
  -- Update lock table
  STM.writeTVar (lockTable interpreter) newLocks

-- | Apply an effect to the system
applyEffect :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffectType -> 
  m EffectResult
applyEffect interpreter proposedEffect = liftIO $ do
  -- Get current context and ledger
  context <- STM.readTVarIO (executionContext interpreter)
  ledger <- STM.readTVarIO (resourceLedger interpreter)
  
  -- Apply the effect based on its type
  let extEffect = extractEffect proposedEffect
  case extEffect of
    EITransferEffect{} ->
      applyResourceTransfer interpreter proposedEffect ledger
      
    EIProgramEffect{} ->
      applyInternalStateUpdate interpreter proposedEffect context
      
    EIAccountMessageEffect{} ->
      applyAccountMessage interpreter proposedEffect context
      
    _ ->
      -- For other effect types, we would have specific handlers
      pure $ EffectRejected $ InternalStateError "Unsupported effect type"

-- | Apply a resource transfer effect
applyResourceTransfer :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffectType -> 
  ResourceLedger -> 
  m EffectResult
applyResourceTransfer interpreter proposedEffect ledger = do
  -- In a real implementation, this would update the resource ledger
  -- and log the effect
  let extEffect = extractEffect proposedEffect
  let coreEffect = convertToEffect extEffect
  pure $ EffectApplied coreEffect BS.empty

-- | Apply an internal state update effect
applyInternalStateUpdate :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffectType -> 
  EffectContext -> 
  m EffectResult
applyInternalStateUpdate interpreter proposedEffect context = do
  -- In a real implementation, this would update the program memory
  -- and log the effect
  let extEffect = extractEffect proposedEffect
  let coreEffect = convertToEffect extEffect
  pure $ EffectApplied coreEffect BS.empty

-- | Apply an account message effect
applyAccountMessage :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffectType -> 
  EffectContext -> 
  m EffectResult
applyAccountMessage interpreter proposedEffect context = do
  let extEffect = extractEffect proposedEffect
  case extEffect of
    EIAccountMessageEffect actorId message -> do
      -- Get the account program for this actor
      let maybeAccountProgram = findAccountProgram actorId context
      
      case maybeAccountProgram of
        Nothing -> 
          pure $ EffectFailed $ "Account program not found for actor: " <> show actorId
        
        Just accountProgram -> do
          -- Process the message based on its type
          case message of
            AccountProgram.Deposit resource amount to -> do
              -- Check if the account has sufficient balance
              let currentBalance = MapStrict.findWithDefault 0 resource (balances accountProgram)
              if currentBalance >= amount
                then do
                  -- Update the account program's balances
                  let updatedBalances = MapStrict.adjust (\b -> b - amount) resource (balances accountProgram)
                  let updatedAccount = accountProgram { balances = updatedBalances }
                  
                  -- Update the resource ledger to reflect the transfer
                  ledger <- liftIO $ STM.readTVarIO (resourceLedger interpreter)
                  let from = owner accountProgram
                  let updatedLedger = transferResource resource from to amount ledger
                  
                  -- Record the message in the outbox
                  let outboxMsg = createSentMessage "deposit" to (show resource <> ":" <> show amount)
                  let finalAccount = updatedAccount { outbox = outboxMsg : outbox updatedAccount }
                  
                  -- Update the account program in the context
                  updateAccountProgram finalAccount context
                  
                  -- Update the resource ledger
                  liftIO $ STM.atomically $ STM.writeTVar (resourceLedger interpreter) updatedLedger
                  
                  let coreEffect = convertToEffect extEffect
                  pure $ EffectApplied coreEffect BS.empty
                else
                  pure $ EffectFailed $ "Insufficient balance for resource: " <> show resource
            
            _ -> pure $ EffectFailed $ "Unsupported message type"

    _ -> pure $ EffectFailed "Not an account message effect"

-- | Helper function to find an account program by actor ID
findAccountProgram :: ActorId -> EffectContext -> Maybe LocalAccountProgram
findAccountProgram _ _ = Nothing  -- Placeholder, would look up in a real implementation

-- | Helper function to update an account program in the context
updateAccountProgram :: (MonadIO m) => LocalAccountProgram -> EffectContext -> m ()
updateAccountProgram _ _ = liftIO $ pure ()  -- Placeholder, would update in a real implementation

-- | Helper function to create a sent message
createSentMessage :: String -> ProgramId -> T.Text -> SentMessage
createSentMessage typeStr to payload = SentMessage
  { sentMessageId = T.pack $ show $ SHA256.hash $ 
                    BS.concat [
                      ByteString.pack typeStr,
                      ByteString.pack $ show to,
                      TE.encodeUtf8 payload
                    ]
  , sentMessageType = messageTypeFromString typeStr
  , toProgram = to
  , sentPayload = payload
  , sentStatus = Pending
  , sentTimestamp = 0  -- Would use current time in a real implementation
  }

-- | Helper function to create a received message
createReceivedMessage :: String -> ProgramId -> T.Text -> ReceivedMessage
createReceivedMessage typeStr from payload = ReceivedMessage
  { rcvMessageId = T.pack $ show $ SHA256.hash $ 
                   BS.concat [
                     ByteString.pack typeStr,
                     ByteString.pack $ show from,
                     TE.encodeUtf8 payload
                   ]
  , rcvMessageType = messageTypeFromString typeStr
  , fromProgram = from
  , rcvPayload = payload
  , rcvStatus = Pending
  , rcvTimestamp = 0  -- Would use current time in a real implementation
  }

-- | Helper function to get resource owner
getResourceOwner :: ResourceId -> ResourceLedger -> Maybe (ProgramId, ResourceState)
getResourceOwner resource ledger = Map.lookup resource ledger

-- | Helper function to update resource owner
updateResourceOwner :: ResourceId -> ProgramId -> Integer -> ResourceLedger -> IO ResourceLedger
updateResourceOwner resource newOwner amount ledger = do
  currentTime <- getCurrentTime
  case Map.lookup resource ledger of
    Just (_, state) ->
      let newState = state { value = amount }
      in pure $ Map.insert resource (newOwner, newState) ledger
    Nothing ->
      -- Create a new resource state if it doesn't exist
      let newState = ResourceState amount Map.empty currentTime
      in pure $ Map.insert resource (newOwner, newState) ledger

-- | Helper function to transfer a resource
transferResource :: ResourceId -> ProgramId -> ProgramId -> Integer -> ResourceLedger -> ResourceLedger
transferResource resource from to amount ledger =
  case Map.lookup resource ledger of
    Just (owner, state) | owner == from && value state >= amount ->
      let remainingAmount = value state - amount
          remainingState = state { value = remainingAmount }
      in if remainingAmount > 0
         then 
           -- Update the source with remaining amount
           let updatedLedger = Map.insert resource (from, remainingState) ledger
           -- Add or update the target with the transferred amount
           in case Map.lookup resource updatedLedger of
                Just (targetOwner, targetState) | targetOwner == to ->
                  -- Target already has this resource, update the amount
                  let newTargetState = targetState { value = value targetState + amount }
                  in Map.insert resource (to, newTargetState) updatedLedger
                _ ->
                  -- Target doesn't have this resource yet, create a new entry
                  -- In a real implementation, we would use the current time from state
                  let newTargetState = ResourceState amount Map.empty (lastUpdated state)
                  in Map.insert resource (to, newTargetState) updatedLedger
         else
           -- Remove the source entry and create a target entry
           let updatedLedger = Map.delete resource ledger
               -- In a real implementation, we would use the current time from state
               newTargetState = ResourceState amount Map.empty (lastUpdated state)
           in Map.insert resource (to, newTargetState) updatedLedger
    _ -> ledger  -- No change if conditions not met

-- | Log an effect (placeholder)
logEffect :: (MonadIO m) => EffectInterpreter -> ProposedEffectType -> Execution.EffectInterpreter.EffectResult -> m ()
logEffect _ _ _ = pure ()  -- Placeholder, would log in a real implementation

-- | Get the effect ID for an effect
getEffectId :: Effect -> EffectId
getEffectId = CoreEffect.getEffectId

-- | Evaluate preconditions (placeholder)
evaluatePreconditions :: 
  PreconditionEvaluator -> 
  ProposedEffectType -> 
  ResourceLedger -> 
  TimeMap -> 
  IO PreconditionResult
evaluatePreconditions _ _ _ _ = pure PreconditionsSatisfied  -- Placeholder

-- | Interpret the ResourceOperationEffect
interpretResourceOp :: Member (Trace) r => IORef.IORef ResourceLog -> Sem (ResourceOps ': r) a -> Sem r a
interpretResourceOp resourceLog = interpret $ \case
    CreateResource metadata ownerHash timelineId -> do
        PTrace.trace "Creating resource..."
        -- Implementation would create a resource and return its info
        -- Create a ResourceInfo with the correct type structure
        pure $ Right $ ResourceInfo 
            { resourceId = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 "dummy" -- ResourceHash
            , resourceOrigin = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 $ T.pack $ show timelineId -- TimelineHash 
            , resourceOwner = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 $ T.pack $ show ownerHash -- ActorHash
            , resourceCapabilities = [] -- List of ResourceCapability
            , resourceMeta = metadata -- ByteString
            , resourceSpentBy = Nothing -- Maybe Hash
            , resourceParents = [] -- [ResourceHash]
            , resourceTimestamp = Common.LamportTime 0 -- LamportTime
            , resourceProvenanceChain = [Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 $ T.pack $ show timelineId] -- [TimelineHash]
            }

    TransferResource resourceInfo newOwnerHash timelineId -> do
        PTrace.trace "Transferring resource..."
        -- Implementation would transfer ownership and return updated info
        let updatedResource = resourceInfo {
            resourceOwner = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 $ T.pack $ show newOwnerHash,
            resourceProvenanceChain = resourceProvenanceChain resourceInfo ++ 
                                     [Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 $ T.pack $ show timelineId]
        }
        pure $ Right $ updatedResource

    ConsumeResource resourceInfo -> do
        PTrace.trace "Consuming resource..."
        -- Implementation would mark resource as consumed
        let updatedResource = resourceInfo {
            resourceSpentBy = Just (Hash "consumed")
        }
        pure $ Right $ updatedResource

    VerifyResource resourceInfo -> do
        PTrace.trace "Verifying resource..."
        -- Implementation would verify resource validity
        pure $ Right True

    GetResource hash -> do
        PTrace.trace "Getting resource by hash..."
        -- Implementation would look up resource by hash
        pure $ Right $ ResourceInfo
            { resourceId = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 $ T.pack $ show hash -- ResourceHash
            , resourceOrigin = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 "origin-timeline" -- TimelineHash
            , resourceOwner = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 "dummy-owner" -- ActorHash
            , resourceCapabilities = [] -- List of ResourceCapability
            , resourceMeta = "metadata" -- ByteString
            , resourceSpentBy = Nothing -- Maybe Hash
            , resourceParents = [] -- [ResourceHash]
            , resourceTimestamp = Common.LamportTime 0 -- LamportTime
            , resourceProvenanceChain = [Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 "origin-timeline"] -- [TimelineHash]
            }

    GetResourcesByOwner ownerHash -> do
        PTrace.trace "Getting resources by owner..."
        -- Implementation would find all resources owned by actor
        pure $ Right [ResourceInfo
            { resourceId = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 "dummy-resource" -- ResourceHash
            , resourceOrigin = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 "origin-timeline" -- TimelineHash
            , resourceOwner = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 $ T.pack $ show ownerHash -- ActorHash
            , resourceCapabilities = [] -- List of ResourceCapability
            , resourceMeta = "metadata" -- ByteString
            , resourceSpentBy = Nothing -- Maybe Hash
            , resourceParents = [] -- [ResourceHash]
            , resourceTimestamp = Common.LamportTime 0 -- LamportTime
            , resourceProvenanceChain = [Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 "origin-timeline"] -- [TimelineHash]
            }]

    GetResourcesByTimeline timelineId -> do
        PTrace.trace "Getting resources by timeline..."
        -- Implementation would find all resources in timeline
        pure $ Right [ResourceInfo
            { resourceId = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 "dummy-resource" -- ResourceHash
            , resourceOrigin = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 $ T.pack $ show timelineId -- TimelineHash
            , resourceOwner = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 "dummy-owner" -- ActorHash
            , resourceCapabilities = [] -- List of ResourceCapability
            , resourceMeta = "metadata" -- ByteString
            , resourceSpentBy = Nothing -- Maybe Hash
            , resourceParents = [] -- [ResourceHash]
            , resourceTimestamp = Common.LamportTime 0 -- LamportTime
            , resourceProvenanceChain = [Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 $ T.pack $ show timelineId] -- [TimelineHash]
            }]

-- | Interpret the LogicalClock effect
interpretLogicalClock :: Member (Embed IO) r => IORef.IORef Effects.LamportTime -> Sem (LogicalClock ': r) a -> Sem r a
interpretLogicalClock clockRef = interpret $ \case
    GetLamportTime -> embed $ IORef.readIORef clockRef
    IncrementTime -> embed $ do
        lt <- IORef.readIORef clockRef
        let t = case lt of
                  Effects.LamportTime n -> n
        let newTime = Effects.LamportTime (t + 1)
        IORef.writeIORef clockRef newTime
        pure newTime
    UpdateTime newTime -> embed $ do
        current <- IORef.readIORef clockRef
        let currentValue = case current of
                             Effects.LamportTime n -> n
        let newValue = case newTime of
                         Effects.LamportTime n -> n
        M.when (newValue > currentValue) $
            IORef.writeIORef clockRef newTime
        pure newTime

-- | Interpret the KeyManagement effect
interpretKeyManagement :: (Member (Embed IO) r, Member (Trace) r) => IORef.IORef (Map.Map String ByteString) -> Sem (KeyManagement ': r) a -> Sem r a
interpretKeyManagement keyStore = interpret $ \case
    GenerateKeyPair -> do
        -- In a real implementation, this would generate a proper key pair
        -- For now, we just return dummy values
        pure (Common.PubKey "dummy-pub", Common.PrivKey "dummy-priv")
        
    RegisterPublicKey actorHash pubKey -> embed $ do
        keyMap <- IORef.readIORef keyStore
        -- Convert ActorHash to a String key
        let key = T.unpack $ T.pack $ show actorHash
        -- Convert PubKey to ByteString - use a safer approach than show
        let pubKeyBytes = case pubKey of
                            Common.PubKey bs -> BS.copy bs
        IORef.writeIORef keyStore (MapStrict.insert key pubKeyBytes keyMap)
        pure ()
        
    LookupPublicKey actorHash -> embed $ do
        keyMap <- IORef.readIORef keyStore
        -- Convert ActorHash to a String key
        let key = T.unpack $ T.pack $ show actorHash
        pure $ case Map.lookup key keyMap of
            Just pubKeyBytes -> Just $ Common.PubKey pubKeyBytes
            Nothing -> Nothing
            
    SignData privateKey message -> do
        PTrace.trace "Signing data..."
        -- In a real implementation, this would use proper cryptography
        pure $ Right $ Signature "dummy-signature"
        
    VerifyWithPublicKey publicKey message signature -> do
        PTrace.trace "Verifying signature..."
        -- In a real implementation, this would verify the signature
        pure True
        
    RegisterActorType actorHash actorType -> do
        PTrace.trace $ "Registering actor type: " <> show actorType
        pure ()
        
    LookupActorType actorHash -> do
        PTrace.trace "Looking up actor type..."
        -- Return a dummy actor type for now
        pure $ Just $ Types.TimeTraveler

-- | Interpret the P2PNetwork effect
interpretP2PNetwork :: (Member (Embed IO) r, Member (Trace) r) => IORef.IORef [Effects.P2PNode] -> Sem (P2PNetwork ': r) a -> Sem r a
interpretP2PNetwork nodesRef = interpret $ \case
    DiscoverNodes -> do
        -- In a real implementation, this would discover nodes on the network
        -- For now, we just return an empty list
        pure []
        
    ConnectToNode node -> do
        -- Extract nodeId using pattern matching directly
        let nId = case node of
                    Effects.P2PNode nodeId _ _ -> nodeId
        PTrace.trace $ "Connecting to node: " <> show nId
        -- In a real implementation, this would establish a connection
        -- For now, we just return success
        pure True
        
    DisconnectFromNode nodeId -> do
        PTrace.trace $ "Disconnecting from node: " <> show nodeId
        -- In a real implementation, this would close a connection
        -- For now, we just return success
        pure True
        
    SendMessage nodeId message -> do
        PTrace.trace $ "Sending message to node: " <> show nodeId
        -- In a real implementation, this would send a message
        -- For now, we just return success
        pure True
        
    BroadcastMessage message -> do
        PTrace.trace "Broadcasting message to all nodes"
        -- In a real implementation, this would broadcast to all connected nodes
        -- For now, we just return 0 (no nodes received the message)
        pure 0
        
    ReceiveMessage -> do
        PTrace.trace "Waiting for message..."
        -- In a real implementation, this would wait for a message
        -- For now, we just return Nothing (no message received)
        pure Nothing

-- | Interpret the TransactionEffect effect
interpretTransactionEffect :: (Member (Embed IO) r, Member (Trace) r) => Sem (TransactionEffect ': r) a -> Sem r a
interpretTransactionEffect = interpret $ \case
    BeginTransaction -> do
        PTrace.trace "Beginning transaction..."
        -- In a real implementation, this would start a transaction
        -- and return a transaction ID. For now, we generate a random ID
        -- but don't return it (we just create it as an effect)
        liftIO $ do
            g <- Random.getStdGen
            let randomBytes = take 32 $ show g
            let _ = ByteString.pack randomBytes  -- Generate but ignore the ID
            pure ()
        
    CommitTransaction -> do
        PTrace.trace "Committing transaction"
        -- In a real implementation, this would commit a transaction
        -- For now, we just return success
        pure True
        
    RollbackTransaction -> do
        PTrace.trace "Rolling back transaction"
        -- In a real implementation, this would roll back a transaction
        -- For now, we just return success
        pure True

-- | Interpret the EffectHandler effect
interpretEffectHandler :: Member (Trace) r => Sem (EffectHandler ': r) a -> Sem r a
interpretEffectHandler = interpret $ \case
    ApplyEffect effect metadata -> do
        PTrace.trace $ "Applying effect: " <> show effect
        -- In a real implementation, this would apply the effect
        -- For now, we just return success
        pure $ CoreEffect.EffectSuccess "dummy-result"
        
    ValidateEffectPreconditions effect facts -> do
        PTrace.trace $ "Validating effect preconditions: " <> show effect
        -- In a real implementation, this would validate the preconditions
        -- For now, we just return success
        pure True

-- | Type list concatenation
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
    '[] ++ bs = bs
    (a ': as) ++ bs = a ': (as ++ bs)

-- | Helper to convert our local EffectResult to Core.Effect.EffectResult
toEffectResult :: Execution.EffectInterpreter.EffectResult -> CoreEffect.EffectResult
toEffectResult (Execution.EffectInterpreter.EffectApplied _ _) = CoreEffect.EffectSuccess "success"
toEffectResult (Execution.EffectInterpreter.EffectFailed _) = CoreEffect.EffectFailure "failed"
toEffectResult (Execution.EffectInterpreter.EffectRejected _) = CoreEffect.EffectFailure "rejected"

-- | Type alias for all application effects in our interpreter
type InterpreterEffects r =
    '[ ResourceOps
     , LogicalClock
     , KeyManagement
     , P2PNetwork
     , TransactionEffect
     , EffectHandler
     , Trace
     , Error AppError
     , Embed IO
     ]

-- | Interpret all application effects
interpretAppEffects :: IORef.IORef ResourceLog
                     -> IORef.IORef (Map.Map String ByteString)
                     -> IORef.IORef Effects.LamportTime
                     -> IORef.IORef [Effects.P2PNode]
                     -> Sem (InterpreterEffects r) a
                     -> IO (Either AppError a)
interpretAppEffects resourceLog keyStore clockRef nodesRef program =
    runM
    . runError
    . runTraceIO
    . interpretEffectHandler
    . interpretTransactionEffect
    . interpretP2PNetwork nodesRef
    . interpretKeyManagement keyStore
    . interpretLogicalClock clockRef
    . interpretResourceOp resourceLog
    $ program
  where
    runTraceIO :: Member (Embed IO) r => Sem (Trace ': r) a -> Sem r a
    runTraceIO = interpret $ \case
        Trace message -> liftIO $ putStrLn message

-- | Helper function to run a trace effect with configuration
runTraceWithConfig :: Member (Embed IO) r => InterpreterConfig -> Sem (Trace ': r) a -> Sem r a
runTraceWithConfig config = interpret $ \case
    Trace message -> liftIO $ M.when (traceEnabled config) $ 
        putStrLn $ T.unpack (tracePrefix config) <> message

-- | Trace verbosity configuration
data TraceConfig
  = Silent        -- ^ No tracing
  | Normal        -- ^ Standard tracing
  | Verbose       -- ^ Verbose tracing with full context
  deriving (Show, Eq, Generic)

-- | Configuration for the interpreter
data InterpreterConfig = InterpreterConfig
  { icTraceLevel :: TraceConfig           -- ^ How much to trace
  , icEnforceOwnership :: Bool            -- ^ Whether to strictly enforce resource ownership
  , icEnforceCausality :: Bool            -- ^ Whether to enforce causal order
  , icMaxTries :: Int                     -- ^ Maximum retries for failed effects
  , icTimeout :: Int                      -- ^ Timeout in milliseconds
  , icResilienceStrategy :: Text          -- ^ Strategy for handling failures
  , traceEnabled :: Bool                  -- ^ Whether tracing is enabled
  , tracePrefix :: Text                   -- ^ Prefix for trace messages
  }
  deriving (Show, Generic)

-- | Default interpreter configuration
defaultConfig :: InterpreterConfig
defaultConfig = InterpreterConfig
  { icTraceLevel = Normal
  , icEnforceOwnership = True
  , icEnforceCausality = True
  , icMaxTries = 3
  , icTimeout = 5000
  , icResilienceStrategy = "exponential-backoff"
  , traceEnabled = True
  , tracePrefix = ""
  }

-- | Verbose interpreter configuration
verboseConfig :: InterpreterConfig
verboseConfig = defaultConfig { icTraceLevel = Verbose }

-- | Silent interpreter configuration
silentConfig :: InterpreterConfig
silentConfig = defaultConfig { icTraceLevel = Silent }

-- | Interpret with configuration
interpretWithConfig :: InterpreterConfig -> Effect -> IO (CoreEffect.EffectResult)
interpretWithConfig config effect = do
    -- Create necessary IORef instances
    resourceLog <- IORef.newIORef Map.empty
    keyStore <- IORef.newIORef Map.empty
    clockRef <- IORef.newIORef (Common.LamportTime 0)
    nodesRef <- IORef.newIORef []
    
    -- Run the effect through the interpreter
    result <- interpretAppEffects resourceLog keyStore clockRef nodesRef $ do
        -- Implementation would go here
        PTrace.trace "Applying effect with configuration"
        pure $ CoreEffect.EffectSuccess "dummy-result"
        
    case result of
        Left err -> pure $ CoreEffect.EffectFailure $ T.pack $ show err
        Right r -> pure r

-- | Causal enforcement
enforceResourceOwnership :: EffectInterpreter -> ResourceId -> Bool
enforceResourceOwnership _ _ = True  -- Placeholder

enforceCausalOrder :: EffectInterpreter -> Bool
enforceCausalOrder _ = True  -- Placeholder

enforceTimeMapConsistency :: EffectInterpreter -> Bool
enforceTimeMapConsistency _ = True  -- Placeholder

 