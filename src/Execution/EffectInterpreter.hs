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
import Data.Text (Text)
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed, interpret, run, runM)
import Polysemy.Embed (Embed)
import Polysemy.Error (Error, catch, throw)
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

-- Import from Core modules
import Core.Common (Hash(..), Signature(..))
import qualified Core.Common as Common
import qualified Core.Effect as CoreEffect
import Core.Effect (EffectId)
import Core.Effects (Effect, AppEffects,
                    ResourceOps(..),
                    LogicalClock(..),
                    KeyManagement(..),
                    P2PNetwork(..),
                    TransactionEffect(..),
                    EffectHandler(..))
import qualified Core.Effects as Effects
import Core.ResourceId (ResourceId(..))
import Core.TimelineId (TimelineId(..))
import Core.ProgramId (ProgramId(..))
import Core.TimeMap (TimeMap)
import Core.ResourceLedger (ResourceLedger, ResourceState(..))
import Core.ActorId (ActorId)
import Core.AccountProgram (AccountProgram)

import Execution.ExecutionLog (ExecutionLog)
import qualified Execution.ExecutionLog as ExecutionLog
import Execution.PreconditionEvaluator (PreconditionEvaluator, PreconditionResult(..), ProposedEffect(..))
import Execution.EffectLogger (EffectLogger, LogResult(..))

-- | Type alias for actor hash
type ActorHash = T.Text

-- | Type for resource log
type ResourceLog = Map.Map ResourceId [T.Text]

-- | Lamport timestamp for logical clocks
data LamportTime = LamportTime Integer
  deriving (Eq, Ord, Show, Generic)

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
  | EffectRejected EffectError
  deriving (Eq, Show, Generic)

-- | Message status
data MessageStatus = Pending | Delivered | Failed
  deriving (Eq, Show, Generic)

-- | Message type for program communication
data MessageType = Deposit | Withdraw | Invoke | Transfer | ReceiveCallback | CustomMessage
  deriving (Eq, Show, Generic)

-- | Account message types
data AccountMessage = 
    DepositMessage ResourceId Integer ProgramId
  | WithdrawMessage ResourceId Integer ProgramId
  | InvokeMessage ProgramId Text [Text]
  | ReceiveCallbackMessage ProgramId Text
  | CustomAccountMessage Text Text
  deriving (Eq, Show, Generic)

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
  ledgerVar <- newTVar ledger
  lockTableVar <- newTVar Map.empty
  contextVar <- newTVar context
  
  pure $ EffectInterpreter
    { resourceLedger = ledgerVar
    , lockTable = lockTableVar
    , preconditionEvaluator = evaluator
    , effectLogger = logger
    , executionContext = contextVar
    }

-- | Extract the effect from a proposed effect
extractEffect :: ProposedEffect -> ExtendedEffect
extractEffect (ProposedEffect e _ _ _) = e

-- | Extract the write set from a proposed effect
writeSet :: ProposedEffect -> [ResourceId]
writeSet (ProposedEffect _ _ _ ws) = ws

-- | Interpret a proposed effect
interpretEffect :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffect -> 
  m EffectResult
interpretEffect interpreter proposedEffect = liftIO $ do
  -- 1. Lock resources in the write set
  lockResult <- STM.atomically $ lockResources interpreter (Execution.EffectInterpreter.writeSet proposedEffect) (Execution.EffectInterpreter.extractEffect proposedEffect)
  case lockResult of
    Left err -> pure $ Execution.EffectInterpreter.EffectRejected err
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
          STM.atomically $ releaseResources interpreter (Execution.EffectInterpreter.writeSet proposedEffect)
          pure $ Execution.EffectInterpreter.EffectRejected $ PreconditionFailed $ T.pack $ show errs
          
        PreconditionsSatisfied -> do
          -- 4. Apply the effect
          applyResult <- applyEffect interpreter proposedEffect
          
          -- 5. Release locks regardless of application result
          STM.atomically $ releaseResources interpreter (Execution.EffectInterpreter.writeSet proposedEffect)
          
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
      let lockedResource = head lockedResources
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
  ProposedEffect -> 
  m EffectResult
applyEffect interpreter proposedEffect = liftIO $ do
  -- Get current context and ledger
  context <- STM.readTVarIO (executionContext interpreter)
  ledger <- STM.readTVarIO (resourceLedger interpreter)
  
  -- Apply the effect based on its type
  case Execution.EffectInterpreter.extractEffect proposedEffect of
    ResourceTransfer{} ->
      applyResourceTransfer interpreter proposedEffect ledger
      
    InternalStateUpdate{} ->
      applyInternalStateUpdate interpreter proposedEffect context
      
    AccountMessageEffect{} ->
      applyAccountMessage interpreter proposedEffect context
      
    _ ->
      -- For other effect types, we would have specific handlers
      pure $ Execution.EffectInterpreter.EffectRejected $ InternalStateError "Unsupported effect type"

-- | Apply a resource transfer effect
applyResourceTransfer :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffect -> 
  ResourceLedger -> 
  m EffectResult
applyResourceTransfer interpreter proposedEffect ledger = do
  -- In a real implementation, this would update the resource ledger
  -- and log the effect
  pure $ Execution.EffectInterpreter.EffectApplied (Execution.EffectInterpreter.extractEffect proposedEffect) BS.empty

-- | Apply an internal state update effect
applyInternalStateUpdate :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffect -> 
  EffectContext -> 
  m EffectResult
applyInternalStateUpdate interpreter proposedEffect context = do
  -- In a real implementation, this would update the program memory
  -- and log the effect
  pure $ Execution.EffectInterpreter.EffectApplied (Execution.EffectInterpreter.extractEffect proposedEffect) BS.empty

-- | Apply an account message effect
applyAccountMessage :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffect -> 
  EffectContext -> 
  m EffectResult
applyAccountMessage interpreter proposedEffect context = do
  case Execution.EffectInterpreter.extractEffect proposedEffect of
    AccountMessageEffect actorId message -> do
      -- Get the account program for this actor
      let maybeAccountProgram = findAccountProgram actorId context
      
      case maybeAccountProgram of
        Nothing -> 
          pure $ Execution.EffectInterpreter.EffectFailed $ "Account program not found for actor: " <> show actorId
        
        Just accountProgram -> do
          -- Process the message based on its type
          result <- case message of
            DepositMessage resource amount to -> do
              -- Check if the account has sufficient balance
              let currentBalance = MapStrict.findWithDefault 0 resource (balances accountProgram)
              if currentBalance >= amount
                then do
                  -- Update the account program's balances
                  let updatedBalances = MapStrict.adjust (\b -> b - amount) resource (balances accountProgram)
                  let updatedAccount = accountProgram { balances = updatedBalances }
                  
                  -- Update the resource ledger to reflect the transfer
                  ledger <- STM.readTVarIO (resourceLedger interpreter)
                  let updatedLedger = updateResourceOwner resource to amount ledger
                  
                  -- Record the message in the outbox
                  let outboxMsg = createSentMessage "deposit" to (show resource <> ":" <> show amount)
                  let finalAccount = updatedAccount { outbox = outboxMsg : outbox updatedAccount }
                  
                  -- Update the account program in the context
                  updateAccountProgram finalAccount context
                  
                  -- Update the resource ledger
                  STM.atomically $ STM.writeTVar (resourceLedger interpreter) updatedLedger
                  
                  pure $ Execution.EffectInterpreter.EffectApplied (Execution.EffectInterpreter.extractEffect proposedEffect) (encode finalAccount)
                else
                  pure $ Execution.EffectInterpreter.EffectFailed $ "Insufficient balance for resource: " <> show resource
            
            WithdrawMessage resource amount from -> do
              -- Check if the target program owns the resource
              ledger <- STM.readTVarIO (resourceLedger interpreter)
              let ownership = getResourceOwner resource ledger
              
              case ownership of
                Just (owner, _) | owner == from -> do
                  -- Update the resource ledger
                  let updatedLedger = transferResource resource from (owner accountProgram) amount ledger
                  
                  -- Update the account program's balances
                  let updatedBalances = MapStrict.insertWith (+) resource amount (balances accountProgram)
                  let updatedAccount = accountProgram { balances = updatedBalances }
                  
                  -- Record the message in the outbox
                  let outboxMsg = createSentMessage "withdraw" from (show resource <> ":" <> show amount)
                  let finalAccount = updatedAccount { outbox = outboxMsg : outbox updatedAccount }
                  
                  -- Update the account program in the context
                  updateAccountProgram finalAccount context
                  
                  -- Update the resource ledger
                  STM.atomically $ STM.writeTVar (resourceLedger interpreter) updatedLedger
                  
                  pure $ Execution.EffectInterpreter.EffectApplied (Execution.EffectInterpreter.extractEffect proposedEffect) (encode finalAccount)
                
                _ -> pure $ Execution.EffectInterpreter.EffectFailed $ "Resource not owned by target program: " <> show resource
            
            InvokeMessage targetProgram entrypoint args -> do
              -- Create a message in the outbox
              let payload = "entrypoint:" <> entrypoint <> ",args:" <> T.intercalate "," args
              let outboxMsg = createSentMessage "invoke" targetProgram payload
              let updatedAccount = accountProgram { outbox = outboxMsg : outbox accountProgram }
              
              -- Update the account program in the context
              updateAccountProgram updatedAccount context
              
              pure $ Execution.EffectInterpreter.EffectApplied (Execution.EffectInterpreter.extractEffect proposedEffect) (encode updatedAccount)
            
            ReceiveCallbackMessage sourceProgram payload -> do
              -- Create a message in the inbox
              let inboxMsg = createReceivedMessage "callback" sourceProgram payload
              let updatedAccount = accountProgram { inbox = inboxMsg : inbox accountProgram }
              
              -- Update the account program in the context
              updateAccountProgram updatedAccount context
              
              pure $ Execution.EffectInterpreter.EffectApplied (Execution.EffectInterpreter.extractEffect proposedEffect) (encode updatedAccount)
            
            CustomAccountMessage messageType payload -> do
              -- Handle custom messages based on type
              -- This is a placeholder for extensibility
              let inboxMsg = createReceivedMessage (T.unpack messageType) (owner accountProgram) payload
              let updatedAccount = accountProgram { inbox = inboxMsg : inbox accountProgram }
              
              -- Update the account program in the context
              updateAccountProgram updatedAccount context
              
              pure $ Execution.EffectInterpreter.EffectApplied (Execution.EffectInterpreter.extractEffect proposedEffect) (encode updatedAccount)
          
          -- Log the effect
          logEffect interpreter proposedEffect result
          
          pure result
    
    _ -> pure $ Execution.EffectInterpreter.EffectFailed "Not an account message effect"

-- | Helper function to find an account program by actor ID
findAccountProgram :: ActorId -> EffectContext -> Maybe LocalAccountProgram
findAccountProgram _ _ = Nothing  -- Placeholder, would look up in a real implementation

-- | Helper function to update an account program in the context
updateAccountProgram :: LocalAccountProgram -> EffectContext -> IO ()
updateAccountProgram _ _ = pure ()  -- Placeholder, would update in a real implementation

-- | Helper function to create a sent message
createSentMessage :: String -> ProgramId -> T.Text -> SentMessage
createSentMessage typeStr to payload = SentMessage
  { sentMessageId = T.pack $ show (hash (typeStr <> show to <> T.unpack payload))
  , sentMessageType = messageTypeFromString typeStr
  , toProgram = to
  , sentPayload = payload
  , sentStatus = Pending
  , sentTimestamp = 0  -- Would use current time in a real implementation
  }

-- | Helper function to create a received message
createReceivedMessage :: String -> ProgramId -> T.Text -> ReceivedMessage
createReceivedMessage typeStr from payload = ReceivedMessage
  { rcvMessageId = T.pack $ show (hash (typeStr <> show from <> T.unpack payload))
  , rcvMessageType = messageTypeFromString typeStr
  , fromProgram = from
  , rcvPayload = payload
  , rcvStatus = Pending
  , rcvTimestamp = 0  -- Would use current time in a real implementation
  }

-- | Convert a string to a MessageType
messageTypeFromString :: String -> MessageType
messageTypeFromString "deposit" = Deposit
messageTypeFromString "withdraw" = Withdraw
messageTypeFromString "invoke" = Invoke
messageTypeFromString "transfer" = Transfer
messageTypeFromString "callback" = ReceiveCallback
messageTypeFromString _ = CustomMessage

-- | Helper function to get resource owner
getResourceOwner :: ResourceId -> ResourceLedger -> Maybe (ProgramId, ResourceState)
getResourceOwner resource ledger = Map.lookup resource ledger

-- | Helper function to update resource owner
updateResourceOwner :: ResourceId -> ProgramId -> Integer -> ResourceLedger -> ResourceLedger
updateResourceOwner resource newOwner amount ledger =
  Map.insert resource (newOwner, ResourceState amount) ledger

-- | Helper function to transfer a resource
transferResource :: ResourceId -> ProgramId -> ProgramId -> Integer -> ResourceLedger -> Maybe ResourceLedger
transferResource resource from to amount ledger =
  case getResourceOwner resource ledger of
    Just (owner, ResourceState currentAmount) | owner == from && currentAmount >= amount ->
      let remainingAmount = currentAmount - amount
      in Just $ if remainingAmount > 0
         then MapStrict.insert resource (from, ResourceState remainingAmount) ledger
         else MapStrict.delete resource ledger
    _ -> Nothing

-- | Log an effect (placeholder)
logEffect :: (MonadIO m) => EffectInterpreter -> ProposedEffect -> Execution.EffectInterpreter.EffectResult -> m ()
logEffect _ _ _ = pure ()  -- Placeholder, would log in a real implementation

-- | Get the effect ID from an effect
getEffectId :: ExtendedEffect -> CoreEffect.EffectId
getEffectId _ = BS.empty  -- Placeholder

-- | Evaluate preconditions (placeholder)
evaluatePreconditions :: 
  PreconditionEvaluator -> 
  ProposedEffect -> 
  ResourceLedger -> 
  TimeMap -> 
  IO PreconditionResult
evaluatePreconditions _ _ _ _ = pure PreconditionsSatisfied  -- Placeholder

-- | Interpret the ResourceOperationEffect
interpretResourceOp :: IORef.IORef ResourceLog -> Sem (ResourceOps ': r) a -> Sem r a
interpretResourceOp resourceLog = interpret $ \case
    CreateResource metadata ownerHash timelineId -> do
        PTrace.trace "Creating resource..."
        -- Implementation would create a resource and return its info
        pure $ Right $ ResourceInfo (ResourceId "dummy") ownerHash timelineId metadata

    TransferResource resource newOwnerHash timelineId -> do
        PTrace.trace "Transferring resource..."
        -- Implementation would transfer ownership and return updated info
        pure $ Right $ ResourceInfo (ResourceId "dummy") newOwnerHash timelineId "transferred"

    ConsumeResource resource -> do
        PTrace.trace "Consuming resource..."
        -- Implementation would mark resource as consumed
        pure $ Right $ ResourceInfo (ResourceId "dummy") "consumed" (TimelineId "main") "consumed"

    VerifyResource resource -> do
        PTrace.trace "Verifying resource..."
        -- Implementation would verify resource validity
        pure $ Right True

    GetResource hash -> do
        PTrace.trace "Getting resource by hash..."
        -- Implementation would look up resource by hash
        pure $ Right $ ResourceInfo (ResourceId "dummy") "owner" (TimelineId "main") "metadata"

    GetResourcesByOwner ownerHash -> do
        PTrace.trace "Getting resources by owner..."
        -- Implementation would find all resources owned by actor
        pure $ Right [ResourceInfo (ResourceId "dummy") ownerHash (TimelineId "main") "metadata"]

    GetResourcesByTimeline timelineId -> do
        PTrace.trace "Getting resources by timeline..."
        -- Implementation would find all resources in timeline
        pure $ Right [ResourceInfo (ResourceId "dummy") "owner" timelineId "metadata"]

-- | Interpret the LogicalClock effect
interpretLogicalClock :: IORef.IORef LamportTime -> Sem (LogicalClock ': r) a -> Sem r a
interpretLogicalClock clockRef = interpret $ \case
    GetLamportTime -> embed $ IORef.readIORef clockRef
    IncrementTime -> embed $ do
        lt@(LamportTime t) <- IORef.readIORef clockRef
        let newTime = LamportTime (t + 1)
        IORef.writeIORef clockRef newTime
        pure newTime
    UpdateTime newTime -> embed $ do
        (LamportTime current) <- IORef.readIORef clockRef
        let (LamportTime new) = newTime
        M.when (new > current) $
            IORef.writeIORef clockRef newTime
        pure newTime

-- | Interpret the KeyManagement effect
interpretKeyManagement :: IORef.IORef (Map.Map ActorHash ByteString) -> Sem (KeyManagement ': r) a -> Sem r a
interpretKeyManagement keyStore = interpret $ \case
    GenerateKeyPair -> embed $ do
        -- In a real implementation, this would generate a proper key pair
        -- For now, we just return dummy values
        pure (PubKey "dummy-pub", PrivKey "dummy-priv")
        
    RegisterPublicKey actorHash pubKey -> embed $ do
        keyMap <- IORef.readIORef keyStore
        IORef.writeIORef keyStore (MapStrict.insert actorHash (BS.pack $ show pubKey) keyMap)
        pure ()
        
    LookupPublicKey actorHash -> embed $ do
        keyMap <- IORef.readIORef keyStore
        pure $ case Map.lookup actorHash keyMap of
            Just pubKeyBytes -> Just $ PubKey (BS.unpack pubKeyBytes)
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
        pure $ Just $ ActorType "dummy-actor-type"

-- | Interpret the P2PNetwork effect
interpretP2PNetwork :: IORef.IORef [Effects.P2PNode] -> Sem (P2PNetwork ': r) a -> Sem r a
interpretP2PNetwork nodesRef = interpret $ \case
    DiscoverNodes -> embed $ do
        -- In a real implementation, this would discover nodes on the network
        -- For now, we just return an empty list
        pure []
        
    ConnectToNode node -> do
        PTrace.trace $ "Connecting to node: " <> show (nodeId node)
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
interpretTransactionEffect :: Sem (TransactionEffect ': r) a -> Sem r a
interpretTransactionEffect = interpret $ \case
    BeginTransaction -> do
        PTrace.trace "Beginning transaction..."
        -- In a real implementation, this would start a transaction
        -- For now, we just generate a random transaction ID
        embed $ do
            randomBytes <- Random.getStdGen >>= \g -> pure $ take 32 $ show g
            pure $ ByteString.pack $ take 32 randomBytes
        
    CommitTransaction txId -> do
        PTrace.trace $ "Committing transaction: " <> show txId
        -- In a real implementation, this would commit a transaction
        -- For now, we just return success
        pure True
        
    RollbackTransaction txId -> do
        PTrace.trace $ "Rolling back transaction: " <> show txId
        -- In a real implementation, this would roll back a transaction
        -- For now, we just return success
        pure True

-- | Interpret the EffectHandler effect
interpretEffectHandler :: Sem (EffectHandler ': r) a -> Sem r a
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
toEffectResult (Execution.EffectInterpreter.EffectRejected _) = CoreEffect.EffectFailure "failed"

-- | Interpret all application effects
interpretAppEffects :: IORef.IORef ResourceLog
                     -> IORef.IORef (Map.Map ActorHash ByteString)
                     -> IORef.IORef LamportTime
                     -> IORef.IORef [Effects.P2PNode]
                     -> Sem (Effects.AppEffects r) a
                     -> IO (Either AppError a)
interpretAppEffects resourceLog keyStore clockRef nodesRef =
    runM
    . runError
    . runTraceIO
    . interpretResourceOp resourceLog
    . interpretLogicalClock clockRef
    . interpretKeyManagement keyStore
    . interpretP2PNetwork nodesRef
    . interpretTransactionEffect
    . interpretEffectHandler
  where
    runTraceIO :: Sem (Trace ': r) a -> Sem r a
    runTraceIO = interpret $ \case
        Trace message -> embed $ putStrLn message

-- | Helper function to run a trace effect with configuration
runTraceWithConfig :: InterpreterConfig -> Sem (Trace ': r) a -> Sem r a
runTraceWithConfig config = interpret $ \case
    Trace message -> embed $ M.when (traceEnabled $ traceConfig config) $ 
        putStrLn $ T.unpack (tracePrefix $ traceConfig config) <> message

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
  }

-- | Verbose interpreter configuration
verboseConfig :: InterpreterConfig
verboseConfig = defaultConfig { icTraceLevel = Verbose }

-- | Silent interpreter configuration
silentConfig :: InterpreterConfig
silentConfig = defaultConfig { icTraceLevel = Silent }

-- | Interpret an effect with configuration
interpretWithConfig :: InterpreterConfig -> Effect -> IO (CoreEffect.EffectResult)
interpretWithConfig config effect = do
    -- Create necessary IORef instances
    resourceLog <- IORef.newIORef Map.empty
    keyStore <- IORef.newIORef Map.empty
    clockRef <- IORef.newIORef (LamportTime 0)
    nodesRef <- IORef.newIORef []
    
    -- Run the effect through the interpreter
    result <- interpretAppEffects resourceLog keyStore clockRef nodesRef $ do
        -- Implementation would go here
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

-- | Additional effect types that extend the core Effect type
data ExtendedEffect = 
    ResourceTransfer ResourceId ProgramId Integer
  | InternalStateUpdate ProgramId Text ByteString
  | AccountMessageEffect ActorId AccountMessage
  | EffectFailed Text
  deriving (Eq, Show, Generic)

 