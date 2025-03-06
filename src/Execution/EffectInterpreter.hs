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
import Data.Map.Strict qualified as Map
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
import Polysemy.Trace (Trace, trace)
import System.Random qualified as Random
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Hashable (Hashable)
import Control.Concurrent.STM (STM, atomically, TVar, readTVar, writeTVar, readTVarIO)
import Control.Concurrent.STM.TVar (newTVar)
import Control.Monad (forM_, when)

-- Import from Core modules
import Core.Common (Hash(..), Signature(..))
import Core.Utils (computeContentHash, computeSha256)
import Core.Effect (Effect(..), EffectResult(..), EffectStatus(..), EffectId)
import Core.Effects
    ( ResourceOps(..)
    , ResourceOperationEffect(..)
    , LogicalClock(..)
    , KeyManagement(..)
    , P2PNetwork(..)
    , P2PNode(..)
    , TransactionEffect(..)
    , AppEffects
    , ResourceCapability(..)
    , Resource(..)
    , Actor(..)
    , ActorType(..)
    , ContentAddressedMessage(..)
    , AuthenticatedMessage(..)
    , UnifiedResourceTransaction(..)
    , TransactionValidationResult(..)
    , LamportTime(..)
    )
import Core.ResourceId (ResourceId(..))
import Core.TimelineId (TimelineId(..))
import Core.ProgramId (ProgramId(..))
import Core.TimeMap (TimeMap)
import Core.ResourceLedger (ResourceLedger, ResourceState)
import Core.ActorId (ActorId)
import Core.AccountProgram (AccountProgram)

import Execution.ExecutionLog (ExecutionLog, ExecutionLogEntry, createExecutionLog, recordEffect)
import Execution.PreconditionEvaluator (PreconditionEvaluator, PreconditionResult(..), ProposedEffect(..))
import Execution.EffectLogger (EffectLogger, LogResult(..))

-- | Result of interpreting an effect
data EffectResult =
    EffectApplied Effect BS.ByteString  -- ^ Effect and resulting state hash
  | EffectRejected EffectError
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

-- | Interpret a proposed effect
interpretEffect :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffect -> 
  m EffectResult
interpretEffect interpreter proposedEffect = liftIO $ do
  -- 1. Lock resources in the write set
  lockResult <- atomically $ lockResources interpreter (writeSet proposedEffect) (effect proposedEffect)
  case lockResult of
    Left err -> pure $ EffectRejected err
    Right _ -> do
      -- 2. Get current context
      context <- readTVarIO (executionContext interpreter)
      
      -- 3. Validate preconditions
      ledger <- readTVarIO (resourceLedger interpreter)
      preconditionResult <- liftIO $ evaluatePreconditions 
        (preconditionEvaluator interpreter) 
        proposedEffect 
        ledger 
        (currentTimeMap context)
      
      case preconditionResult of
        PreconditionsNotSatisfied errs -> do
          -- Release locks if preconditions fail
          atomically $ releaseResources interpreter (writeSet proposedEffect)
          pure $ EffectRejected $ PreconditionFailed $ T.pack $ show errs
          
        PreconditionsSatisfied -> do
          -- 4. Apply the effect
          applyResult <- applyEffect interpreter proposedEffect
          
          -- 5. Release locks regardless of application result
          atomically $ releaseResources interpreter (writeSet proposedEffect)
          
          -- 6. Return the result
          pure applyResult

-- | Lock resources for an effect
lockResources :: 
  EffectInterpreter -> 
  [ResourceId] -> 
  Effect -> 
  STM (Either EffectError ())
lockResources interpreter resources effect = do
  -- Get current lock table
  locks <- readTVar (lockTable interpreter)
  
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
          newLocks = foldr (\r acc -> Map.insert r (LockedBy effectId) acc) locks resources
      
      -- Update lock table
      writeTVar (lockTable interpreter) newLocks
      pure $ Right ()

-- | Release resources after effect application
releaseResources :: 
  EffectInterpreter -> 
  [ResourceId] -> 
  STM ()
releaseResources interpreter resources = do
  -- Get current lock table
  locks <- readTVar (lockTable interpreter)
  
  -- Release all locks
  let newLocks = foldr (\r acc -> Map.insert r Unlocked acc) locks resources
  
  -- Update lock table
  writeTVar (lockTable interpreter) newLocks

-- | Apply an effect to the system
applyEffect :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffect -> 
  m EffectResult
applyEffect interpreter proposedEffect = liftIO $ do
  -- Get current context and ledger
  context <- readTVarIO (executionContext interpreter)
  ledger <- readTVarIO (resourceLedger interpreter)
  
  -- Apply the effect based on its type
  case effect proposedEffect of
    ResourceTransfer{} ->
      applyResourceTransfer interpreter proposedEffect ledger
      
    InternalStateUpdate{} ->
      applyInternalStateUpdate interpreter proposedEffect context
      
    AccountMessageEffect{} ->
      applyAccountMessage interpreter proposedEffect context
      
    _ ->
      -- For other effect types, we would have specific handlers
      pure $ EffectRejected $ InternalStateError "Unsupported effect type"

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
  pure $ EffectApplied (effect proposedEffect) BS.empty

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
  pure $ EffectApplied (effect proposedEffect) BS.empty

-- | Apply an account message effect
applyAccountMessage :: 
  (MonadIO m) => 
  EffectInterpreter -> 
  ProposedEffect -> 
  EffectContext -> 
  m EffectResult
applyAccountMessage interpreter proposedEffect context = do
  case effect proposedEffect of
    AccountMessageEffect actorId message -> do
      -- Get the account program for this actor
      let maybeAccountProgram = findAccountProgram actorId context
      
      case maybeAccountProgram of
        Nothing -> 
          pure $ EffectFailed $ "Account program not found for actor: " <> show actorId
        
        Just accountProgram -> do
          -- Process the message based on its type
          result <- case message of
            DepositMessage resource amount to -> do
              -- Check if the account has sufficient balance
              let currentBalance = Map.findWithDefault 0 resource (balances accountProgram)
              if currentBalance >= amount
                then do
                  -- Update the account program's balances
                  let updatedBalances = Map.adjust (\b -> b - amount) resource (balances accountProgram)
                  let updatedAccount = accountProgram { balances = updatedBalances }
                  
                  -- Update the resource ledger to reflect the transfer
                  ledger <- readTVarIO (resourceLedger interpreter)
                  let updatedLedger = updateResourceOwner resource to amount ledger
                  
                  -- Record the message in the outbox
                  let outboxMsg = createSentMessage "deposit" to (show resource <> ":" <> show amount)
                  let finalAccount = updatedAccount { outbox = outboxMsg : outbox updatedAccount }
                  
                  -- Update the account program in the context
                  updateAccountProgram finalAccount context
                  
                  -- Update the resource ledger
                  atomically $ writeTVar (resourceLedger interpreter) updatedLedger
                  
                  pure $ EffectApplied (effect proposedEffect) (encode finalAccount)
                else
                  pure $ EffectFailed $ "Insufficient balance for resource: " <> show resource
            
            WithdrawMessage resource amount from -> do
              -- Check if the target program owns the resource
              ledger <- readTVarIO (resourceLedger interpreter)
              let ownership = getResourceOwner resource ledger
              
              case ownership of
                Just (owner, _) | owner == from -> do
                  -- Update the resource ledger
                  let updatedLedger = transferResource resource from (owner accountProgram) amount ledger
                  
                  -- Update the account program's balances
                  let updatedBalances = Map.insertWith (+) resource amount (balances accountProgram)
                  let updatedAccount = accountProgram { balances = updatedBalances }
                  
                  -- Record the message in the outbox
                  let outboxMsg = createSentMessage "withdraw" from (show resource <> ":" <> show amount)
                  let finalAccount = updatedAccount { outbox = outboxMsg : outbox updatedAccount }
                  
                  -- Update the account program in the context
                  updateAccountProgram finalAccount context
                  
                  -- Update the resource ledger
                  atomically $ writeTVar (resourceLedger interpreter) updatedLedger
                  
                  pure $ EffectApplied (effect proposedEffect) (encode finalAccount)
                
                _ -> pure $ EffectFailed $ "Resource not owned by target program: " <> show resource
            
            InvokeMessage targetProgram entrypoint args -> do
              -- Create a message in the outbox
              let payload = "entrypoint:" <> entrypoint <> ",args:" <> T.intercalate "," args
              let outboxMsg = createSentMessage "invoke" targetProgram payload
              let updatedAccount = accountProgram { outbox = outboxMsg : outbox accountProgram }
              
              -- Update the account program in the context
              updateAccountProgram updatedAccount context
              
              pure $ EffectApplied (effect proposedEffect) (encode updatedAccount)
            
            ReceiveCallbackMessage sourceProgram payload -> do
              -- Create a message in the inbox
              let inboxMsg = createReceivedMessage "callback" sourceProgram payload
              let updatedAccount = accountProgram { inbox = inboxMsg : inbox accountProgram }
              
              -- Update the account program in the context
              updateAccountProgram updatedAccount context
              
              pure $ EffectApplied (effect proposedEffect) (encode updatedAccount)
            
            CustomAccountMessage messageType payload -> do
              -- Handle custom messages based on type
              -- This is a placeholder for extensibility
              let inboxMsg = createReceivedMessage (T.unpack messageType) (owner accountProgram) payload
              let updatedAccount = accountProgram { inbox = inboxMsg : inbox accountProgram }
              
              -- Update the account program in the context
              updateAccountProgram updatedAccount context
              
              pure $ EffectApplied (effect proposedEffect) (encode updatedAccount)
          
          -- Log the effect
          logEffect interpreter proposedEffect result
          
          pure result
    
    _ -> pure $ EffectFailed "Not an account message effect"

-- | Helper function to find an account program by actor ID
findAccountProgram :: ActorId -> EffectContext -> Maybe AccountProgram
findAccountProgram _ _ = Nothing  -- Placeholder, would look up in a real implementation

-- | Helper function to update an account program in the context
updateAccountProgram :: AccountProgram -> EffectContext -> IO ()
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
transferResource :: ResourceId -> ProgramId -> ProgramId -> Integer -> ResourceLedger -> ResourceLedger
transferResource resource from to amount ledger =
  case Map.lookup resource ledger of
    Just (owner, ResourceState currentAmount) | owner == from && currentAmount >= amount ->
      let remainingAmount = currentAmount - amount
      in if remainingAmount > 0
         then Map.insert resource (from, ResourceState remainingAmount) ledger
         else Map.delete resource ledger
    _ -> ledger  -- No change if conditions not met

-- | Log an effect (placeholder)
logEffect :: (MonadIO m) => EffectInterpreter -> ProposedEffect -> EffectResult -> m ()
logEffect _ _ _ = pure ()  -- Placeholder, would log in a real implementation

-- | Get the effect ID from an effect
getEffectId :: Effect -> EffectId
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
interpretResourceOp :: (Member (Embed IO) r, Member Trace r) 
                   -> IORef.IORef ResourceLog 
                   -> Sem (ResourceOperationEffect ': r) a 
                   -> Sem r a
interpretResourceOp resourceLog = interpret $ \case
    OpCreateResource metadata ownerHash timelineId -> do
        trace "Creating resource..."
        -- Implementation would go here
        pure $ Right undefined

    OpTransferResource resource newOwnerHash timelineId -> do
        trace "Transferring resource..."
        -- Implementation would go here
        pure $ Right undefined
        
    OpConsumeResource resource -> do
        trace "Consuming resource..."
        -- Implementation would go here
        pure $ Right undefined
    
    OpVerifyResource resource -> do
        trace "Verifying resource..."
        -- Implementation would go here
        pure $ Right undefined
        
    OpGetResource hash -> do
        trace "Getting resource by hash..."
        -- Implementation would go here
        pure $ Right undefined
        
    OpGetResourcesByOwner ownerHash -> do
        trace "Getting resources by owner..."
        -- Implementation would go here
        pure $ Right undefined
        
    OpGetResourcesByTimeline timelineId -> do
        trace "Getting resources by timeline..."
        -- Implementation would go here
        pure $ Right undefined
        
    OpCreateTransaction inputs outputs actorHash timelineId -> do
        trace "Creating transaction..."
        -- Implementation would go here
        pure $ Right undefined
        
    OpValidateTransaction transaction -> do
        trace "Validating transaction..."
        -- Implementation would go here
        pure $ Right undefined
        
    OpExecuteTransaction transaction -> do
        trace "Executing transaction..."
        -- Implementation would go here
        pure $ Right undefined
        
    OpTransactionHistory hash -> do
        trace "Getting transaction history..."
        -- Implementation would go here
        pure $ Right undefined

-- | Interpret the LogicalClock effect
interpretLogicalClock :: (Member (Embed IO) r) 
                      -> IORef.IORef LamportTime 
                      -> Sem (LogicalClock ': r) a 
                      -> Sem r a
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
        when (new > current) $
            IORef.writeIORef clockRef newTime
        pure newTime

-- | Interpret the KeyManagement effect
interpretKeyManagement :: (Member (Embed IO) r, Member Trace r) 
                       -> IORef.IORef (Map.Map ActorHash ByteString) 
                       -> Sem (KeyManagement ': r) a 
                       -> Sem r a
interpretKeyManagement keyStore = interpret $ \case
    GenerateKeyPair -> embed $ do
        -- This is a placeholder; in a real implementation we'd use proper crypto
        priv <- Random.randomIO :: IO ByteString
        let pub = SHA256.hash priv
        pure (priv, pub)
    
    RegisterPublicKey actorHash pubKey -> embed $ do
        keyMap <- IORef.readIORef keyStore
        IORef.writeIORef keyStore (Map.insert actorHash pubKey keyMap)
    
    LookupPublicKey actorHash -> embed $ do
        keyMap <- IORef.readIORef keyStore
        pure $ Map.lookup actorHash keyMap
    
    SignData privateKey message -> do
        trace "Signing data..."
        -- This is a placeholder; in a real implementation we'd use proper crypto
        pure $ Just $ Signature $ SHA256.hash (privateKey <> message)
    
    VerifyWithPublicKey publicKey message signature -> do
        trace "Verifying signature..."
        -- This is a placeholder; in a real implementation we'd use proper crypto
        let expectedSig = SHA256.hash (publicKey <> message)
        pure $ expectedSig == convert (unSignature signature)
    
    RegisterActorType actorHash actorType -> do
        trace $ "Registering actor type: " <> show actorType
        -- Implementation would go here
        pure ()
    
    LookupActorType actorHash -> do
        trace "Looking up actor type..."
        -- Implementation would go here
        pure $ Just TimeKeeperType

-- | Interpret the P2PNetwork effect
interpretP2PNetwork :: (Member (Embed IO) r, Member Trace r) 
                    -> IORef.IORef [P2PNode] 
                    -> Sem (P2PNetwork ': r) a 
                    -> Sem r a
interpretP2PNetwork nodesRef = interpret $ \case
    DiscoverNodes -> embed $ do
        nodes <- IORef.readIORef nodesRef
        pure nodes
    
    ConnectToNode node -> do
        trace $ "Connecting to node: " <> show (nodeId node)
        embed $ do
            nodes <- IORef.readIORef nodesRef
            -- Check if node is already connected
            if any (\n -> nodeId n == nodeId node) nodes
                then pure False
                else do
                    IORef.modifyIORef nodesRef (node:)
                    pure True
    
    DisconnectFromNode nodeId -> do
        trace $ "Disconnecting from node: " <> show nodeId
        embed $ IORef.modifyIORef nodesRef (filter (\n -> nodeId /= nodeId n))
    
    SendMessage nodeId message -> do
        trace $ "Sending message to node: " <> show nodeId
        -- This would actually send a message in a real implementation
        pure True
    
    BroadcastMessage message -> do
        trace "Broadcasting message to all nodes"
        embed $ do
            nodes <- IORef.readIORef nodesRef
            -- This would actually broadcast in a real implementation
            pure $ length nodes
    
    ReceiveMessage -> do
        trace "Waiting for message..."
        -- This would actually receive a message in a real implementation
        pure Nothing

-- | Interpret the TransactionEffect
interpretTransactionEffect :: (Member (Embed IO) r, Member Trace r) 
                           -> Sem (TransactionEffect ': r) a 
                           -> Sem r a
interpretTransactionEffect = interpret $ \case
    BeginTransaction -> do
        trace "Beginning transaction..."
        -- Generate a transaction ID
        txId <- embed $ do
            randomBytes <- Random.randoms :: IO [Word8]
            pure $ ByteString.pack $ take 32 randomBytes
        pure txId
    
    CommitTransaction txId -> do
        trace $ "Committing transaction: " <> show txId
        -- This would actually commit the transaction in a real implementation
        pure True
    
    RollbackTransaction txId -> do
        trace $ "Rolling back transaction: " <> show txId
        -- This would actually roll back the transaction in a real implementation
        pure ()

-- | Interpret all application effects
interpretAppEffects :: InterpreterConfig
                    -> IORef.IORef ResourceLog
                    -> IORef.IORef (Map.Map ActorHash ByteString)
                    -> IORef.IORef LamportTime
                    -> IORef.IORef [P2PNode]
                    -> Sem (AppEffects ++ '[Trace, Error AppError, Embed IO]) a
                    -> IO (Either AppError a)
interpretAppEffects config resourceLog keyStore clockRef nodesRef =
    runM . runError . interpretTrace . 
    interpretTransactionEffect .
    interpretLogicalClock clockRef .
    interpretKeyManagement keyStore .
    interpretP2PNetwork nodesRef .
    interpretResourceOp resourceLog
  where
    interpretTrace = interpret $ \case
        Trace message -> liftIO $ when (traceEnabled $ traceConfig config) $ 
            putStrLn $ Text.unpack (tracePrefix $ traceConfig config) <> message

-- Helper function for when condition
when :: Applicative f => Bool -> f () -> f ()
when True f = f
when False _ = pure ()

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

-- | Interpret an effect with a given configuration
interpretWithConfig :: InterpreterConfig -> Effect a -> IO (EffectResult a)
interpretWithConfig config effect = do
  interpreter <- createInterpreter config
  interpretEffect interpreter effect

-- | Causal enforcement
enforceResourceOwnership :: EffectInterpreter -> ResourceId -> Bool
enforceResourceOwnership _ _ = True  -- Placeholder

enforceCausalOrder :: EffectInterpreter -> Bool
enforceCausalOrder _ = True  -- Placeholder

enforceTimeMapConsistency :: EffectInterpreter -> Bool
enforceTimeMapConsistency _ = True  -- Placeholder

 