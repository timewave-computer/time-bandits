{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- |
This module provides a composable effect system for the Time Bandits application.
It defines the core effect stack and provides utilities for interpreting and
working with effects in a type-safe manner.

The module is organized into sections:
1. Effect Definitions - The core effect types that define our effect system
2. Effect Interpreters - Functions that interpret effects into concrete implementations
   * All interpreters start with "interpret" by convention
   * Transform @Sem (Effect ': r) a@ into @Sem r a@
   * Use Polysemy's @interpret@ function
   * Handle specific effect constructors via pattern matching
3. Helper Functions - Supporting functions that use effects but don't interpret them
   * Regular functions that may use effects
   * Combine multiple effects or provide common patterns
   * Make code more readable and reusable
   * Handle low-level implementation details
-}
module TimeBandits.Effects (
    -- * Resource Operations
    ResourceOps (..),
    ResourceOperationEffect (..),
    interpretResourceOp,

    -- * Logical Clock
    LogicalClock (..),
    interpretLogicalClock,

    -- * Application Effects
    AppEffects,
    interpretAppEffects,

    -- * Modular Interpreter System
    TraceConfig (..),
    InterpreterConfig (..),
    defaultConfig,
    verboseConfig,
    silentConfig,
    interpretWithConfig,
) where

import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.IORef qualified as IORef
import Data.List (find, sortBy)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down(..), comparing)
import Data.Serialize (encode)
import Data.Time.Clock (getCurrentTime)
import Polysemy (Embed (..), Member, Members, Sem, embed, interpret, makeSem, send)
import Polysemy.Error (Error, throw)
import Polysemy.Output (Output)
import Polysemy.Trace (Trace (..), trace)
import TimeBandits.Core (ResourceHash, computeHash, computeMessageHash, computeSha256)
import TimeBandits.Events (
    Event (..),
    Message (..),
 )
import TimeBandits.Types (
    Actor (..),
    ActorErrorType (..),
    ActorHash,
    ActorType (..),
    AppError (..),
    AuthenticatedMessage (..),
    ContentAddressedMessage (..),
    CryptoErrorType (..),
    EntityHash (..),
    EventContent (..),
    EventMetadata (..),
    Hash (..),
    LamportTime (..),
    LogEntry (..),
    PrivKey (..),
    PubKey (..),
    Resource (..),
    ResourceErrorType (..),
    ResourceEvent (..),
    ResourceEventType (..),
    ResourceLog,
    Signature (..),
    StorageErrorType (..),
    TimelineErrorType (..),
    TimelineHash,
    TransactionValidationResult (..),
    TransientDatastore (..),
    UnifiedResourceTransaction (..),
    emTimestamp,
    resourceId,
 )
import TimeBandits.Types qualified as Types
import Prelude hiding (trace)
import TimeBandits.Network (
  P2PNetwork,
  P2PNode
 )
import Polysemy.State qualified as PS

-- | Logical clock for tracking causal ordering in timelines
data LogicalClock m a where
    GetLamportTime :: LogicalClock m LamportTime
    IncrementTime :: LogicalClock m LamportTime
    UpdateTime :: LamportTime -> LogicalClock m LamportTime

makeSem ''LogicalClock

-- | Atomic transaction effect for managing concurrent operations
data AtomicTransaction m a where
    BeginTransaction :: AtomicTransaction m ()
    CommitTransaction :: AtomicTransaction m ()
    RollbackTransaction :: AtomicTransaction m ()

-- | Timeout management for preventing deadlocks
data Timeout m a where
    ScheduleTimeout :: Int -> Timeout m ()
    CancelTimeout :: Int -> Timeout m ()

-- | Cryptographic proof generation and verification for timelines
data TimelineProof m a where
    GenerateProof :: Hash -> TimelineProof m Hash
    VerifyProof :: Hash -> Hash -> TimelineProof m Bool

-- | Timeline-specific messaging and communication
data TimelineMessage m a where
    SendMessage :: (Message msg) => msg -> TimelineMessage m ()
    BroadcastMessage :: (Message msg) => msg -> TimelineMessage m ()
    ReceiveMessage :: TimelineMessage m (AuthenticatedMessage ByteString)
    ConvertEventToMessage :: (Event e) => e -> PrivKey -> Maybe ActorHash -> TimelineMessage m (AuthenticatedMessage ByteString)
    ConvertMessageToEvent :: (Message msg) => msg -> TimelineMessage m (Maybe EventContent)

-- | Resource operations for managing timeline resources
class ResourceOps r where
    createResource :: ByteString -> ActorHash -> TimelineHash -> Sem r (Either AppError Resource)
    transferResource :: Resource -> ActorHash -> TimelineHash -> Sem r (Either AppError Resource)
    consumeResource :: Resource -> Sem r (Either AppError Resource)
    verifyResource :: Resource -> Sem r (Either AppError Bool)
    getResource :: ResourceHash -> Sem r (Either AppError Resource)
    getResourcesByOwner :: ActorHash -> Sem r (Either AppError [Resource])
    getResourcesByTimeline :: TimelineHash -> Sem r (Either AppError [Resource])
    createTransaction :: [Resource] -> [Resource] -> ActorHash -> TimelineHash -> Sem r (Either AppError UnifiedResourceTransaction)
    validateTransaction :: UnifiedResourceTransaction -> Sem r (Either AppError TransactionValidationResult)
    executeTransaction :: UnifiedResourceTransaction -> Sem r (Either AppError [Resource])
    transactionHistory :: ResourceHash -> Sem r (Either AppError [UnifiedResourceTransaction])

-- | Resource operations effect
data ResourceOperationEffect m a where
    OpCreateResource :: ByteString -> ActorHash -> TimelineHash -> ResourceOperationEffect m (Either AppError Resource)
    OpTransferResource :: Resource -> ActorHash -> TimelineHash -> ResourceOperationEffect m (Either AppError Resource)
    OpConsumeResource :: Resource -> ResourceOperationEffect m (Either AppError Resource)
    OpVerifyResource :: Resource -> ResourceOperationEffect m (Either AppError Bool)
    OpGetResource :: ResourceHash -> ResourceOperationEffect m (Either AppError Resource)
    OpGetResourcesByOwner :: ActorHash -> ResourceOperationEffect m (Either AppError [Resource])
    OpGetResourcesByTimeline :: TimelineHash -> ResourceOperationEffect m (Either AppError [Resource])
    OpCreateTransaction :: [Resource] -> [Resource] -> ActorHash -> TimelineHash -> ResourceOperationEffect m (Either AppError UnifiedResourceTransaction)
    OpValidateTransaction :: UnifiedResourceTransaction -> ResourceOperationEffect m (Either AppError TransactionValidationResult)
    OpExecuteTransaction :: UnifiedResourceTransaction -> ResourceOperationEffect m (Either AppError [Resource])
    OpTransactionHistory :: ResourceHash -> ResourceOperationEffect m (Either AppError [UnifiedResourceTransaction])

makeSem ''ResourceOperationEffect

instance (Member ResourceOperationEffect r) => ResourceOps r where
    createResource metadata owner timeline = opCreateResource metadata owner timeline
    transferResource resource actor timeline = opTransferResource resource actor timeline
    consumeResource resource = opConsumeResource resource
    verifyResource resource = opVerifyResource resource
    getResource hash = opGetResource hash
    getResourcesByOwner owner = opGetResourcesByOwner owner
    getResourcesByTimeline timeline = opGetResourcesByTimeline timeline
    createTransaction inputs outputs actor timeline = opCreateTransaction inputs outputs actor timeline
    validateTransaction transaction = opValidateTransaction transaction
    executeTransaction transaction = opExecuteTransaction transaction
    transactionHistory hash = opTransactionHistory hash

-- | Core application effects stack
type AppEffects r =
    '[ ResourceOperationEffect
     , P2PNetwork
     , PS.State ResourceLog
     , PS.State [P2PNode]
     , Trace
     , Output String
     , Error AppError
     , Embed IO
     ]

-- | Increment the Lamport time
incrementLamportTime :: LamportTime -> LamportTime
incrementLamportTime (LamportTime t) = LamportTime (t + 1)

-- | Interpret the LogicalClock effect
interpretLogicalClock :: (Member (Embed IO) r) => IORef.IORef LamportTime -> Sem (LogicalClock ': r) a -> Sem r a
interpretLogicalClock timeRef = interpret \case
    GetLamportTime -> embed @IO $ IORef.readIORef timeRef
    IncrementTime -> do
        t <- embed @IO $ IORef.readIORef timeRef
        let t' = incrementLamportTime t
        embed @IO $ IORef.writeIORef timeRef t'
        pure t
    UpdateTime t -> do
        embed @IO $ IORef.writeIORef timeRef t
        pure t

-- | Trace configuration options
data TraceConfig
    = -- | Disable all tracing
      NoTracing
    | -- | Enable standard tracing
      SimpleTracing
    | -- | Enable verbose tracing
      VerboseTracing
    deriving stock (Eq, Show)

-- | Interpreter configuration for controlling effect inclusion
data InterpreterConfig = InterpreterConfig
    { traceConfig :: TraceConfig
    -- ^ How to handle trace logs
    }

-- | Default interpreter configuration
defaultConfig :: InterpreterConfig
defaultConfig =
    InterpreterConfig
        { traceConfig = SimpleTracing
        }

-- | Verbose configuration with detailed logging
verboseConfig :: InterpreterConfig
verboseConfig =
    defaultConfig
        { traceConfig = VerboseTracing
        }

-- | Silent configuration with no tracing
silentConfig :: InterpreterConfig
silentConfig =
    defaultConfig
        { traceConfig = NoTracing
        }

-- | Apply a configuration to an interpreter chain
interpretWithConfig ::
    InterpreterConfig ->
    IORef.IORef LamportTime ->
    IORef.IORef ResourceLog ->
    IORef.IORef TransientDatastore ->
    IORef.IORef [TimelineHash] ->
    IO (Either AppError ([String], a))
interpretWithConfig = error "This function has been replaced by the version in Main.hs"

-- | Interpret all application effects with default configuration
interpretAppEffects ::
    IORef.IORef LamportTime ->
    IORef.IORef ResourceLog ->
    IORef.IORef TransientDatastore ->
    IORef.IORef [TimelineHash] ->
    Sem (LogicalClock ': AppEffects r) a ->
    IO (Either AppError ([String], a))
interpretAppEffects = error "This function has been replaced by the version in Main.hs"

-- | Interpret ResourceOperationEffect
interpretResourceOp :: (Members '[Trace, Error AppError, Embed IO, LogicalClock, PS.State ResourceLog, P2PNetwork] r) => ResourceOperationEffect m a -> Sem r a
interpretResourceOp = \case
    OpCreateResource metadata ownerHash timelineHash -> do
      trace "Creating resource..."
      -- Generate a resource ID from its content
      let resourceId = EntityHash $ computeSha256 $ encode (metadata, ownerHash, timelineHash)
      
      -- Update Lamport time for this operation using the LogicalClock effect
      -- This helps maintain causal ordering in the distributed system
      lt <- send IncrementTime
      
      -- Create the resource with proper timestamp
      let resource = Resource
            { resourceId = resourceId
            , resourceOrigin = timelineHash
            , resourceOwner = ownerHash
            , resourceCapabilities = [Types.TransferCapability, Types.UpdateCapability]
            , resourceMeta = metadata
            , resourceSpentBy = Nothing
            , resourceParents = []
            , resourceTimestamp = lt  -- Using the Lamport timestamp
            , resourceProvenanceChain = [timelineHash]
            }
      
      trace $ "Resource created: " <> show resourceId
      pure $ Right resource
      
    OpGetResource resourceHash -> do
        trace $ "Looking up resource: " <> show resourceHash
        
        -- Get the resource log
        resourceLog <- PS.get
        
        -- Find the most recent entry for this resource in the log
        let resourceEntries = filter (\entry -> 
                case leContent entry of
                    ResourceCreated res -> resourceId res == resourceHash
                    ResourceTransferred txn -> 
                        -- Check if resource is in outputs
                        any (\msg -> resourceId (camContent msg) == resourceHash) (urtOutputs txn)
                    _ -> False
                ) resourceLog
                
        -- Sort entries by timestamp (latest first) to get the most recent state
        let sortedEntries = sortBy (comparing (Down . emTimestamp . leMetadata)) resourceEntries
        
        -- Extract the resource from the latest entry
        case sortedEntries of
            [] -> pure $ Left $ ResourceError $ ResourceNotFound resourceHash
            (latest:_) -> 
                case leContent latest of
                    ResourceCreated res -> 
                        -- Check if the resource has been spent
                        if isNothing (resourceSpentBy res)
                        then pure $ Right res
                        else pure $ Left $ ResourceError $ ResourceAlreadySpent resourceHash
                    ResourceTransferred txn -> 
                        -- Find the resource in the transaction outputs
                        case find (\msg -> resourceId (camContent msg) == resourceHash) (urtOutputs txn) of
                            Just msg -> 
                                let res = camContent msg
                                in if isNothing (resourceSpentBy res)
                                   then pure $ Right res
                                   else pure $ Left $ ResourceError $ ResourceAlreadySpent resourceHash
                            Nothing -> pure $ Left $ ResourceError $ ResourceNotFound resourceHash
                    _ -> pure $ Left $ ResourceError $ ResourceNotFound resourceHash
    
    OpTransferResource resource newOwnerHash timelineHash -> do
        trace "Transferring resource..."
        -- Update Lamport time for this operation
        lt <- send IncrementTime
        
        let transferredResource =
                resource
                    { resourceOwner = newOwnerHash
                    , resourceOrigin = timelineHash
                    , resourceProvenanceChain = resourceProvenanceChain resource ++ [timelineHash]
                    , resourceTimestamp = lt  -- Using the Lamport timestamp
                    }
        trace "Resource transferred successfully."
        pure $ Right transferredResource
        
    OpConsumeResource resource -> do
        trace "Consuming resource..."
        -- Update Lamport time for this operation
        lt <- send IncrementTime
        
        let consumedResource = 
                resource
                    { resourceSpentBy = Just (computeHash resource Nothing)
                    , resourceTimestamp = lt  -- Using the Lamport timestamp
                    }
        trace "Resource consumed successfully."
        pure $ Right consumedResource
    
    OpVerifyResource resource -> do
        trace "Verifying resource..."
        pure $ Right $ isNothing (resourceSpentBy resource) && not (null (resourceCapabilities resource))
    
    OpGetResourcesByOwner ownerHash -> do
        trace $ "Looking up resources for owner: " <> show ownerHash
        
        -- Get the resource log
        resourceLog <- PS.get
        
        -- Filter resources by owner
        let ownerResources = filter (\entry -> 
                case leContent entry of
                    ResourceCreated res -> resourceOwner res == ownerHash
                    ResourceTransferred txn -> 
                        -- Check if any output resources are owned by this owner
                        any (\msg -> resourceOwner (camContent msg) == ownerHash) (urtOutputs txn)
                    _ -> False
                ) resourceLog
        
        -- Extract resources from the log entries
        let resources = mapMaybe (\entry -> 
                case leContent entry of
                    ResourceCreated res -> Just res
                    ResourceTransferred txn -> 
                        -- Find the first resource owned by this owner
                        listToMaybe $ filter (\res -> resourceOwner res == ownerHash) $ 
                            map camContent (urtOutputs txn)
                    _ -> Nothing
                ) ownerResources
        
        trace $ "Found " <> show (length resources) <> " resources for owner " <> show ownerHash
        pure $ Right resources
    
    OpGetResourcesByTimeline timelineHash -> do
        trace $ "Looking up resources for timeline: " <> show timelineHash
        
        -- Get the resource log
        resourceLog <- PS.get
        
        -- Filter resources by timeline
        let timelineResources = filter (\entry -> 
                case leContent entry of
                    ResourceCreated res -> resourceOrigin res == timelineHash || 
                                          timelineHash `elem` resourceProvenanceChain res
                    ResourceTransferred txn -> 
                        -- Check if any output resources are in this timeline
                        any (\msg -> timelineHash `elem` resourceProvenanceChain (camContent msg)) (urtOutputs txn)
                    _ -> False
                ) resourceLog
        
        -- Extract resources from the log entries
        let resources = mapMaybe (\entry -> 
                case leContent entry of
                    ResourceCreated res -> 
                        if resourceOrigin res == timelineHash || timelineHash `elem` resourceProvenanceChain res
                        then Just res
                        else Nothing
                    ResourceTransferred txn -> 
                        -- Find the first resource in this timeline
                        listToMaybe $ filter (\res -> timelineHash `elem` resourceProvenanceChain res) $ 
                            map camContent (urtOutputs txn)
                    _ -> Nothing
                ) timelineResources
        
        trace $ "Found " <> show (length resources) <> " resources for timeline " <> show timelineHash
        pure $ Right resources
    
    OpCreateTransaction inputs outputs actor timelineHash -> do
        trace "Creating transaction..."
        -- Update Lamport time for this operation
        lt <- send IncrementTime
        
        -- Create a proper Actor from the ActorHash
        -- In a real implementation, this would be retrieved from an actor database/registry
        let actorType = determineActorType actor
        let signingActor = Actor
                { actorId = actor
                , actorType = actorType
                }
        
        -- Generate a proper signature for the transaction
        let txnData = encode (inputs, outputs, lt, timelineHash)
        let signatureBytes = generateSignatureForActor actor txnData
        
        let transaction = UnifiedResourceTransaction
                { urtInputs = map createAuthenticatedMessage inputs
                , urtOutputs = map createContentAddressedMessage outputs
                , urtMetadata = ""
                , urtTimestamp = lt  -- Using the Lamport timestamp
                , urtSigner = signingActor
                , urtSignature = Signature signatureBytes  -- Using generated signature
                , urtProvenanceChain = [timelineHash]
                }
        trace "Transaction created successfully."
        pure $ Right transaction
    
    OpValidateTransaction transaction -> do
        trace "Validating transaction..."
        let result =
                if all (isNothing . resourceSpentBy . camContent . amPayload) (urtInputs transaction) && not (null (urtOutputs transaction))
                    then TransactionValid
                    else TransactionInvalid "Invalid inputs or outputs"
        pure $ Right result
    
    OpExecuteTransaction transaction -> do
        trace "Executing transaction..."
        -- Not using spentResources since it's unused
        -- let spentResources = map (markAsSpent . camContent . amPayload) (urtInputs transaction)
        let newResources = map camContent (urtOutputs transaction)
        trace "Transaction executed successfully."
        pure $ Right newResources
    
    OpTransactionHistory resourceHash -> do
        trace $ "Looking up transaction history for resource: " <> show resourceHash
        
        -- Get the resource log
        resourceLog <- PS.get
        
        -- Filter transactions involving this resource
        let resourceTransactions = filter (\entry -> 
                case leContent entry of
                    ResourceTransferred txn -> 
                        -- Check if resource is in inputs or outputs
                        any (\msg -> resourceId (camContent (amPayload msg)) == resourceHash) (urtInputs txn) ||
                        any (\msg -> resourceId (camContent msg) == resourceHash) (urtOutputs txn)
                    _ -> False
                ) resourceLog
        
        -- Extract transactions from the log entries
        let transactions = mapMaybe (\entry -> 
                case leContent entry of
                    ResourceTransferred txn -> Just txn
                    _ -> Nothing
                ) resourceTransactions
        
        -- Sort transactions by timestamp
        let sortedTransactions = sortBy (comparing (\txn -> urtTimestamp txn)) transactions
        
        trace $ "Found " <> show (length sortedTransactions) <> " transactions for resource " <> show resourceHash
        pure $ Right sortedTransactions

-- Helper functions
createAuthenticatedMessage :: Resource -> AuthenticatedMessage Resource
createAuthenticatedMessage resource = 
    let messageHash = computeMessageHash $ encode resource
        content = createContentAddressedMessage resource
        -- Generate a deterministic actor ID based on the resource owner
        actorId = resourceOwner resource
        -- Create a proper actor instance
        actor = Actor actorId $ determineActorType actorId
        -- Generate a signature using the resource data
        signatureBytes = generateSignatureForActor actorId (encode resource)
    in AuthenticatedMessage messageHash actor Nothing content (Signature signatureBytes)

createContentAddressedMessage :: Resource -> ContentAddressedMessage Resource
createContentAddressedMessage resource = ContentAddressedMessage (computeMessageHash $ encode resource) resource

markAsSpent :: Resource -> Resource
markAsSpent resource = resource{resourceSpentBy = Just (computeHash resource Nothing)}

-- | Determine actor type based on actor hash
-- In a real implementation, this would query an actor registry
determineActorType :: ActorHash -> ActorType
determineActorType actorHash = 
    -- Simple demonstration of actor type determination
    -- In a real implementation, this would involve a proper lookup
    let actorIdStr = BS.pack (show actorHash)
    in if "val_" `BS.isPrefixOf` actorIdStr
       then Types.Validator
       else Types.TimeTraveler  -- Using TimeTraveler as default instead of User

-- | Generate a signature for the given actor and data
-- In a real implementation, this would use the actor's private key
generateSignatureForActor :: ActorHash -> ByteString -> ByteString
generateSignatureForActor actorHash dataToSign = 
    -- Create a deterministic "signature" based on the actor and data
    -- This is NOT a real cryptographic signature - just for demonstration
    let combinedData = BS.pack (show actorHash) <> dataToSign
        Hash hashBytes = computeSha256 combinedData
    in "sig-" <> hashBytes
