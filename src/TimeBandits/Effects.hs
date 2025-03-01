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

import Control.Monad (forM, forM_, when)
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash.SHA256 qualified as SHA256
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.IORef qualified as IORef
import Data.List (nub, sortBy, union)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Serialize (decode, encode)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (getCurrentTime)
import Polysemy (Embed (..), Member, Members, Sem, embed, interpret, interpretH, makeSem, runM, send)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Output (Output, output, runOutputList)
import Polysemy.Reader
import Polysemy.State
import Polysemy.Trace (Trace (..), ignoreTrace, trace, traceToStdout)
import TimeBandits.Core (ResourceHash, computeHash, computeMessageHash, computePubKeyHash, computeSha256)
import TimeBandits.Events (
    Event (..),
    Message (..),
    createLogEntry,
    verifyEventSignature,
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
    TimelineBlock (..),
    TimelineErrorType (..),
    TimelineEventType (..),
    TimelineHash,
    TimelineLog (..),
    TransactionValidationResult (..),
    TransientDatastore (..),
    TransientStoredItem (..),
    Trie (..),
    UnifiedResourceTransaction (..),
    elemsTrie,
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

-- | Get the public key from an actor
getActorPubKey :: Actor -> PubKey
getActorPubKey actor = PubKey $ BS.pack $ show $ actorId actor -- This is a placeholder implementation

-- | Logical clock for tracking causal ordering in timelines
data LogicalClock m a where
    GetCurrentTime :: LogicalClock m LamportTime
    IncrementTime :: LogicalClock m LamportTime
    UpdateTime :: LamportTime -> LogicalClock m LamportTime

-- | Error conversion helper functions
asTimelineError :: (Members '[Error AppError] r) => TimelineErrorType -> Sem r a
asTimelineError = throw . TimelineError

asResourceError :: (Members '[Error AppError] r) => ResourceErrorType -> Sem r a
asResourceError = throw . ResourceError

asActorError :: (Members '[Error AppError] r) => ActorErrorType -> Sem r a
asActorError = throw . ActorError

asCryptoError :: (Members '[Error AppError] r) => CryptoErrorType -> Sem r a
asCryptoError = throw . CryptoError

asStorageError :: (Members '[Error AppError] r) => StorageErrorType -> Sem r a
asStorageError = throw . StorageError

{- | Helper function to sign event content
This function signs event content with a private key and returns the signature and public key.
It handles the cryptographic operations needed for event authentication.
-}
signEventContent ::
    (Members '[Error AppError] r) =>
    PrivKey ->
    ByteString ->
    Sem r (Signature, PubKey)
signEventContent privKey content = do
    -- Use Types.signMessage directly instead of going through the CryptoOperation effect
    case Types.signMessage privKey content of
        Left err -> throw $ CryptoError $ SigningError err
        Right signature -> do
            -- Generate the corresponding public key
            let pubKey = case Ed25519.secretKey (let PrivKey bytes = privKey in bytes) of
                    CryptoFailed _ -> PubKey "invalid-key" -- Fallback for invalid keys
                    CryptoPassed sk -> PubKey $ convert $ Ed25519.toPublic sk
            pure (signature, pubKey)

-- | Resource transfer operation
transferResourceOp ::
    (Members '[Error AppError, LogicalClock] r) =>
    Resource ->
    ActorHash ->
    TimelineHash ->
    LamportTime ->
    [TimelineHash] ->
    PrivKey ->
    Sem r (Either AppError (Resource, ResourceEvent))
transferResourceOp resource newOwner destTimeline timestamp provChain privKey = do
    let updatedResource = resource{resourceOwner = newOwner}
        eventContent = ResourceCreated updatedResource

    -- Sign the event content
    (signature, pubKey) <- signEventContent privKey (encode eventContent)

    -- Create the event with proper signature
    let event =
            ResourceEvent
                { reContent = eventContent
                , reMetadata =
                    EventMetadata
                        { emTimestamp = timestamp
                        , emCreatedAt = undefined -- TODO: Fix timestamp type
                        , emActor = newOwner
                        , emTimeline = destTimeline
                        , emSignature = signature
                        , emSigner = pubKey
                        }
                , rePreviousEvent = Nothing -- TODO: Track previous events
                }
    pure $ Right (updatedResource, event)

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
    GetCurrentTime -> embed @IO $ IORef.readIORef timeRef
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
    deriving (Eq, Show)

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

-- | Custom verbose trace interpreter that adds timestamps and context
traceVerbose :: (Member (Embed IO) r) => Sem (Trace ': r) a -> Sem r a
traceVerbose = interpret \case
    Trace message -> do
        timestamp <- embed @IO getCurrentTime
        embed @IO $ putStrLn $ "[VERBOSE][" ++ show timestamp ++ "] " ++ message

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

-- | Interpret the timeline resource effect
interpretResourceOp :: (Members '[Trace, Error AppError, Embed IO, PS.State ResourceLog, P2PNetwork] r) => Sem (ResourceOperationEffect ': r) a -> Sem r a
interpretResourceOp = interpret \case
    OpCreateResource metadata ownerHash timelineHash -> do
        trace "Creating resource..."
        let resourceId = EntityHash $ computeSha256 $ encode (metadata, ownerHash, timelineHash)
            resource =
                Resource
                    { resourceId = resourceId
                    , resourceOrigin = timelineHash
                    , resourceOwner = ownerHash
                    , resourceCapabilities = [Types.TransferCapability, Types.UpdateCapability]
                    , resourceMeta = metadata
                    , resourceSpentBy = Nothing
                    , resourceParents = []
                    , resourceTimestamp = LamportTime 0 -- TODO: Get proper timestamp
                    , resourceProvenanceChain = [timelineHash]
                    }
                    
        -- Use P2P network to select nodes for storing this resource
        trace "Selecting nodes for resource storage using Rendezvous Hashing..."
        let resourceKey = encode resourceId
        -- In a real implementation, we would use the P2P network to select and store the resource
        -- For now, we just simulate selection
        -- selectedNodes <- selectNodesForKey resourceKey 3
        
        trace "Selected nodes for resource storage"
        
        -- In a real implementation, we would store the resource on these nodes
        -- For now, we just log the selected nodes
        
        trace "Resource created successfully."
        pure $ Right resource
        
    OpGetResource resourceHash -> do
        trace "Getting resource..."
        -- Use P2P network to locate the resource
        let resourceKey = encode resourceHash
        -- selectedNodes <- selectNodesForKey resourceKey 3
        
        -- If we found nodes to query
        trace "Resource lookup through P2P network would happen here"
        -- In a real implementation, we would query these nodes for the resource
        -- For now, we just simulate a failure
        pure $ Left $ ResourceError $ ResourceNotFound resourceHash
        
    -- Keep other handlers unchanged for brevity
    handler -> interpretResourceOp' handler

-- Helper to handle the remaining cases
interpretResourceOp' :: (Members '[Trace, Error AppError, Embed IO, PS.State ResourceLog, P2PNetwork] r) => ResourceOperationEffect m a -> Sem r a
interpretResourceOp' = \case
    OpTransferResource resource newOwnerHash timelineHash -> do
        trace "Transferring resource..."
        let transferredResource =
                resource
                    { resourceOwner = newOwnerHash
                    , resourceOrigin = timelineHash
                    , resourceProvenanceChain = resourceProvenanceChain resource ++ [timelineHash]
                    , resourceTimestamp = LamportTime 0 -- TODO: Get proper timestamp
                    }
        trace "Resource transferred successfully."
        pure $ Right transferredResource
    OpConsumeResource resource -> do
        trace "Consuming resource..."
        let consumedResource = resource{resourceSpentBy = Just (computeHash resource Nothing), resourceTimestamp = LamportTime 0} -- TODO: Get proper timestamp
        trace "Resource consumed successfully."
        pure $ Right consumedResource
    OpVerifyResource resource -> do
        trace "Verifying resource..."
        pure $ Right $ isNothing (resourceSpentBy resource) && not (null (resourceCapabilities resource))
    OpGetResourcesByOwner ownerHash -> do
        trace "Getting resources by owner..."
        -- TODO: Implement actual resource lookup by owner
        pure $ Right []
    OpGetResourcesByTimeline timelineHash -> do
        trace "Getting resources by timeline..."
        -- TODO: Implement actual resource lookup by timeline
        pure $ Right []
    OpCreateTransaction inputs outputs actor timelineHash -> do
        trace "Creating transaction..."
        let transaction =
                UnifiedResourceTransaction
                    { urtInputs = map createAuthenticatedMessage inputs
                    , urtOutputs = map createContentAddressedMessage outputs
                    , urtMetadata = "Transaction metadata"
                    , urtTimestamp = LamportTime 0 -- TODO: Get proper timestamp
                    , urtSigner = undefined -- TODO: Create proper actor
                    , urtSignature = Signature "" -- TODO: Add proper signature
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
        let spentResources = map (markAsSpent . camContent . amPayload) (urtInputs transaction)
        let newResources = map camContent (urtOutputs transaction)
        trace "Transaction executed successfully."
        pure $ Right newResources
    OpTransactionHistory resourceHash -> do
        trace "Getting transaction history..."
        -- TODO: Implement actual transaction history lookup
        pure $ Right []

-- Helper functions
createAuthenticatedMessage :: Resource -> AuthenticatedMessage Resource
createAuthenticatedMessage resource = AuthenticatedMessage (computeMessageHash $ encode resource) undefined undefined (createContentAddressedMessage resource) (Signature "")

createContentAddressedMessage :: Resource -> ContentAddressedMessage Resource
createContentAddressedMessage resource = ContentAddressedMessage (computeMessageHash $ encode resource) resource

markAsSpent :: Resource -> Resource
markAsSpent resource = resource{resourceSpentBy = Just (computeHash resource Nothing)}

-- | Helper function to update a log entry with spent resources
updateLogEntry :: [Resource] -> LogEntry ResourceEventType -> LogEntry ResourceEventType
updateLogEntry spentResources entry =
    case leContent entry of
        ResourceCreated resource ->
            if any (\spent -> resourceId spent == resourceId resource) spentResources
                then entry{leContent = ResourceCreated $ markAsSpent resource}
                else entry
        ResourceTransferred transaction ->
            let updatedInputs = map (\msg -> msg{amPayload = (amPayload msg){camContent = markAsSpent (camContent (amPayload msg))}}) (urtInputs transaction)
             in entry{leContent = ResourceTransferred $ transaction{urtInputs = updatedInputs}}
        _ -> entry

-- | Helper function to check if a resource is involved in a transaction
isInvolvedInTransaction :: ResourceHash -> LogEntry ResourceEventType -> Bool
isInvolvedInTransaction resourceHash entry =
    case leContent entry of
        ResourceCreated resource -> resourceId resource == resourceHash
        ResourceTransferred transaction ->
            any (\msg -> resourceId (camContent (amPayload msg)) == resourceHash) (urtInputs transaction)
                || any (\msg -> resourceId (camContent msg) == resourceHash) (urtOutputs transaction)
        _ -> False
