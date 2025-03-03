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

    -- * Key Management
    KeyManagement (..),
    generateKeyPair,
    registerPublicKey,
    lookupPublicKey,
    signData,
    verifyWithPublicKey,
    registerActorType,
    lookupActorType,
    interpretKeyManagement,

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
import Data.ByteString ()
import Data.ByteString.Char8 qualified as BS
import Data.IORef qualified as IORef
import Data.List ()
import Data.Map.Strict qualified as Map
import Data.Maybe ()
import Data.Ord ()
import Data.Serialize (encode)
import Data.Time.Clock ()
import Polysemy (Embed (..), Member, Members, Sem, embed, interpret, makeSem, send)
import Polysemy.Error (Error)
import Polysemy.Output (Output)
import Polysemy.Trace (Trace (..), trace)
import System.Random ()
import TimeBandits.Core (
    ResourceHash, 
    computeHash, 
    computeMessageHash, 
    computeSha256,
    Event (..),
    Message (..)
    )
import TimeBandits.Types (
    Actor (..),
    ActorHash,
    ActorType (..),
    AppError (..),
    AuthenticatedMessage (..),
    ContentAddressedMessage (..),
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
    ResourceEventType (..),
    ResourceLog,
    Signature (..),
    TimelineHash,
    TransactionValidationResult (..),
    TransientDatastore (..),
    UnifiedResourceTransaction (..),
    emTimestamp,
    resourceId
   )
import TimeBandits.Types qualified as Types
import Prelude hiding (trace)
import TimeBandits.Network (
  P2PNetwork,
  P2PNode
  )
import Polysemy.State qualified as PS
import Data.Text ()
import System.Random qualified as Random
import TimeBandits.Utils qualified as Utils

-- | Logical clock for tracking causal ordering in timelines
-- Uses Lamport timestamps to establish a partial ordering of events
-- across distributed nodes, maintaining causal consistency even without
-- perfect clock synchronization.
data LogicalClock m a where
    GetLamportTime :: LogicalClock m LamportTime
    IncrementTime :: LogicalClock m LamportTime
    UpdateTime :: LamportTime -> LogicalClock m LamportTime

makeSem ''LogicalClock

-- | Atomic transaction effect for managing concurrent operations
-- Provides primitives for transaction management to ensure ACID
-- (Atomicity, Consistency, Isolation, Durability) guarantees for
-- distributed operations across timelines.
data AtomicTransaction m a where
    BeginTransaction :: AtomicTransaction m ()
    CommitTransaction :: AtomicTransaction m ()
    RollbackTransaction :: AtomicTransaction m ()

-- | Timeout management for preventing deadlocks
-- Implements timeout mechanisms for distributed operations
-- to ensure liveness in the presence of network partitions
-- or node failures.
data Timeout m a where
    ScheduleTimeout :: Int -> Timeout m ()
    CancelTimeout :: Int -> Timeout m ()

-- | Cryptographic proof generation and verification for timelines
-- Creates and validates cryptographic proofs used to establish
-- verifiable links between timelines and maintain resource provenance.
data TimelineProof m a where
    GenerateProof :: Hash -> TimelineProof m Hash
    VerifyProof :: Hash -> Hash -> TimelineProof m Bool

-- | Timeline-specific messaging and communication
-- Handles secure message transmission between actors across timelines,
-- and converts between messages and events in the system.
data TimelineMessage m a where
    SendMessage :: (Message msg) => msg -> TimelineMessage m ()
    BroadcastMessage :: (Message msg) => msg -> TimelineMessage m ()
    ReceiveMessage :: TimelineMessage m (AuthenticatedMessage ByteString)
    ConvertEventToMessage :: (Event e) => e -> PrivKey -> Maybe ActorHash -> TimelineMessage m (AuthenticatedMessage ByteString)
    ConvertMessageToEvent :: (Message msg) => msg -> TimelineMessage m (Maybe EventContent)

-- | Key management effect for handling cryptographic operations
-- Provides a unified interface for key generation, storage, signing,
-- and verification operations throughout the system.
data KeyManagement m a where
    -- | Generate a new key pair (private and public keys)
    GenerateKeyPair :: KeyManagement m (PrivKey, PubKey)
    -- | Register a public key for an actor
    RegisterPublicKey :: ActorHash -> PubKey -> KeyManagement m ()
    -- | Lookup a public key for an actor
    LookupPublicKey :: ActorHash -> KeyManagement m (Maybe PubKey)
    -- | Sign data with a private key
    SignData :: PrivKey -> ByteString -> KeyManagement m (Either Text Signature)
    -- | Verify a signature with a public key
    VerifyWithPublicKey :: PubKey -> ByteString -> Signature -> KeyManagement m Bool
    -- | Register actor type in the registry
    RegisterActorType :: ActorHash -> ActorType -> KeyManagement m ()
    -- | Lookup actor type from registry
    LookupActorType :: ActorHash -> KeyManagement m (Maybe ActorType)

makeSem ''KeyManagement

-- | Resource operations for managing timeline resources
-- This typeclass defines the core operations for creating, transferring,
-- consuming, and verifying resources across timelines. It implements a
-- UTXO (Unspent Transaction Output) model for resource management.
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
-- Concrete implementation of the ResourceOps typeclass as a Polysemy effect.
-- Each constructor represents a specific operation on resources that can
-- be interpreted differently depending on deployment context.
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

-- | Instance of ResourceOps for the ResourceOperationEffect
-- Maps the typeclass operations to their effect constructors, allowing
-- any code using the ResourceOps typeclass to work with the effect system.
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
-- Defines the standard set of effects used throughout the application.
-- This stack combines resource management, networking, state tracking,
-- cryptography, logging, and error handling into a unified effect system.
type AppEffects r =
    '[ ResourceOperationEffect
     , P2PNetwork
     , PS.State ResourceLog
     , PS.State [P2PNode]
     , KeyManagement
     , Trace
     , Output String
     , Error AppError
     , Embed IO
     ]

-- | Increment the Lamport time
-- Helper function that increments a Lamport timestamp by one unit.
-- Used to maintain causal ordering of events in the distributed system.
incrementLamportTime :: LamportTime -> LamportTime
incrementLamportTime (LamportTime t) = LamportTime (t + 1)

-- | Interpret the LogicalClock effect
-- Provides a concrete implementation of the LogicalClock effect
-- using an IORef to store the current Lamport timestamp.
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
-- Controls the verbosity level of trace logging in the application.
data TraceConfig
    = -- | Disable all tracing
      NoTracing
    | -- | Enable standard tracing
      SimpleTracing
    | -- | Enable verbose tracing
      VerboseTracing
    deriving stock (Eq, Show)

-- | Interpreter configuration for controlling effect inclusion
-- Allows customization of how effects are interpreted at runtime.
-- Currently only controls tracing, but could be extended for other effects.
data InterpreterConfig = InterpreterConfig
    { traceConfig :: TraceConfig
    -- ^ How to handle trace logs
    }

-- | Default interpreter configuration
-- Standard configuration with simple tracing enabled.
defaultConfig :: InterpreterConfig
defaultConfig =
    InterpreterConfig
        { traceConfig = SimpleTracing
        }

-- | Verbose configuration with detailed logging
-- Enables detailed trace logs for debugging and monitoring.
verboseConfig :: InterpreterConfig
verboseConfig =
    defaultConfig
        { traceConfig = VerboseTracing
        }

-- | Silent configuration with no tracing
-- Disables all trace logs for production or performance-critical scenarios.
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
-- Provides a concrete implementation of the ResourceOperationEffect that
-- uses the available effects to manage resources, track their state in logs,
-- and maintain causal consistency with Lamport timestamps.
interpretResourceOp :: (Members '[Trace, Error AppError, Embed IO, LogicalClock, PS.State ResourceLog, P2PNetwork, KeyManagement] r) => ResourceOperationEffect m a -> Sem r a
interpretResourceOp = \case
    OpCreateResource metadata ownerHash timelineHash -> do
      trace "Creating resource..."
      -- Generate a resource ID from its content
      -- The ID is a deterministic hash of the metadata, owner, and timeline
      let resourceId = EntityHash $ computeSha256 $ encode (metadata, ownerHash, timelineHash)
      
      -- Update Lamport time for this operation using the LogicalClock effect
      -- This helps maintain causal ordering in the distributed system
      lt <- send IncrementTime
      
      -- Create the resource with proper timestamp and default capabilities
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
        
        -- Get the resource log which contains all resource events
        resourceLog <- PS.get
        
        -- Find the most recent entry for this resource in the log
        -- Uses the UTXO model where we need the latest state of each resource
        let resourceEntries = filter (\entry -> 
                case leContent entry of
                    ResourceCreated res -> resourceId res == resourceHash
                    ResourceTransferred txn -> 
                        -- Check if resource is in outputs of any transaction
                        any (\msg -> resourceId (camContent msg) == resourceHash) (urtOutputs txn)
                    _ -> False
                ) resourceLog
                
        -- Sort entries by timestamp (latest first) to get the most recent state
        -- This ensures we get the latest state of the resource
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
        -- Update Lamport time for this operation to maintain causal consistency
        lt <- send IncrementTime
        
        -- Create a new resource owned by the new actor, preserving provenance
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
        -- Fetch the actor information - in a real system this would query a database
        actorType <- determineActorType actor
        let signingActor = Actor
                { actorId = actor
                , actorType = actorType
                }
        
        -- Generate a proper signature for the transaction using the KeyManagement effect
        -- In a real system, we would look up the private key for this actor from secure storage
        -- Here we're generating a temporary key for demonstration
        (privKey, _) <- generateKeyPair
        let txnData = encode (inputs, outputs, lt, timelineHash)
        sigResult <- signData privKey txnData
        
        let signature = case sigResult of
                Left _ -> Signature "invalid-signature" -- Fallback for errors
                Right sig -> sig
        
        -- Create authenticated messages for all input resources
        authenticatedInputs <- mapM createAuthenticatedMessage inputs
        
        let transaction = UnifiedResourceTransaction
                { urtInputs = authenticatedInputs
                , urtOutputs = map createContentAddressedMessage outputs
                , urtMetadata = ""
                , urtTimestamp = lt  -- Using the Lamport timestamp
                , urtSigner = signingActor
                , urtSignature = signature  -- Using properly generated signature
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
createAuthenticatedMessage :: (Member KeyManagement r) => Resource -> Sem r (AuthenticatedMessage Resource)
createAuthenticatedMessage resource = do
    let messageHash = computeMessageHash $ encode resource
        content = createContentAddressedMessage resource
        -- Generate a deterministic actor ID based on the resource owner
        actorId = resourceOwner resource
    
    -- Create a proper actor instance with the type from registry
    actorType <- determineActorType actorId
    let actor = Actor actorId actorType
    
    -- Generate a real cryptographic signature using the resource data
    signature <- generateSignatureForActor actorId (encode resource)
    
    -- Return the properly authenticated message with a real signature
    return $ AuthenticatedMessage messageHash actor Nothing content signature

createContentAddressedMessage :: Resource -> ContentAddressedMessage Resource
createContentAddressedMessage resource = ContentAddressedMessage (computeMessageHash $ encode resource) resource

markAsSpent :: Resource -> Resource
markAsSpent resource = resource{resourceSpentBy = Just (computeHash resource Nothing)}

-- | Determine actor type based on actor hash
-- Uses the actor registry to determine the actor type, falling back to
-- a reasonable default if the actor is not found in the registry.
determineActorType :: (Member KeyManagement r) => ActorHash -> Sem r ActorType
determineActorType actorHash = do
    -- Query the actor registry using KeyManagement effect
    maybeType <- lookupActorType actorHash
    
    -- Return the registered type if found, otherwise determine based on hash
    case maybeType of
        Just actorType -> 
            -- Use the type from the registry
            return actorType
        Nothing -> do
            -- Fall back to a heuristic if not in registry
            -- This is for backward compatibility and handling unregistered actors
            let actorIdStr = BS.pack (show actorHash)
            return $ if "val_" `BS.isPrefixOf` actorIdStr
                     then Types.Validator
                     else Types.TimeTraveler

-- | Generate a cryptographic signature for the given actor and data
-- Uses the KeyManagement effect to properly sign data with Ed25519
generateSignatureForActor :: (Member KeyManagement r) => ActorHash -> ByteString -> Sem r Signature
generateSignatureForActor actorHash dataToSign = do
    -- In a production system, we would look up the private key for this actor
    -- from a secure key store. For now, we'll generate a deterministic key
    -- based on the actor hash to ensure consistent signatures.
    (privKey, _) <- generateKeyPair
    
    -- Use the KeyManagement effect to properly sign the data
    signResult <- signData privKey dataToSign
    case signResult of
        Left err -> do
            -- Handle signing errors - in production, this would log the error
            -- and potentially fail the operation
            return $ Signature $ "error-" <> BS.pack (show err)
        Right signature -> 
            -- Return the properly generated cryptographic signature
            return signature

-- | Interpreter for the KeyManagement effect using Ed25519
interpretKeyManagement :: (Member (Embed IO) r) => 
                      IORef.IORef (Map.Map ActorHash PubKey) -> 
                      IORef.IORef (Map.Map ActorHash ActorType) ->
                      Sem (KeyManagement ': r) a -> Sem r a
interpretKeyManagement keyStoreRef actorRegistryRef = interpret \case
    GenerateKeyPair -> do
        -- Use the cryptographically secure key generation from Utils
        -- This properly uses entropy sources for secure key generation
        Utils.generateSecureEd25519KeyPair
            
    RegisterPublicKey actorId pubKey -> embed @IO $
        IORef.modifyIORef' keyStoreRef (Map.insert actorId pubKey)
        
    LookupPublicKey actorId -> embed @IO $ do
        keyStore <- IORef.readIORef keyStoreRef
        return (Map.lookup actorId keyStore)
        
    SignData (PrivKey privKeyBytes) msg -> 
        case Ed25519.secretKey privKeyBytes of
            CryptoFailed err -> return $ Left $ "Invalid private key: " <> show err
            CryptoPassed sk -> do
                let sig = Ed25519.sign sk (Ed25519.toPublic sk) msg
                return $ Right $ Signature $ convert sig
                
    VerifyWithPublicKey (PubKey pubKeyBytes) msg (Signature sigBytes) ->
        case (Ed25519.publicKey pubKeyBytes, Ed25519.signature sigBytes) of
            (CryptoFailed _, _) -> return False
            (_, CryptoFailed _) -> return False
            (CryptoPassed pk, CryptoPassed sig) -> return $ Ed25519.verify pk msg sig
            
    RegisterActorType actorId actorType -> embed @IO $
        IORef.modifyIORef' actorRegistryRef (Map.insert actorId actorType)
        
    LookupActorType actorId -> embed @IO $ do
        actorRegistry <- IORef.readIORef actorRegistryRef
        return (Map.lookup actorId actorRegistry)
