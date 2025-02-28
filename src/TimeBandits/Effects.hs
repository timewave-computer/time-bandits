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
    -- * Effect Stack
    AppEffects,
    interpretAppEffects,

    -- * Error Types
    TimelineErrorType (..),
    ResourceErrorType (..),
    ActorErrorType (..),
    CryptoErrorType (..),
    StorageErrorType (..),

    -- * Error Conversion Functions
    asTimelineError,
    asResourceError,
    asActorError,
    asCryptoError,
    asStorageError,

    -- * Effects
    LogicalClock (..),
    AtomicTransaction (..),
    Timeout (..),
    TimelineProof (..),
    TimelineMessage (..),
    TimelineResource (..),
    CryptoOperation (..),
    AuthenticatedMessage (..),
    TransientStorage (..),
    BanditSubscriptions (..),
    TimelineEffect (..),

    -- * Effect Interpreters
    interpretLogicalClock,
    interpretAtomicTransaction,
    interpretTimeout,
    interpretTimelineProof,
    interpretTimelineMessage,
    interpretTimelineResource,
    interpretCryptoOperation,
    interpretTransientStorage,
    interpretBanditSubscriptions,
    interpretTimelineEffect,

    -- * Message Effect Functions
    semCreateMessage,
    semAuthenticateMessage,

    -- * Helper Functions
    emptyTrie,
    (?!>),
    assignTimeBandits,
    convertEventType,
    convertLogEntry,
    convertTimelineLog,
    convertToTimelineEvent,
    validateEventSequence,
    validateEventTimestamps,
    validateEvents,
    storeTimelineLog,
    getTimelineLog,
    checkTimelineExists,
    initializeTimelineLog,
    validateMerge,
    mergeEvents,
    mergeSyncPoints,
    mergeLogs,
    createTimelineBlock,
    updateTimelineLog,
    convertEventsOrThrow,

    -- * Actor Management
    ActorManagement (..),
    interpretActorManagement,

    -- * Resource Operations
    transferResourceOp,

    -- * Resource operations for managing timeline resources
    ResourceOps (..),

    -- * Resource operations effect
    ResourceOperationEffect (..),
    -- | Get transaction history for a resource
    getTransactionHistoryOp,
    -- | Demonstrate how to use the unified resource transaction model
    -- This function creates a resource, transfers it to another actor, and then consumes it
    demonstrateUnifiedTransactionModel,
) where

import Control.Monad (forM, forM_, when)
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash.SHA256 qualified as SHA256
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.IORef ()
import Data.List (nub, sortBy, union)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Serialize (decode, encode)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (getCurrentTime)
import Polysemy (Embed, Member, Members, Sem, interpret, makeSem, runM, send)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Output (Output, output, runOutputList)
import Polysemy.Reader
import Polysemy.State
import Polysemy.Trace (Trace, trace, traceToStdout)
import TimeBandits.Core (computeHash, computeMessageHash, computePubKeyHash, computeSha256)
import TimeBandits.Events hiding (StorageError, getTimelineLog)
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
    ResourceHash,
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

-- | Resource transfer operation
transferResourceOp :: (Members '[Error AppError, LogicalClock] r) => Resource -> ActorHash -> TimelineHash -> LamportTime -> [TimelineHash] -> Sem r (Either AppError (Resource, ResourceEvent))
transferResourceOp resource newOwner destTimeline timestamp provChain = do
    let updatedResource = resource{resourceOwner = newOwner}
        event =
            ResourceEvent
                { reContent = ResourceCreated updatedResource
                , reMetadata =
                    EventMetadata
                        { emTimestamp = timestamp
                        , emCreatedAt = undefined -- TODO: Fix timestamp type
                        , emActor = newOwner
                        , emTimeline = destTimeline
                        , emSignature = undefined -- TODO: Get proper signature
                        , emSigner = PubKey "TODO" -- TODO: Get proper signer
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
data TimelineResource m a where
    GetResourceTime :: TimelineResource m LamportTime
    LogResourceEvent :: ResourceEvent -> TimelineResource m ()
    GetResourceByHash :: Hash -> TimelineResource m (Maybe Resource)

-- | Transient storage effect for P2P data storage
data TransientStorage m a where
    StoreItem :: TransientStoredItem -> TransientStorage m (Either AppError ())
    RetrieveItem :: ByteString -> TransientStorage m (Either AppError TransientStoredItem)
    GetResponsibleNodes :: ByteString -> TransientStorage m [Actor]
    UpdateDatastore :: TransientDatastore -> TransientStorage m ()

-- | Effect for managing Time Bandit subscriptions to timelines
data BanditSubscriptions m a where
    GetSubscriptions :: BanditSubscriptions m [TimelineHash]
    AddSubscription :: TimelineHash -> BanditSubscriptions m ()
    RemoveSubscription :: TimelineHash -> BanditSubscriptions m ()
    SetSubscriptions :: [TimelineHash] -> BanditSubscriptions m ()

-- | Cryptographic operations effect
data CryptoOperation m a where
    SignMessage :: PrivKey -> ByteString -> CryptoOperation m (Either AppError Signature)
    VerifySignature :: PubKey -> ByteString -> Signature -> CryptoOperation m Bool
    GenerateKeyPair :: ByteString -> CryptoOperation m (Either AppError (PubKey, PrivKey))

-- | Consolidated timeline effect
data TimelineEffect m a where
    -- Timeline creation and management
    CreateNewTimeline :: TimelineHash -> ActorHash -> TimelineEffect m (Either TimelineErrorType TimelineLog)
    MergeTimelines :: TimelineHash -> TimelineHash -> TimelineEffect m (Either TimelineErrorType TimelineLog)
    -- Event registration and retrieval
    RegisterEvent :: TimelineHash -> EventContent -> TimelineEffect m (Either TimelineErrorType (LogEntry EventContent))
    GetTimelineHistory :: TimelineHash -> TimelineEffect m (Either TimelineErrorType [LogEntry EventContent])
    GetObjectHistory :: TimelineHash -> Hash -> TimelineEffect m (Either TimelineErrorType [LogEntry EventContent])
    GetEventsAfterMerkle :: TimelineHash -> Hash -> TimelineEffect m (Either TimelineErrorType [LogEntry EventContent])
    GetPendingEvents :: TimelineHash -> TimelineEffect m (Either TimelineErrorType [LogEntry EventContent])
    -- Block finalization
    FinalizeTimelineBlock :: TimelineHash -> [LogEntry EventContent] -> TimelineEffect m (Either TimelineErrorType TimelineBlock)

-- | The core effect stack for the Time Bandits application
type AppEffects r =
    '[ TimelineEffect
     , BanditSubscriptions
     , TransientStorage
     , TimelineResource
     , TimelineMessage
     , TimelineProof
     , Timeout
     , AtomicTransaction
     , LogicalClock
     , CryptoOperation
     , Error AppError
     , Output String
     , Trace
     , Embed IO
     ]

-- | Helper function to get a resource from storage
getResource :: (Members '[Embed IO, Error AppError, Output String] r) => ResourceHash -> Sem r (Maybe Resource)
getResource rHash = do
    output $ "Getting resource: " <> show rHash
    -- TODO: Implement actual storage retrieval
    pure Nothing

-- | Helper function to get a resource and its complete lineage
getResourceWithLineage :: ResourceHash -> ResourceLog -> ResourceLog
getResourceWithLineage targetHash log =
    [entry | entry@LogEntry{leContent = ResourceCreated res} <- log, resourceId res == targetHash]

-- | Helper function to check if an event is relevant for a resource
isRelevantEvent :: ResourceHash -> LogEntry ResourceEventType -> Bool
isRelevantEvent hash entry = case leContent entry of
    ResourceCreated res -> resourceId res == hash
    ResourceTransferred tx -> any (\output -> resourceId (camContent output) == hash) (urtOutputs tx)
    ResourceCapabilityChecked{rcCheckedResource = checkedHash} -> checkedHash == hash
    ResourceVerified{rvResource = verifiedHash} -> verifiedHash == hash

-- | Root timeline hash for actor events
rootTimelineHash :: TimelineHash
rootTimelineHash = EntityHash $ Hash "root-timeline" -- TODO: Use proper genesis hash

-- | Empty trie for initialization
emptyTrie :: Trie a
emptyTrie = Trie Map.empty

-- | Operator for Maybe to Either conversion with error
(?!>) :: (Members '[Error e] r) => Sem r (Maybe a) -> e -> Sem r a
maybeVal ?!> err = do
    val <- maybeVal
    case val of
        Just x -> pure x
        Nothing -> throw err

-- | Sort events by timestamp
sortEventsByTimestamp :: [LogEntry a] -> [LogEntry a]
sortEventsByTimestamp = sortWith (emTimestamp . leMetadata)

-- | Convert between event types
convertEventType :: TimelineEventType -> EventContent
convertEventType = TimelineEventContent

-- | Convert back to timeline event type
convertToTimelineEvent :: EventContent -> Maybe TimelineEventType
convertToTimelineEvent = \case
    TimelineEventContent evt -> Just evt
    _ -> Nothing

-- | Convert log entries back to timeline events
convertToTimelineLog :: [LogEntry EventContent] -> Maybe [LogEntry TimelineEventType]
convertToTimelineLog = traverse (\entry -> (\evt -> entry{leContent = evt}) <$> convertToTimelineEvent (leContent entry))

-- | Validate event sequence
validateEventSequence :: (Members '[Error TimelineErrorType] r) => [LogEntry EventContent] -> Sem r ()
validateEventSequence [] = pure ()
validateEventSequence [_] = pure ()
validateEventSequence (e1 : e2 : es) = do
    when (emTimestamp (leMetadata e1) >= emTimestamp (leMetadata e2)) $
        throw $
            InvalidTimelineState "Events not in chronological order"
    validateEventSequence (e2 : es)

-- | Validate event timestamps
validateEventTimestamps :: (Members '[Error TimelineErrorType] r) => LamportTime -> [LogEntry EventContent] -> Sem r ()
validateEventTimestamps lastTime events = do
    forM_ events $ \event -> do
        when (emTimestamp (leMetadata event) <= lastTime) $
            throw $
                InvalidTimelineState "Event timestamp before last processed time"

-- | Convert log entry
convertLogEntry :: LogEntry TimelineEventType -> LogEntry EventContent
convertLogEntry entry = entry{leContent = convertEventType (leContent entry)}

-- | Compute Merkle tree root from list of hashes
computeMerkleTreeRoot :: [Hash] -> Hash
computeMerkleTreeRoot [] = Hash "empty" -- Should never happen in practice
computeMerkleTreeRoot [h] = h
computeMerkleTreeRoot hs =
    let pairs = zip hs (drop 1 hs ++ [Hash "empty"])
        nextLevel = map (uncurry combineMerkleHashes) pairs
     in computeMerkleTreeRoot nextLevel

-- | Combine two hashes in Merkle tree
combineMerkleHashes :: Hash -> Hash -> Hash
combineMerkleHashes (Hash h1) (Hash h2) =
    computeSha256 $ h1 <> h2

-- | Merge two event tries
mergeEvents :: Trie (LogEntry TimelineEventType) -> Trie (LogEntry TimelineEventType) -> Trie (LogEntry TimelineEventType)
mergeEvents (Trie src) (Trie dst) = Trie $ Map.union src dst

-- | Merge sync points from two timelines
mergeSyncPoints :: [Hash] -> [Hash] -> [Hash]
mergeSyncPoints = union

-- | Compute Merkle root for a list of events
computeMerkleRoot :: [LogEntry EventContent] -> Hash
computeMerkleRoot events =
    let eventHashes = map leHash events
     in computeMerkleTreeRoot eventHashes

-- | Validate events
validateEvents :: (Members '[Error TimelineErrorType] r) => TimelineLog -> [LogEntry EventContent] -> Sem r ()
validateEvents log events = do
    -- Check event sequence
    validateEventSequence events
    -- Check timestamps
    validateEventTimestamps (tlLastProcessedTime log) events

-- | Merge two timeline logs
mergeLogs :: LamportTime -> TimelineLog -> TimelineLog -> TimelineLog
mergeLogs timestamp src dst =
    dst
        { tlEvents = mergeEvents (tlEvents src) (tlEvents dst)
        , tlLastProcessedTime = timestamp
        , tlSyncPoints = mergeSyncPoints (tlSyncPoints src) (tlSyncPoints dst)
        }

-- | Check if a timeline exists
checkTimelineExists :: (Members '[Embed IO] r) => TimelineHash -> Sem r Bool
checkTimelineExists th = do
    -- TODO: Implement actual storage check
    pure False

-- | Get a timeline log from storage
getTimelineLog :: (Members '[Embed IO] r) => TimelineHash -> Sem r (Maybe TimelineLog)
getTimelineLog th = do
    -- TODO: Implement actual storage retrieval
    pure Nothing

-- | Initialize a new timeline log
initializeTimelineLog :: TimelineHash -> LamportTime -> ActorHash -> TimelineLog
initializeTimelineLog th time actor =
    TimelineLog
        { tlEvents = emptyTrie
        , tlLatestMerkleRoot = Nothing
        , tlTimelineId = th
        , tlLastProcessedTime = time
        , tlSyncPoints = []
        }

-- | Store a timeline log
storeTimelineLog :: (Members '[Embed IO] r) => TimelineHash -> TimelineLog -> Sem r ()
storeTimelineLog th log = do
    -- TODO: Implement actual storage
    pure ()

-- | Validate that two timelines can be merged
validateMerge :: (Members '[Error TimelineErrorType] r) => TimelineLog -> TimelineLog -> Sem r ()
validateMerge src dst = do
    -- Check for conflicts
    when (tlLastProcessedTime src > tlLastProcessedTime dst) $
        throw $
            TimelineMergeConflict (tlTimelineId src) (tlTimelineId dst)

-- | Create a new timeline
createNewTimeline ::
    ( Members '[LogicalClock, Error TimelineErrorType, Output String, Embed IO] r
    ) =>
    TimelineHash ->
    ActorHash ->
    Sem r (Either TimelineErrorType TimelineLog)
createNewTimeline th actor = do
    output $ "Creating timeline: " <> show th
    timestamp <- send GetCurrentTime
    timelineExists <- checkTimelineExists th
    if timelineExists
        then throw $ TimelineAlreadyExists th
        else do
            let newLog = initializeTimelineLog th timestamp actor
            storeTimelineLog th newLog
            pure $ Right newLog

-- | Merge two existing timelines
mergeExistingTimelines ::
    ( Members '[LogicalClock, Error TimelineErrorType, Output String, Embed IO] r
    ) =>
    TimelineHash ->
    TimelineHash ->
    Sem r (Either TimelineErrorType TimelineLog)
mergeExistingTimelines src dst = do
    output $ "Merging timelines: " <> show src <> " -> " <> show dst
    srcLog <- TimeBandits.Effects.getTimelineLog src ?!> TimelineNotFound src
    dstLog <- TimeBandits.Effects.getTimelineLog dst ?!> TimelineNotFound dst
    validateMerge srcLog dstLog
    timestamp <- send GetCurrentTime
    let mergedLog = mergeLogs timestamp srcLog dstLog
    storeTimelineLog dst mergedLog
    pure $ Right mergedLog

-- | Get timeline event history
getTimelineEventHistory ::
    ( Members '[Error TimelineErrorType, Output String, Embed IO] r
    ) =>
    TimelineHash ->
    Sem r (Either TimelineErrorType [LogEntry EventContent])
getTimelineEventHistory th = do
    output $ "Getting timeline history: " <> show th
    timelineLog <- TimeBandits.Effects.getTimelineLog th ?!> TimelineNotFound th
    let timelineEvents = sortEventsByTimestamp $ elemsTrie $ tlEvents timelineLog
        contentEvents = map convertLogEntry timelineEvents
    pure $ Right contentEvents

-- | Finalize timeline events into a block
finalizeTimelineEvents ::
    ( Members '[LogicalClock, Error TimelineErrorType, Output String, Embed IO] r
    ) =>
    TimelineHash ->
    [LogEntry EventContent] ->
    Sem r (Either TimelineErrorType TimelineBlock)
finalizeTimelineEvents th events = do
    output $ "Finalizing events for timeline: " <> show th
    timelineLog <- TimeBandits.Effects.getTimelineLog th ?!> TimelineNotFound th
    timelineEvents <- convertEventsOrThrow events
    validateEvents timelineLog events
    timestamp <- send GetCurrentTime
    let merkleRoot = computeMerkleRoot events
        block = createTimelineBlock timelineEvents merkleRoot timelineLog timestamp
        updatedLog = updateTimelineLog timelineLog merkleRoot timestamp
    storeTimelineLog th updatedLog
    pure $ Right block

-- | Convert events or throw an error
convertEventsOrThrow ::
    ( Member (Error TimelineErrorType) r
    ) =>
    [LogEntry EventContent] ->
    Sem r [LogEntry TimelineEventType]
convertEventsOrThrow events =
    case convertToTimelineLog events of
        Nothing -> throw $ InvalidTimelineState "Invalid event types"
        Just timelineEvents -> pure timelineEvents

-- | Create a timeline block
createTimelineBlock :: [LogEntry TimelineEventType] -> Hash -> TimelineLog -> LamportTime -> TimelineBlock
createTimelineBlock events merkleRoot timelineLog timestamp =
    TimelineBlock
        { tbEvents = events
        , tbMerkleRoot = merkleRoot
        , tbPrevBlock = tlLatestMerkleRoot timelineLog
        , tbTimestamp = timestamp
        }

-- | Update timeline log with new merkle root and timestamp
updateTimelineLog :: TimelineLog -> Hash -> LamportTime -> TimelineLog
updateTimelineLog log merkleRoot timestamp =
    log
        { tlLatestMerkleRoot = Just merkleRoot
        , tlLastProcessedTime = timestamp
        }

-- | Actor management effect
data ActorManagement m a where
    CreateActor :: ActorType -> ActorManagement m Actor
    GetActor :: ActorHash -> ActorManagement m (Maybe Actor)
    UpdateActor :: Actor -> ActorManagement m ()
    DeleteActor :: ActorHash -> ActorManagement m ()

makeSem ''ActorManagement

-- | Assign time bandits for a key
assignTimeBandits :: TransientDatastore -> ByteString -> [Actor]
assignTimeBandits store key =
    let hash = computeSha256 key
        bandits = tdTimeBandits store
     in take 3 $ sortBy (comparing actorId) bandits

-- | Convert timeline log entries
convertTimelineLog :: TimelineLog -> [LogEntry EventContent]
convertTimelineLog log = map convertLogEntry $ elemsTrie $ tlEvents log

--------------------------------------------------------------------------------

-- ** Effect Interpreters **

--------------------------------------------------------------------------------

{- | Effect interpreters transform abstract effects into concrete implementations.
Each interpreter follows the naming convention of starting with "interpret" and
is responsible for handling a specific effect type.
-}

-- | Interpret the complete application effect stack into a final IO action.
interpretAppEffects :: IORef LamportTime -> IORef ResourceLog -> IORef TransientDatastore -> IORef [TimelineHash] -> Sem (AppEffects r) a -> IO (Either AppError ([String], a))
interpretAppEffects timeRef logRef storeRef subsRef action = do
    runM
        . traceToStdout
        . fmap
            ( \(logs, res) -> case res of
                Left err -> Left err
                Right val -> Right (logs, val)
            )
        . runOutputList
        . runError @AppError
        . (\r -> trace "Before interpretCryptoOperation" >> interpretCryptoOperation r)
        . (\r -> trace "Before interpretLogicalClock" >> interpretLogicalClock timeRef r)
        . (\r -> trace "Before interpretAtomicTransaction" >> interpretAtomicTransaction r)
        . (\r -> trace "Before interpretTimeout" >> interpretTimeout r)
        . (\r -> trace "Before interpretTimelineProof" >> interpretTimelineProof r)
        . (\r -> trace "Before interpretTimelineMessage" >> interpretTimelineMessage r)
        . (\r -> trace "Before interpretTimelineResource" >> interpretTimelineResource logRef r)
        . (\r -> trace "Before interpretTransientStorage" >> interpretTransientStorage storeRef r)
        . (\r -> trace "Before interpretBanditSubscriptions" >> interpretBanditSubscriptions subsRef r)
        . (\r -> trace "Before interpretTimelineEffect" >> interpretTimelineEffect r)
        $ action

-- | Interpret the logical clock effect
interpretLogicalClock :: (Members '[Trace, Embed IO] r) => IORef LamportTime -> Sem (LogicalClock ': r) a -> Sem r a
interpretLogicalClock timeRef = interpret \case
    GetCurrentTime -> do
        trace "Getting current logical time"
        liftIO $ readIORef timeRef
    IncrementTime -> do
        trace "Incrementing logical time"
        liftIO $ atomicModifyIORef' timeRef $ \(LamportTime t) ->
            let newTime = LamportTime (t + 1)
             in (newTime, newTime)
    UpdateTime newTime@(LamportTime t) -> do
        trace $ "Updating logical time to: " <> show t
        liftIO $ atomicModifyIORef' timeRef $ \(LamportTime current) ->
            let maxTime = LamportTime (max t current)
             in (maxTime, maxTime)

-- | Interpret the atomic transaction effect
interpretAtomicTransaction :: (Members '[Output String] r) => Sem (AtomicTransaction ': r) a -> Sem r a
interpretAtomicTransaction = interpret \case
    BeginTransaction -> output "Transaction begun"
    CommitTransaction -> output "Transaction committed"
    RollbackTransaction -> output "Transaction rolled back"

-- | Interpret the timeout effect
interpretTimeout :: (Members '[Output String] r) => Sem (Timeout ': r) a -> Sem r a
interpretTimeout = interpret \case
    ScheduleTimeout t -> output $ "Timeout scheduled for: " ++ show t
    CancelTimeout t -> output $ "Timeout cancelled for: " ++ show t

-- | Interpret the timeline proof effect
interpretTimelineProof :: (Members '[Output String, Error AppError] r) => Sem (TimelineProof ': r) a -> Sem r a
interpretTimelineProof = interpret \case
    GenerateProof h -> do
        output $ "Generating proof for hash: " ++ show h
        pure h -- TODO: Implement actual proof generation
    VerifyProof h p -> do
        output $ "Verifying proof for hash: " <> show h
        pure True -- TODO: Implement actual proof verification

-- | Interpret the timeline messaging effect
interpretTimelineMessage :: (Members '[Output String, Error AppError, CryptoOperation] r) => Sem (TimelineMessage ': r) a -> Sem r a
interpretTimelineMessage = interpret \case
    SendMessage msg -> do
        output $ "Sending message to: " ++ maybe "broadcast" show (messageDestination msg)
        -- TODO: Implement actual network sending
        pure ()
    BroadcastMessage msg -> do
        output "Broadcasting message to all nodes"
        -- TODO: Implement actual network broadcasting
        pure ()
    ReceiveMessage -> do
        output "Waiting for message"
        -- TODO: Implement actual message receiving
        throw $ NetworkError "Message receiving not implemented"
    ConvertEventToMessage event privKey destination -> do
        output "Converting event to message"
        -- Implement directly instead of using Events module function
        let content = encode $ toEventContent event
            actor = Actor (EntityHash $ Hash "TODO") Validator -- TODO: Get proper actor
        sig <- send $ SignMessage privKey content
        case sig of
            Left err -> throw $ NetworkError "Failed to sign event as message"
            Right signature -> do
                let msgHash = computeMessageHash content
                    payload = ContentAddressedMessage msgHash content
                pure $ AuthenticatedMessage msgHash actor destination payload signature
    ConvertMessageToEvent msg -> do
        output "Converting message to event"
        -- Implement directly instead of using Events module function
        -- This is a simplified implementation
        pure Nothing -- TODO: Implement proper conversion

-- | Helper function to find a resource by its hash in the log
findResourceInLog :: Hash -> ResourceLog -> Maybe Resource
findResourceInLog hash log =
    case [res | LogEntry{leContent = ResourceCreated res} <- log, unEntityHash (resourceId res) == hash] of
        [] -> Nothing
        (res : _) -> Just res

-- | Helper function to find a unified transaction by its hash in the log
findUnifiedTransactionInLog :: Hash -> ResourceLog -> Maybe UnifiedResourceTransaction
findUnifiedTransactionInLog hash log =
    listToMaybe [tx | LogEntry{leContent = ResourceTransferred tx} <- log, computeHash tx Nothing == hash]

-- | Create a unified resource transaction
createUnifiedTransactionOp ::
    (Members '[CryptoOperation, Error AppError, Trace] r) =>
    [Resource] ->
    [Resource] ->
    Actor ->
    TimelineHash ->
    LamportTime ->
    PrivKey ->
    Sem r (Either AppError UnifiedResourceTransaction)
createUnifiedTransactionOp inputs outputs actor timeline timestamp privKey = do
    -- Convert inputs to authenticated messages
    inputMsgs <- forM inputs $ \input -> do
        let content = encode input
        sig <- send $ SignMessage privKey content
        case sig of
            Left err -> throw $ CryptoError InvalidSignatureError
            Right signature -> do
                let msgHash = computeMessageHash content
                    payload = ContentAddressedMessage msgHash input
                pure $ AuthenticatedMessage msgHash actor Nothing payload signature

    -- Convert outputs to content-addressed messages
    let outputMsgs =
            map
                ( \output ->
                    let content = encode output
                        msgHash = computeMessageHash content
                     in ContentAddressedMessage msgHash output
                )
                outputs

    -- Create transaction metadata
    let metadata = encode (map resourceId inputs, map resourceId outputs)

    -- Create provenance chain from input resources
    let provenanceChain = nub $ concatMap resourceProvenanceChain inputs ++ [timeline]

    -- Sign the transaction
    let txContent = encode (inputMsgs, outputMsgs, metadata, timestamp, actor, provenanceChain)
    sig <- send $ SignMessage privKey txContent
    case sig of
        Left err -> throw $ CryptoError InvalidSignatureError
        Right signature -> do
            let transaction =
                    UnifiedResourceTransaction
                        { urtInputs = inputMsgs
                        , urtOutputs = outputMsgs
                        , urtMetadata = metadata
                        , urtTimestamp = timestamp
                        , urtSigner = actor
                        , urtSignature = signature
                        , urtProvenanceChain = provenanceChain
                        }
            pure $ Right transaction

-- | Check if a transaction validation result is invalid
isInvalid :: TransactionValidationResult -> Bool
isInvalid (TransactionInvalid _) = True
isInvalid _ = False

-- | Validate a unified resource transaction
validateTransactionOp ::
    (Members '[ResourceOperationEffect, CryptoOperation, Error AppError, Trace] r) =>
    UnifiedResourceTransaction ->
    Sem r (Either AppError TransactionValidationResult)
validateTransactionOp tx = do
    -- Verify transaction signature
    let txContent = encode (urtInputs tx, urtOutputs tx, urtMetadata tx, urtTimestamp tx, urtSigner tx, urtProvenanceChain tx)
        pubKey = getActorPubKey (urtSigner tx)

    sigValid <- send $ VerifySignature pubKey txContent (urtSignature tx)

    if not sigValid
        then pure $ Right $ TransactionInvalid $ BS.pack "Transaction signature verification failed"
        else do
            -- Verify all input resources exist and are unspent
            inputsValid <- forM (urtInputs tx) $ \inputMsg -> do
                -- Extract the content from the authenticated message
                let res = camContent (amPayload inputMsg)
                resourceExists <- send $ ResourceOpGetResourceById (resourceId res)
                case resourceExists of
                    Left err -> pure TransactionDeferred -- Resource not found, might be pending
                    Right foundRes -> do
                        -- Check if resource is unspent
                        if isJust (resourceSpentBy foundRes)
                            then pure $ TransactionInvalid $ BS.pack "Input resource already spent"
                            else do
                                -- Verify resource ownership
                                if resourceOwner foundRes == actorId (urtSigner tx)
                                    then pure TransactionValid
                                    else pure $ TransactionInvalid $ BS.pack "Resource not owned by transaction signer"

            -- If any input is invalid, the transaction is invalid
            if any isInvalid inputsValid
                then case [iv | iv@(TransactionInvalid _) <- inputsValid] of
                    (invalidResult : _) -> pure $ Right invalidResult
                    [] -> pure $ Right $ TransactionInvalid $ BS.pack "Unknown validation error"
                else
                    if any (== TransactionDeferred) inputsValid
                        then pure $ Right TransactionDeferred
                        else pure $ Right TransactionValid

-- | Execute a unified resource transaction
executeTransactionOp ::
    (Members '[ResourceOperationEffect, CryptoOperation, Error AppError, Trace] r) =>
    UnifiedResourceTransaction ->
    Sem r (Either AppError [Resource])
executeTransactionOp tx = do
    -- First validate the transaction
    validationResult <- validateTransactionOp tx

    case validationResult of
        Left err -> pure $ Left err
        Right (TransactionInvalid reason) -> pure $ Left $ ResourceError $ InvalidResourceState $ pack $ BS.unpack reason
        Right TransactionDeferred -> pure $ Left $ ResourceError $ ResourceNotFound (EntityHash $ Hash "Transaction inputs not available")
        Right TransactionValid -> do
            -- Mark all input resources as spent
            let txHash = computeHash tx Nothing

            forM_ (urtInputs tx) $ \inputMsg -> do
                -- Extract the content from the authenticated message
                let res = camContent (amPayload inputMsg)
                -- Mark resource as spent
                let updatedRes = res{resourceSpentBy = Just txHash}
                send $ ResourceOpUpdateResource updatedRes

            -- Create all output resources
            outputResources <- forM (urtOutputs tx) $ \outputMsg -> do
                -- For ContentAddressedMessage Resource, we can directly access the content
                let res = camContent outputMsg
                -- Create the resource
                send $ ResourceOpCreateResource res
                pure res

            -- Return the created resources
            pure $ Right outputResources

-- | Get transaction history for a resource
getTransactionHistoryOp ::
    (Members '[ResourceOperationEffect, Error AppError] r) =>
    ResourceHash ->
    Sem r (Either AppError [UnifiedResourceTransaction])
getTransactionHistoryOp resourceHash = do
    -- Get the resource
    resourceResult <- send $ ResourceOpGetResourceById resourceHash
    case resourceResult of
        Left err -> pure $ Left err
        Right resource -> do
            -- Get all transactions that created this resource
            parentTxs <- forM (resourceParents resource) $ \parentHash -> do
                txResult <- send $ ResourceOpGetTransactionByOutput parentHash
                case txResult of
                    Left _ -> pure Nothing
                    Right tx -> pure (Just tx)
            let validParentTxs = catMaybes parentTxs

            -- Get all transactions that spent this resource
            spentTx <- case resourceSpentBy resource of
                Nothing -> pure []
                Just txHash -> do
                    txResult <- send $ ResourceOpGetUnifiedTransactionByHash txHash
                    case txResult of
                        Left _ -> pure []
                        Right tx -> pure [tx]

            -- Return all transactions
            pure $ Right $ validParentTxs ++ spentTx

-- | Interpret the timeline resource effect
interpretTimelineResource ::
    ( Members '[Trace, Embed IO, LogicalClock, Output String, Error AppError] r
    ) =>
    IORef ResourceLog ->
    Sem (TimelineResource ': r) a ->
    Sem r a
interpretTimelineResource logRef = interpret \case
    -- Get the current resource time from the logical clock
    GetResourceTime -> send GetCurrentTime
    -- Log a new resource event
    LogResourceEvent event -> do
        currentLog <- liftIO $ readIORef logRef
        -- Create a log entry using the Events module's function
        let newEntry =
                LogEntry
                    { leHash = computeSha256 $ encode event
                    , leContent = reContent event
                    , leMetadata = reMetadata event
                    , lePrevHash = case currentLog of
                        [] -> Nothing
                        (entry : _) -> Just $ leHash entry
                    }
        liftIO $ writeIORef logRef (newEntry : currentLog)
        pure ()

    -- Lookup a resource by its hash
    GetResourceByHash hash -> do
        log <- liftIO $ readIORef logRef
        pure $ findResourceInLog hash log

-- | Interpret cryptographic operations
interpretCryptoOperation :: (Members '[Error AppError, Output String] r) => Sem (CryptoOperation ': r) a -> Sem r a
interpretCryptoOperation = interpret \case
    SignMessage privKey msg -> do
        output $ "Signing message with key: " <> show privKey
        pure $ Right $ Signature "TODO"
    VerifySignature pubKey msg sig -> do
        output $ "Verifying signature with key: " <> show pubKey
        pure True
    GenerateKeyPair seed -> do
        output $ "Generating key pair with seed: " <> show seed
        pure $ Right (PubKey "TODO", PrivKey "TODO")

-- | Interpret the transient storage effect
interpretTransientStorage :: (Members '[Trace, Embed IO, Output String, Error AppError] r) => IORef TransientDatastore -> Sem (TransientStorage ': r) a -> Sem r a
interpretTransientStorage storeRef = interpret \case
    StoreItem item -> do
        store <- liftIO $ readIORef storeRef
        let responsibleNodes = assignTimeBandits store (siKey item)
        output $ "Storing item with key " ++ show (siKey item) ++ " on nodes: " ++ show responsibleNodes
        pure $ Right ()
    RetrieveItem key -> do
        store <- liftIO $ readIORef storeRef
        let responsibleNodes = assignTimeBandits store key
        output $ "Retrieving item with key " ++ show key ++ " from nodes: " ++ show responsibleNodes
        throw $ StorageError (StorageFailure "Item retrieval not implemented")
    GetResponsibleNodes key -> do
        store <- liftIO $ readIORef storeRef
        pure $ assignTimeBandits store key
    UpdateDatastore newStore -> do
        output $ "Updating transient datastore with " ++ show (length $ tdTimeBandits newStore) ++ " nodes"
        liftIO $ writeIORef storeRef newStore

-- | Helper function to convert TimelineErrorType to AppError
timelineErrorToAppError :: TimelineErrorType -> AppError
timelineErrorToAppError = TimelineError

-- | Helper function to unwrap nested Either types
unwrapEither :: Either a (Either a b) -> Either a b
unwrapEither (Left err) = Left err
unwrapEither (Right (Left err)) = Left err
unwrapEither (Right (Right val)) = Right val

-- | Interpret the consolidated timeline effect
interpretTimelineEffect ::
    ( Members '[LogicalClock, Error AppError, Output String, Embed IO] r
    ) =>
    Sem (TimelineEffect ': r) a ->
    Sem r a
interpretTimelineEffect = interpret \case
    CreateNewTimeline th actor -> do
        result <- runError $ createNewTimeline th actor
        pure $ unwrapEither result
    MergeTimelines src dst -> do
        result <- runError $ mergeExistingTimelines src dst
        pure $ unwrapEither result
    RegisterEvent th event -> do
        output $ "Registering event for timeline: " <> show th
        timestamp <- send GetCurrentTime
        let metadata =
                EventMetadata
                    { emTimestamp = timestamp
                    , emCreatedAt = undefined -- TODO: Fix timestamp type
                    , emActor = undefined -- TODO: Get proper actor
                    , emTimeline = th
                    , emSignature = undefined -- TODO: Get proper signature
                    , emSigner = PubKey "TODO" -- TODO: Get proper signer
                    }
            entry =
                LogEntry
                    { leHash = computeSha256 $ encode event
                    , leContent = event
                    , leMetadata = metadata
                    , lePrevHash = Nothing -- TODO: Track previous events
                    }
        pure $ Right entry
    GetTimelineHistory th -> do
        result <- runError $ getTimelineEventHistory th
        pure $ unwrapEither result
    GetObjectHistory th objHash -> do
        output $ "Getting object history from timeline: " <> show th
        pure $ Right []
    GetEventsAfterMerkle th merkleRoot -> do
        output $ "Getting events after Merkle root: " <> show merkleRoot
        pure $ Right []
    GetPendingEvents th -> do
        output $ "Getting pending events for timeline: " <> show th
        pure $ Right []
    FinalizeTimelineBlock th events -> do
        result <- runError $ finalizeTimelineEvents th events
        pure $ unwrapEither result

-- | Interpret the bandit subscriptions effect
interpretBanditSubscriptions :: (Members '[Embed IO, Output String] r) => IORef [TimelineHash] -> Sem (BanditSubscriptions ': r) a -> Sem r a
interpretBanditSubscriptions subsRef = interpret \case
    GetSubscriptions -> do
        output "Getting current subscriptions"
        liftIO $ readIORef subsRef
    AddSubscription h -> do
        output $ "Adding subscription to timeline: " ++ show h
        liftIO $ atomicModifyIORef' subsRef $ \subs -> (h : subs, ())
    RemoveSubscription h -> do
        output $ "Removing subscription from timeline: " ++ show h
        liftIO $ atomicModifyIORef' subsRef $ \subs -> (filter (/= h) subs, ())
    SetSubscriptions hs -> do
        output $ "Setting subscriptions to: " ++ show hs
        liftIO $ writeIORef subsRef hs

-- | Interpret actor management operations
interpretActorManagement :: (Members '[Error ActorErrorType, Output String, Embed IO] r) => Sem (ActorManagement ': r) a -> Sem r a
interpretActorManagement = interpret \case
    CreateActor actorType -> do
        output $ "Creating actor of type: " <> show actorType
        pure $ Actor (EntityHash $ Hash "TODO") actorType
    GetActor hash -> do
        output $ "Getting actor: " <> show hash
        pure Nothing
    UpdateActor actor -> do
        output $ "Updating actor: " <> show (actorId actor)
        pure ()
    DeleteActor hash -> do
        output $ "Deleting actor: " <> show hash
        pure ()

-- | Create an authenticated message
semCreateMessage :: (Members '[CryptoOperation, Error AppError] r) => ByteString -> PrivKey -> Sem r (Either AppError (AuthenticatedMessage ByteString))
semCreateMessage msg privKey = do
    sig <- send $ SignMessage privKey msg
    case sig of
        Left err -> pure $ Left err
        Right signature -> do
            let actor = Actor (EntityHash $ Hash "TODO") Validator
                msgHash = computeMessageHash msg
                payload = ContentAddressedMessage msgHash msg
            pure $ Right $ AuthenticatedMessage msgHash actor Nothing payload signature

-- | Authenticate a message
semAuthenticateMessage :: (Members '[CryptoOperation, Error AppError] r) => AuthenticatedMessage ByteString -> Sem r (Either AppError Bool)
semAuthenticateMessage msg = do
    if verifyMessageSignature msg
        then pure $ Right True
        else pure $ Left (CryptoError InvalidSignatureError)

-- | Resource operations for managing timeline resources
class ResourceOps m where
    createResource :: ByteString -> ActorHash -> TimelineHash -> m (Either AppError Resource)
    transferResource :: Resource -> ActorHash -> TimelineHash -> m (Either AppError Resource)
    consumeResource :: Resource -> m (Either AppError ())
    verifyResource :: Resource -> m (Either AppError Bool)
    getResourceById :: ResourceHash -> m (Either AppError Resource)
    getResourcesByOwner :: ActorHash -> m (Either AppError [Resource])
    getResourcesByTimeline :: TimelineHash -> m (Either AppError [Resource])

    -- New unified transaction methods
    createUnifiedTransaction :: [Resource] -> [Resource] -> Actor -> TimelineHash -> m (Either AppError UnifiedResourceTransaction)
    validateTransaction :: UnifiedResourceTransaction -> m (Either AppError TransactionValidationResult)
    executeTransaction :: UnifiedResourceTransaction -> m (Either AppError [Resource])
    getTransactionHistory :: ResourceHash -> m (Either AppError [UnifiedResourceTransaction])

-- | Resource operations effect
data ResourceOperationEffect m a where
    ResourceOpCreateResource :: Resource -> ResourceOperationEffect m Resource
    ResourceOpUpdateResource :: Resource -> ResourceOperationEffect m Resource
    ResourceOpGetResourceById :: ResourceHash -> ResourceOperationEffect m (Either AppError Resource)
    ResourceOpGetResourcesByOwner :: ActorHash -> ResourceOperationEffect m [Resource]
    ResourceOpGetResourcesByTimeline :: TimelineHash -> ResourceOperationEffect m [Resource]
    ResourceOpGetUnifiedTransactionByHash :: Hash -> ResourceOperationEffect m (Either AppError UnifiedResourceTransaction)
    ResourceOpGetTransactionByOutput :: ResourceHash -> ResourceOperationEffect m (Either AppError UnifiedResourceTransaction)

makeSem ''ResourceOperationEffect

-- | Interpret the timeline resource effect
interpretResourceOp :: (Members '[Polysemy.State.State ResourceLog, Error AppError, Trace] r) => Sem (ResourceOperationEffect ': r) a -> Sem r a
interpretResourceOp = interpret \case
    ResourceOpCreateResource resource -> do
        -- Add resource to the log
        let event =
                ResourceEvent
                    { reContent = ResourceCreated resource
                    , reMetadata = undefined -- TODO: Fix this
                    , rePreviousEvent = Nothing
                    }
            eventHash = computeHash event Nothing
            entry =
                LogEntry
                    { leContent = ResourceCreated resource
                    , leMetadata = undefined -- TODO: Fix this
                    , leHash = eventHash
                    , lePrevHash = Nothing
                    }
        Polysemy.State.modify (\log -> entry : log)
        pure resource
    ResourceOpUpdateResource resource -> do
        -- Update resource in the log
        let event =
                ResourceEvent
                    { reContent = ResourceCreated resource -- Using ResourceCreated as a temporary fix
                    , reMetadata = undefined -- TODO: Fix this
                    , rePreviousEvent = Nothing
                    }
            eventHash = computeHash event Nothing
            entry =
                LogEntry
                    { leContent = ResourceCreated resource -- Using ResourceCreated as a temporary fix
                    , leMetadata = undefined -- TODO: Fix this
                    , leHash = eventHash
                    , lePrevHash = Nothing
                    }
        Polysemy.State.modify (\log -> entry : log)
        pure resource
    ResourceOpGetResourceById resourceHash -> do
        log <- Polysemy.State.get
        case findResourceInLog (unEntityHash resourceHash) log of
            Nothing ->
                pure $ Left $ ResourceError $ ResourceNotFound resourceHash
            Just resource ->
                pure $ Right resource
    ResourceOpGetResourcesByOwner ownerHash -> do
        log <- Polysemy.State.get
        let resources = [res | LogEntry{leContent = ResourceCreated res} <- log, resourceOwner res == ownerHash]
        pure resources
    ResourceOpGetResourcesByTimeline timelineHash -> do
        log <- Polysemy.State.get
        let resources = [res | LogEntry{leContent = ResourceCreated res} <- log, resourceOrigin res == timelineHash]
        pure resources
    ResourceOpGetUnifiedTransactionByHash txHash -> do
        log <- Polysemy.State.get
        case findUnifiedTransactionInLog txHash log of
            Nothing ->
                pure $ Left $ ResourceError $ ResourceNotFound (EntityHash $ Hash "Transaction not found")
            Just tx ->
                pure $ Right tx
    ResourceOpGetTransactionByOutput resourceHash -> do
        log <- Polysemy.State.get
        let transactions =
                [ tx | LogEntry{leContent = ResourceTransferred tx} <- log, any (\output -> resourceId (camContent output) == resourceHash) (urtOutputs tx)
                ]
        case listToMaybe transactions of
            Nothing ->
                pure $ Left $ ResourceError $ ResourceNotFound (EntityHash $ Hash "Transaction not found")
            Just tx ->
                pure $ Right tx

-- | Implement the ResourceOps class for the Sem monad
instance (Members '[ResourceOperationEffect, CryptoOperation, Error AppError, Trace] r) => ResourceOps (Sem r) where
    createResource metadata ownerHash timelineHash = do
        -- Create a new resource
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
        -- Add resource to the log
        createdResource <- send $ ResourceOpCreateResource resource
        pure $ Right createdResource

    transferResource resource newOwnerHash timelineHash = do
        -- Check if resource is already spent
        let resourceHash = resourceId resource
        existingResource <- send $ ResourceOpGetResourceById resourceHash
        case existingResource of
            Left err -> pure $ Left err
            Right foundResource ->
                if isJust (resourceSpentBy foundResource)
                    then pure $ Left $ ResourceError $ InvalidResourceState "Resource already spent"
                    else do
                        -- Create a new resource with new owner
                        let transferredResource =
                                resource
                                    { resourceOwner = newOwnerHash
                                    , resourceOrigin = timelineHash
                                    , resourceProvenanceChain = resourceProvenanceChain resource ++ [timelineHash]
                                    }
                        -- Mark original as spent
                        let spentResource = resource{resourceSpentBy = Just $ computeHash transferredResource Nothing}
                        _ <- send $ ResourceOpUpdateResource spentResource
                        updatedResource <- send $ ResourceOpCreateResource transferredResource
                        pure $ Right updatedResource

    consumeResource resource = do
        -- Check if resource is already spent
        let resourceHash = resourceId resource
        existingResource <- send $ ResourceOpGetResourceById resourceHash
        case existingResource of
            Left err -> pure $ Left err
            Right foundResource ->
                if isJust (resourceSpentBy foundResource)
                    then pure $ Left $ ResourceError $ InvalidResourceState "Resource already spent"
                    else do
                        -- Mark resource as spent with a placeholder transaction
                        let spentResource = resource{resourceSpentBy = Just $ Hash "Consumed"}
                        _ <- send $ ResourceOpUpdateResource spentResource
                        pure $ Right ()

{- | Demonstrate how to use the unified resource transaction model
This function creates a resource, transfers it to another actor, and then consumes it
-}
demonstrateUnifiedTransactionModel ::
    (Members '[ResourceOperationEffect, CryptoOperation, Error AppError, Trace] r) =>
    Actor ->
    Actor ->
    TimelineHash ->
    Sem r (Either AppError [Resource])
demonstrateUnifiedTransactionModel actor1 actor2 timeline = do
    -- 1. Create a resource owned by actor1
    let metadata = "1234" :: ByteString -- More idiomatic with OverloadedStrings

    -- Create a new resource
    let resId = EntityHash $ computeSha256 $ encode (metadata, actorId actor1, timeline)
        resource =
            Resource
                { resourceId = resId
                , resourceOrigin = timeline
                , resourceOwner = actorId actor1
                , resourceCapabilities = [Types.TransferCapability, Types.DelegateCapability]
                , resourceMeta = metadata
                , resourceSpentBy = Nothing
                , resourceParents = []
                , resourceTimestamp = LamportTime 0 -- TODO: Get proper timestamp
                , resourceProvenanceChain = [timeline]
                }
    createdResource <- send $ ResourceOpCreateResource resource

    -- 2. Transfer the resource to actor2
    -- Create a new resource with new owner
    let transferredResource =
            createdResource
                { resourceOwner = actorId actor2
                , resourceOrigin = timeline
                , resourceProvenanceChain = resourceProvenanceChain createdResource ++ [timeline]
                }
    -- Mark original as spent
    let spentResource = createdResource{resourceSpentBy = Just $ computeHash transferredResource Nothing}
    _ <- send $ ResourceOpUpdateResource spentResource
    updatedResource <- send $ ResourceOpCreateResource transferredResource

    -- 3. Create a transaction that consumes the resource and creates two new resources
    let outputResource1 = updatedResource{resourceMeta = "ABCD" :: ByteString} -- Using ByteString literals
        outputResource2 = updatedResource{resourceMeta = "EFGH" :: ByteString} -- Using ByteString literals

    -- Create the unified transaction
    txResult <- createUnifiedTransactionOp [updatedResource] [outputResource1, outputResource2] actor2 timeline (LamportTime 1) (PrivKey "TODO")
    case txResult of
        Left err -> pure $ Left err
        Right transaction -> do
            -- 4. Execute the transaction
            executeResult <- executeTransactionOp transaction
            case executeResult of
                Left err -> pure $ Left err
                Right outputResources -> pure $ Right outputResources
