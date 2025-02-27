{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

    -- * Capability Typeclasses
    TimelineOps (..),
    ResourceOps (..),
    ActorOps (..),
    CryptoOps (..),
    StorageOps (..),

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

    -- * Message Effect Functions
    semCreateMessage,
    semAuthenticateMessage,

    -- * Helper Functions
    handleLogging,
    handleErrors,
    handleCryptoOp,
    handleHopStep,
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

    -- * Timeline Management
    TimelineManagement (..),
    interpretTimelineManagement,

    -- * Event Management
    EventManagement (..),
    interpretEventManagement,
) where

import Control.Monad ()
import Crypto.Error ()
import Crypto.Hash.SHA256 qualified as SHA256
import Crypto.PubKey.Ed25519 ()
import Data.ByteArray ()
import Data.ByteString ()
import Data.ByteString.Builder ()
import Data.IORef ()
import Data.List (union)
import Data.Map.Strict qualified as Map
import Data.Maybe ()
import Data.Ord ()
import Data.Serialize (encode)
import Data.Text ()
import Polysemy (Embed, Member, Members, Sem, interpret, makeSem, runM, send)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Output (Output, output, runOutputList)
import Polysemy.Trace (Trace, trace, traceToStdout)
import TimeBandits.Core (computeMessageHash, computeSha256)
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
    ResourceTransaction (..),
    Signature (..),
    StorageErrorType (..),
    TimelineBlock (..),
    TimelineErrorType (..),
    TimelineEventType (..),
    TimelineHash,
    TimelineLog (..),
    TransientDatastore (..),
    TransientStoredItem (..),
    Trie (..),
    elemsTrie,
    emTimestamp,
    resourceId,
 )
import TimeBandits.Types qualified as Types
import Prelude hiding (trace)

-- | Logical clock for tracking causal ordering in timelines
data LogicalClock m a where
    GetCurrentTime :: LogicalClock m LamportTime
    IncrementTime :: LogicalClock m LamportTime
    UpdateTime :: LamportTime -> LogicalClock m LamportTime

-- | Timeline operations effect
data TimelineOps m a where
    CreateTimeline :: TimelineHash -> ActorHash -> TimelineOps m (Either TimelineErrorType TimelineLog)
    MergeTimelines :: TimelineHash -> TimelineHash -> TimelineOps m (Either TimelineErrorType TimelineLog)
    GetTimelineHistory :: TimelineHash -> TimelineOps m (Either TimelineErrorType [LogEntry EventContent])
    FinalizeEvents :: TimelineHash -> [LogEntry EventContent] -> TimelineOps m (Either TimelineErrorType TimelineBlock)

-- | Error conversion helper functions
asTimelineError :: (Member (Error AppError) r) => TimelineErrorType -> Sem r a
asTimelineError = throw . TimelineError

asResourceError :: (Member (Error AppError) r) => ResourceErrorType -> Sem r a
asResourceError = throw . ResourceError

asActorError :: (Member (Error AppError) r) => ActorErrorType -> Sem r a
asActorError = throw . ActorError

asCryptoError :: (Member (Error AppError) r) => CryptoErrorType -> Sem r a
asCryptoError = throw . CryptoError

asStorageError :: (Member (Error AppError) r) => StorageErrorType -> Sem r a
asStorageError = throw . StorageError

-- | Resource transfer operation
transferResourceOp :: (Member (Error AppError) r, Member LogicalClock r) => Resource -> ActorHash -> TimelineHash -> LamportTime -> [TimelineHash] -> Sem r (Either AppError (Resource, ResourceEvent))
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
    SendAuthenticatedMessage :: AuthenticatedMessage ByteString -> TimelineMessage m ()
    BroadcastAuthenticatedMessage :: AuthenticatedMessage ByteString -> TimelineMessage m ()
    ReceiveAuthenticatedMessage :: TimelineMessage m (AuthenticatedMessage ByteString)

-- | Resource operations for managing timeline resources
data TimelineResource m a where
    GetResourceTime :: TimelineResource m LamportTime
    LogResourceEvent :: ResourceEvent -> TimelineResource m ()
    GetResourceByHash :: Hash -> TimelineResource m (Maybe Resource)
    GetTransactionByHash :: Hash -> TimelineResource m (Maybe ResourceTransaction)

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

-- | Timeline management effect
data TimelineManagement m a where
    CreateNewTimeline :: TimelineHash -> ActorHash -> TimelineManagement m (Either TimelineErrorType TimelineLog)
    MergeTimelinesWith :: TimelineHash -> TimelineHash -> TimelineManagement m (Either TimelineErrorType TimelineLog)
    GetTimelineEvents :: TimelineHash -> TimelineManagement m (Either TimelineErrorType [LogEntry EventContent])
    FinalizeTimelineBlock :: TimelineHash -> [LogEntry EventContent] -> TimelineManagement m (Either TimelineErrorType TimelineBlock)

-- | Event management effect
data EventManagement m a where
    RegisterTimelineEvent :: TimelineHash -> EventContent -> EventManagement m (Either TimelineErrorType (LogEntry EventContent))
    GetObjectEventHistory :: TimelineHash -> Hash -> EventManagement m (Either TimelineErrorType [LogEntry EventContent])
    GetEventsAfterMerkle :: TimelineHash -> Hash -> EventManagement m (Either TimelineErrorType [LogEntry EventContent])
    GetPendingEvents :: TimelineHash -> EventManagement m (Either TimelineErrorType [LogEntry EventContent])

-- | Cryptographic operations effect
data CryptoOperation m a where
    SignMessage :: PrivKey -> ByteString -> CryptoOperation m (Either AppError Signature)
    VerifySignature :: PubKey -> ByteString -> Signature -> CryptoOperation m Bool
    GenerateKeyPair :: ByteString -> CryptoOperation m (Either AppError (PubKey, PrivKey))

-- | The core effect stack for the Time Bandits application
type AppEffects r =
    '[ BanditSubscriptions
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
    ResourceConsumed tx -> hash `elem` rtInputs tx || any ((== hash) . resourceId) (rtOutputs tx)
    ResourceCapabilityChecked{rcCheckedResource = checkedHash} -> checkedHash == hash

-- | Helper function to compute a transaction's hash
computeTransactionHash :: ResourceTransaction -> Hash
computeTransactionHash tx = Hash $ SHA256.hash $ encode (rtInputs tx, rtOutputs tx, rtTimestamp tx, rtSigner tx)

-- | Root timeline hash for actor events
rootTimelineHash :: TimelineHash
rootTimelineHash = EntityHash $ Hash "root-timeline" -- TODO: Use proper genesis hash

-- | Empty trie for initialization
emptyTrie :: Trie a
emptyTrie = Trie Map.empty

-- | Operator for Maybe to Either conversion with error
(?!>) :: (Member (Error e) r) => Sem r (Maybe a) -> e -> Sem r a
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
validateEventSequence :: (Member (Error TimelineErrorType) r) => [LogEntry EventContent] -> Sem r ()
validateEventSequence [] = pure ()
validateEventSequence [_] = pure ()
validateEventSequence (e1 : e2 : es) = do
    when (emTimestamp (leMetadata e1) >= emTimestamp (leMetadata e2)) $
        throw $
            InvalidTimelineState "Events not in chronological order"
    validateEventSequence (e2 : es)

-- | Validate event timestamps
validateEventTimestamps :: (Member (Error TimelineErrorType) r) => LamportTime -> [LogEntry EventContent] -> Sem r ()
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
validateEvents :: (Member (Error TimelineErrorType) r) => TimelineLog -> [LogEntry EventContent] -> Sem r ()
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
checkTimelineExists :: (Member (Embed IO) r) => TimelineHash -> Sem r Bool
checkTimelineExists th = do
    -- TODO: Implement actual storage check
    pure False

-- | Get a timeline log from storage
getTimelineLog :: (Member (Embed IO) r) => TimelineHash -> Sem r (Maybe TimelineLog)
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
storeTimelineLog :: (Member (Embed IO) r) => TimelineHash -> TimelineLog -> Sem r ()
storeTimelineLog th log = do
    -- TODO: Implement actual storage
    pure ()

-- | Validate that two timelines can be merged
validateMerge :: (Member (Error TimelineErrorType) r) => TimelineLog -> TimelineLog -> Sem r ()
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
    srcLog <- getTimelineLog src ?!> TimelineNotFound src
    dstLog <- getTimelineLog dst ?!> TimelineNotFound dst
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
    timelineLog <- getTimelineLog th ?!> TimelineNotFound th
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
    timelineLog <- getTimelineLog th ?!> TimelineNotFound th
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

-- | Resource management capability
class (Monad m) => ResourceOps m where
    -- | Allocate a new resource
    allocateResource :: Resource -> ActorHash -> m (Either ResourceErrorType ResourceHash)

    -- | Transfer resource ownership
    transferResource :: ResourceHash -> ActorHash -> ActorHash -> m (Either ResourceErrorType Resource)

    -- | Get resource history
    getResourceHistory :: ResourceHash -> m (Either ResourceErrorType ResourceLog)

-- | Actor management capability
class (Monad m) => ActorOps m where
    -- | Register a new actor
    registerActor :: ActorType -> PubKey -> m (Either ActorErrorType Actor)

    -- | Update actor role
    updateActorRole :: ActorHash -> ActorType -> m (Either ActorErrorType Actor)

    -- | Get actor history
    getActorHistory :: ActorHash -> m (Either ActorErrorType [LogEntry EventContent])

-- | Cryptographic operations capability
class (Monad m) => CryptoOps m where
    -- | Sign a message
    signMessage :: PrivKey -> ByteString -> m (Either CryptoErrorType Signature)

    -- | Verify a signature
    verifySignature :: PubKey -> ByteString -> Signature -> m (Either CryptoErrorType Bool)

    -- | Generate a new key pair
    generateKeyPair :: m (Either CryptoErrorType (PubKey, PrivKey))

-- | Storage operations capability
class (Monad m) => StorageOps m where
    -- | Store an item
    storeItem :: TransientStoredItem -> m (Either StorageErrorType Hash)

    -- | Retrieve an item
    retrieveItem :: Hash -> m (Either StorageErrorType TransientStoredItem)

    -- | Delete an item
    deleteItem :: Hash -> m (Either StorageErrorType ())

-- | Actor management effect
data ActorManagement m a where
    CreateActor :: ActorType -> ActorManagement m Actor
    GetActor :: ActorHash -> ActorManagement m (Maybe Actor)
    UpdateActor :: Actor -> ActorManagement m ()
    DeleteActor :: ActorHash -> ActorManagement m ()

makeSem ''ActorManagement

-- | Handle logging operations
handleLogging :: (Member (Output String) r) => String -> Sem r ()
handleLogging = output

-- | Handle error operations
handleErrors :: (Member (Error AppError) r) => AppError -> Sem r a
handleErrors = throw

-- | Handle cryptographic operations
handleCryptoOp :: (Member CryptoOperation r) => CryptoOperation (Sem r) a -> Sem r a
handleCryptoOp = send

-- | Handle hop step operations
handleHopStep :: (Members '[LogicalClock, Error AppError] r) => Sem r a -> Sem r a
handleHopStep action = do
    _ <- send IncrementTime
    action

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

{- | Interpret the complete application effect stack into a final IO action.
This interpreter composes all other interpreters in the correct order to
ensure proper effect handling and type safety.

The result type provides:
* Error handling via 'Either AppError'
* Collected logs via '[String]'
* The final computation result 'a'
-}
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
        $ interpretCryptoOperation
        $ interpretLogicalClock timeRef
        $ interpretAtomicTransaction
        $ interpretTimeout
        $ interpretTimelineProof
        $ interpretTimelineMessage
        $ interpretTimelineResource logRef
        $ interpretTransientStorage storeRef
        $ interpretBanditSubscriptions subsRef action

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
interpretAtomicTransaction :: (Member (Output String) r) => Sem (AtomicTransaction ': r) a -> Sem r a
interpretAtomicTransaction = interpret $ \case
    BeginTransaction -> output "Transaction begun"
    CommitTransaction -> output "Transaction committed"
    RollbackTransaction -> output "Transaction rolled back"

-- | Interpret the timeout effect
interpretTimeout :: (Member (Output String) r) => Sem (Timeout ': r) a -> Sem r a
interpretTimeout = interpret $ \case
    ScheduleTimeout t -> output $ "Timeout scheduled for: " ++ show t
    CancelTimeout t -> output $ "Timeout cancelled for: " ++ show t

-- | Interpret the timeline proof effect
interpretTimelineProof :: (Member (Output String) r, Member (Error AppError) r) => Sem (TimelineProof ': r) a -> Sem r a
interpretTimelineProof = interpret $ \case
    GenerateProof h -> do
        output $ "Generating proof for hash: " ++ show h
        pure h -- TODO: Implement actual proof generation
    VerifyProof h p -> do
        output $ "Verifying proof for hash: " ++ show h
        pure True -- TODO: Implement actual proof verification

-- | Interpret the timeline messaging effect
interpretTimelineMessage :: (Member (Output String) r, Member (Error AppError) r) => Sem (TimelineMessage ': r) a -> Sem r a
interpretTimelineMessage = interpret $ \case
    SendAuthenticatedMessage msg -> do
        output $ "Sending authenticated message to: " ++ maybe "broadcast" show (amDestination msg)
    -- TODO: Implement actual network sending
    BroadcastAuthenticatedMessage msg -> do
        output "Broadcasting authenticated message to all nodes"
    -- TODO: Implement actual network broadcasting
    ReceiveAuthenticatedMessage -> do
        output "Waiting for authenticated message"
        -- TODO: Implement actual message receiving
        throw $ NetworkError "Message receiving not implemented"

-- | Helper function to create a new log entry
createLogEntry :: ResourceEvent -> ResourceLog -> LogEntry ResourceEventType
createLogEntry event currentLog =
    LogEntry
        { leHash = computeSha256 $ encode event
        , leContent = reContent event
        , leMetadata = reMetadata event
        , lePrevHash = case currentLog of
            [] -> Nothing
            (entry : _) -> Just $ leHash entry
        }

-- | Helper function to find a resource by its hash in the log
findResourceInLog :: Hash -> ResourceLog -> Maybe Resource
findResourceInLog hash log =
    listToMaybe [res | LogEntry{leContent = ResourceCreated res} <- log, Types.unEntityHash (resourceId res) == hash]

-- | Helper function to find a transaction by its hash in the log
findTransactionInLog :: Hash -> ResourceLog -> Maybe ResourceTransaction
findTransactionInLog hash log =
    listToMaybe [tx | LogEntry{leContent = ResourceConsumed tx} <- log, computeMessageHash tx == hash]

-- | Interpret the timeline resource effect
interpretTimelineResource ::
    ( Members '[Trace, Embed IO, LogicalClock, Output String, Error AppError] r
    ) =>
    IORef ResourceLog ->
    Sem (TimelineResource ': r) a ->
    Sem r a
interpretTimelineResource logRef = interpret \case
    -- Get the current resource time from the logical clock
    GetResourceTime -> do
        trace "Getting current logical time for resource"
        send GetCurrentTime

    -- Log a new resource event
    LogResourceEvent event -> do
        trace "Logging new resource event"
        currentLog <- liftIO $ readIORef logRef
        let newEntry = createLogEntry event currentLog
        liftIO $ writeIORef logRef (newEntry : currentLog)
        pure ()

    -- Lookup a resource by its hash
    GetResourceByHash hash -> do
        trace $ "Looking up resource with hash: " <> show hash
        log <- liftIO $ readIORef logRef
        pure $ findResourceInLog hash log

    -- Lookup a transaction by its hash
    GetTransactionByHash hash -> do
        trace $ "Looking up transaction with hash: " <> show hash
        log <- liftIO $ readIORef logRef
        pure $ findTransactionInLog hash log

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

-- | Interpret event management
interpretEventManagement ::
    ( Members '[TimelineOps, LogicalClock, Error TimelineErrorType, Output String] r
    ) =>
    EventManagement m a ->
    Sem r a
interpretEventManagement = \case
    RegisterTimelineEvent th event -> do
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
    GetObjectEventHistory th objHash -> do
        output $ "Getting object history from timeline: " <> show th
        pure $ Right []
    GetEventsAfterMerkle th merkleRoot -> do
        output $ "Getting events after Merkle root: " <> show merkleRoot
        pure $ Right []
    GetPendingEvents th -> do
        output $ "Getting pending events for timeline: " <> show th
        pure $ Right []

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
            let msgHash = Hash "TODO" -- TODO: Compute actual hash
                actor = Actor (EntityHash $ Hash "TODO") Validator
                payload = ContentAddressedMessage msgHash msg
            pure $ Right $ AuthenticatedMessage msgHash actor Nothing payload signature

-- | Authenticate a message
semAuthenticateMessage :: (Members '[CryptoOperation, Error AppError] r) => AuthenticatedMessage ByteString -> Sem r (Either AppError Bool)
semAuthenticateMessage msg = do
    let content = camContent $ amPayload msg
        sig = amSignature msg
    result <- send $ VerifySignature (PubKey "TODO") content sig -- TODO: Get proper public key from actor
    if result
        then pure $ Right True
        else pure $ Left (CryptoError InvalidSignatureError)

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

-- | Interpret timeline management using TimelineOps
interpretTimelineManagement ::
    ( Members '[TimelineOps, LogicalClock, Error TimelineErrorType, Output String] r
    ) =>
    TimelineManagement m a ->
    Sem r a
interpretTimelineManagement = \case
    CreateNewTimeline th actor -> send $ CreateTimeline th actor
    MergeTimelinesWith src dst -> send $ MergeTimelines src dst
    GetTimelineEvents th -> send $ GetTimelineHistory th
    FinalizeTimelineBlock th events -> send $ FinalizeEvents th events

-- | Interpret timeline operations
interpretTimelineOps ::
    ( Members '[LogicalClock, Error TimelineErrorType, Output String, Embed IO] r
    ) =>
    Sem (TimelineOps ': r) a ->
    Sem r a
interpretTimelineOps = interpret \case
    CreateTimeline th actor -> createNewTimeline th actor
    MergeTimelines src dst -> mergeExistingTimelines src dst
    GetTimelineHistory th -> getTimelineEventHistory th
    FinalizeEvents th events -> finalizeTimelineEvents th events
