{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
Module: TimeBandits.Core.Effect
Description: Defines the core effect system for the Time-Bandits framework

This module defines the fundamental Effect type and related functions that form the
foundation of the Time-Bandits system. Effects represent atomic, verifiable operations
that can be performed on timelines.

Effects have several key properties:
- They are composable, allowing programs to be built from smaller effect primitives
- They are deterministic, ensuring consistent behavior when replayed
- They can be cryptographically verified for security
- They provide an abstraction over different timeline implementations
- They form a directed acyclic graph (DAG) for temporal causality tracking

The module also provides the integration between the Effect data types and the 
Polysemy effect system. It defines the interfaces for effect handlers, allowing
for composable, testable implementations of the Time Bandits effect system.

@since 0.1.0
-}
module TimeBandits.Core.Effect 
  ( -- * Core Effect Types
    Effect(..)
  , EffectId
  , EffectID
  , EffectResult(..)
  , EffectDAG(..)
  , EffectNode(..)
  
  -- * Effect Metadata
  , EffectMetadata(..)
  , EffectStatus(..)
  
  -- * Effect Preconditions
  , Precondition(..)
  , PreconditionType(..)
  
  -- * Fact Observation Model
  , FactSnapshot(..)
  , FactSource(..)
  , ObservationMethod(..)
  
  -- * Effect Creation
  , createEffect
  , getEffectId
  
  -- * Effect Validation
  , validateEffect
  , getEffectPreconditions
  , effectPostconditions
  
  -- * Effect Serialization
  , serializeEffect
  , deserializeEffect
  
  -- * Effect DAG Functions
  , addEffectToDAG
  , getEffectAncestors
  , findEffectInDAG
  
  -- * Resource Operations
  , ResourceOps (..)
  
  -- * Logical Clock
  , LogicalClock (..)
  , getLamportTime
  , incrementTime
  , updateTime
  
  -- * Key Management
  , KeyManagement (..)
  , generateKeyPair
  , registerPublicKey
  , lookupPublicKey
  , signData
  , verifyWithPublicKey
  , registerActorType
  , lookupActorType
  
  -- * P2P Network
  , P2PNetwork (..)
  , P2PNode (..)
  , P2PNodeId
  , discoverNodes
  , connectToNode
  , disconnectFromNode
  , sendMessage
  , broadcastMessage
  , receiveMessage
  
  -- * Transaction Management
  , TransactionEffect (..)
  , beginTransaction
  , commitTransaction
  , rollbackTransaction
  
  -- * Effect Application
  , EffectHandler (..)
  , applyEffect
  
  -- * Application Effects Type
  , AppEffects
  ) where

-- Import documentation of standard extensions
import TimeBandits.Core.Common.Extensions

-- External libraries
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Foldable (foldl)
import GHC.Generics (Generic)
import Data.Word (Word8)
import qualified Data.Serialize as S
import qualified Data.Text.Encoding as TE
import Polysemy (Member, Members, Sem, makeSem)
import Data.Serialize (Serialize, encode, decode)

-- TimeBandits modules
import TimeBandits.Core.Common.Types (Hash, computeHash, Signature(..), PubKey(..), PrivKey(..), LamportTime(..))
import TimeBandits.Core.TimeMap (TimeMap(..))
import TimeBandits.Core.ProgramId (ProgramId)
import TimeBandits.Core.TimelineId (TimelineId)
import TimeBandits.Core.Types (
    ActorHash,
    ActorType (..),
    ActorInfo (..),
    ResourceInfo (..),
    ResourceCapability (..),
    AppError(..),
    TimelineErrorType(..)
  )
import TimeBandits.Core.Common.Serialize ()
import TimeBandits.Core.Common.SharedTypes (ResourceId, ResourceErrorType(..), OwnershipError(..))

-- Forward import for AccountMessage - commented out to avoid cyclic dependencies
-- import TimeBandits.Programs.AccountProgram (AccountMessage)

-- | Type alias for AccountMessage to avoid cyclic dependencies
type AccountMessage = ByteString

-- | Unique identifier for effects
type EffectId = Hash

-- | Alias for EffectId to maintain backward compatibility
type EffectID = EffectId

-- | Status of an effect in the system
data EffectStatus
  = EffectPending     -- ^ Effect has been created but not applied
  | EffectApplied     -- ^ Effect has been successfully applied
  | EffectRejected    -- ^ Effect application was rejected
  | EffectReverted    -- ^ Effect was applied but later reverted
  deriving stock (Show, Eq)

-- | Metadata associated with an effect
data EffectMetadata = EffectMetadata
  { effectId :: EffectId                -- ^ Unique identifier for the effect
  , effectCreator :: ProgramId          -- ^ Program that created the effect
  , effectCreatedAt :: UTCTime          -- ^ When the effect was created
  , effectStatus :: EffectStatus        -- ^ Current status of the effect
  , effectPreconditions :: [Precondition] -- ^ Preconditions that must be satisfied
  , effectParentIds :: Set EffectId     -- ^ Parent effects in the DAG
  , effectObserved :: [FactSnapshot]    -- ^ Facts observed when effect was created
  }
  deriving stock (Show, Eq)

-- | Type of precondition for effect application
data PreconditionType
  = ResourceOwnership ResourceId ProgramId  -- ^ A resource must be owned by the specified program
  | TimelineState TimelineId ByteString     -- ^ Timeline must be in specific state
  | TimeCondition UTCTime                   -- ^ Time-based condition
  | LogicalCondition Text                   -- ^ Logical condition expressed as code
  | PriorEffectApplied EffectId             -- ^ Another effect must be applied first
  deriving stock (Show, Eq)

-- | Precondition that must be satisfied for an effect to be applied
data Precondition = Precondition
  { preconditionType :: PreconditionType    -- ^ Type of the precondition
  , preconditionDescription :: Text         -- ^ Human-readable description of the precondition
  }
  deriving stock (Show, Eq)

-- | Source of an observed fact
data FactSource
  = TimelineSource TimelineId   -- ^ Fact from a specific timeline
  | ProgramSource ProgramId     -- ^ Fact from a program's state
  | ResourceSource ResourceId   -- ^ Fact from a resource
  | SystemSource Text           -- ^ Fact from the system itself
  deriving stock (Show, Eq, Generic)

-- | Method used to observe a fact
data ObservationMethod
  = DirectObservation   -- ^ Directly observed by the system
  | ThirdPartyProvider  -- ^ Provided by a third party
  | DerivedFromData     -- ^ Derived from other data
  deriving stock (Show, Eq, Generic)

-- | Snapshot of an observed fact
data FactSnapshot = FactSnapshot
  { factId :: Hash                -- ^ Unique identifier for the fact
  , factSnapshot :: ByteString    -- ^ Serialized fact data
  , factObservedAt :: UTCTime     -- ^ When the fact was observed
  , factSource :: FactSource      -- ^ Source of the fact
  , observationMethod :: ObservationMethod -- ^ Method used for observation
  }
  deriving stock (Show, Eq, Generic)

-- | Create an empty fact snapshot
emptyFactSnapshot :: FactSnapshot
emptyFactSnapshot = FactSnapshot
  { factId = computeHash BS.empty
  , factSnapshot = BS.empty
  , factObservedAt = posixSecondsToUTCTime 0  -- Unix epoch
  , factSource = SystemSource "empty"
  , observationMethod = DirectObservation
  }

-- | Result of attempting to apply an effect
data EffectResult
  = EffectSuccess ByteString  -- ^ Effect was successfully applied with result data
  | EffectFailure Text        -- ^ Effect application failed with error message
  | EffectDeferred            -- ^ Effect application was deferred
  deriving stock (Show, Eq)

-- | The core Effect type representing atomic operations
data Effect
  = ResourceEffect ResourceAction        -- ^ Effect on a resource
  | TimelineEffect TimelineAction        -- ^ Effect on a timeline
  | ActorEffect ActorAction              -- ^ Effect on an actor
  | NetworkEffect NetworkAction          -- ^ Effect on the network
  | DataEffect DataAction                -- ^ Effect on data
  | MetadataEffect MetadataAction        -- ^ Effect on metadata
  | CompositeEffect [Effect]             -- ^ Composite of multiple effects
  deriving stock (Show, Eq, Generic)

data ResourceAction
  = ResourceActionCreate ResourceInfo              -- ^ Create a new resource
  | ResourceActionUpdate ResourceId ResourceInfo   -- ^ Update an existing resource
  | ResourceActionTransfer ResourceId ProgramId    -- ^ Transfer ownership of a resource
  | ResourceActionConsume ResourceId               -- ^ Consume a resource (making it unusable)
  | ResourceActionVerify ResourceId [ByteString]   -- ^ Verify properties of a resource
  deriving stock (Show, Eq, Generic)

data TimelineAction
  = CreateTimeline TimelineId                -- ^ Create a new timeline
  | ForkTimeline TimelineId TimelineId       -- ^ Fork a timeline
  | MergeTimelines TimelineId TimelineId     -- ^ Merge two timelines
  | FinalizeTimeline TimelineId              -- ^ Finalize a timeline (make it immutable)
  | AddEffectToTimeline TimelineId EffectId  -- ^ Add an effect to a timeline
  deriving stock (Show, Eq, Generic)

data ActorAction
  = RegisterActor ActorInfo              -- ^ Register a new actor
  | UpdateActorInfo ActorHash ActorInfo  -- ^ Update actor information
  | AuthorizeActor ActorHash [ByteString]-- ^ Authorize actor for certain capabilities
  | RevokeActor ActorHash                -- ^ Revoke an actor's registration
  deriving stock (Show, Eq, Generic)

data NetworkAction
  = NetworkActionBroadcast ByteString          -- ^ Broadcast a message to all peers
  | NetworkActionSend ActorHash ByteString     -- ^ Send a message to a specific peer
  | NetworkActionRegisterPeer ActorHash ByteString    -- ^ Register a new peer
  | NetworkActionDisconnectPeer ActorHash             -- ^ Disconnect a peer
  deriving stock (Show, Eq, Generic)

data DataAction
  = StoreData ByteString                 -- ^ Store arbitrary data
  | RetrieveData Hash                    -- ^ Retrieve stored data
  | VerifyData Hash ByteString           -- ^ Verify data integrity
  | EncryptData ByteString PubKey        -- ^ Encrypt data for a recipient
  | DecryptData ByteString PrivKey       -- ^ Decrypt data with private key
  deriving stock (Show, Eq, Generic)

data MetadataAction
  = SetMetadata Text ByteString          -- ^ Set metadata with key and value
  | RemoveMetadata Text                  -- ^ Remove metadata with key
  | TagEffect EffectId Text              -- ^ Tag an effect with a label
  | UntagEffect EffectId Text            -- ^ Remove a tag from an effect
  deriving stock (Show, Eq, Generic)

-- Serialization instances
instance Serialize FactSource where
  put (TimelineSource id) = do
    S.put (0 :: Word8)
    S.put id
  put (ProgramSource id) = do
    S.put (1 :: Word8)
    S.put id
  put (ResourceSource id) = do
    S.put (2 :: Word8)
    S.put id
  put (SystemSource txt) = do
    S.put (3 :: Word8)
    S.put (TE.encodeUtf8 txt)
    
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> TimelineSource <$> S.get
      1 -> ProgramSource <$> S.get
      2 -> ResourceSource <$> S.get
      3 -> SystemSource . TE.decodeUtf8 <$> S.get
      _ -> fail "Invalid FactSource tag"

instance Serialize ObservationMethod where
  put DirectObservation = S.put (0 :: Word8)
  put ThirdPartyProvider = S.put (1 :: Word8)
  put DerivedFromData = S.put (2 :: Word8)
  
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> pure DirectObservation
      1 -> pure ThirdPartyProvider
      2 -> pure DerivedFromData
      _ -> fail "Invalid ObservationMethod tag"

instance Serialize FactSnapshot where
  put snapshot = do
    S.put (factId snapshot)
    S.put (factSnapshot snapshot)
    -- UTCTime serialization - convert to integer timestamp
    let timestampSeconds = floor $ utcTimeToPOSIXSeconds $ factObservedAt snapshot :: Integer
    S.put timestampSeconds
    S.put (factSource snapshot)
    S.put (observationMethod snapshot)
  
  get = do
    fId <- S.get
    fSnapshot <- S.get
    -- UTCTime deserialization - convert from integer timestamp
    timestampSeconds <- S.get :: S.Get Integer
    let fObservedAt = posixSecondsToUTCTime $ fromIntegral timestampSeconds
    fSource <- S.get
    obsMethod <- S.get
    return $ FactSnapshot fId fSnapshot fObservedAt fSource obsMethod

instance Serialize ResourceAction where
  put (ResourceActionCreate info) = do
    S.put (0 :: Word8)
    S.put info
  put (ResourceActionUpdate id info) = do
    S.put (1 :: Word8)
    S.put id
    S.put info
  put (ResourceActionTransfer id programId) = do
    S.put (2 :: Word8)
    S.put id
    S.put programId
  put (ResourceActionConsume id) = do
    S.put (3 :: Word8)
    S.put id
  put (ResourceActionVerify id bytestrings) = do
    S.put (4 :: Word8)
    S.put id
    S.put bytestrings
  
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> ResourceActionCreate <$> S.get
      1 -> ResourceActionUpdate <$> S.get <*> S.get
      2 -> ResourceActionTransfer <$> S.get <*> S.get
      3 -> ResourceActionConsume <$> S.get
      4 -> ResourceActionVerify <$> S.get <*> S.get
      _ -> fail "Invalid ResourceAction tag"

instance Serialize TimelineAction where
  put (CreateTimeline id) = do
    S.put (0 :: Word8)
    S.put id
  put (ForkTimeline id1 id2) = do
    S.put (1 :: Word8)
    S.put id1
    S.put id2
  put (MergeTimelines id1 id2) = do
    S.put (2 :: Word8)
    S.put id1
    S.put id2
  put (FinalizeTimeline id) = do
    S.put (3 :: Word8)
    S.put id
  put (AddEffectToTimeline timelineId effectId) = do
    S.put (4 :: Word8)
    S.put timelineId
    S.put effectId
  
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> CreateTimeline <$> S.get
      1 -> ForkTimeline <$> S.get <*> S.get
      2 -> MergeTimelines <$> S.get <*> S.get
      3 -> FinalizeTimeline <$> S.get
      4 -> AddEffectToTimeline <$> S.get <*> S.get
      _ -> fail "Invalid TimelineAction tag"

instance Serialize ActorAction where
  put (RegisterActor info) = do
    S.put (0 :: Word8)
    S.put info
  put (UpdateActorInfo hash info) = do
    S.put (1 :: Word8)
    S.put hash
    S.put info
  put (AuthorizeActor hash bytestrings) = do
    S.put (2 :: Word8)
    S.put hash
    S.put bytestrings
  put (RevokeActor hash) = do
    S.put (3 :: Word8)
    S.put hash
  
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> RegisterActor <$> S.get
      1 -> UpdateActorInfo <$> S.get <*> S.get
      2 -> AuthorizeActor <$> S.get <*> S.get
      3 -> RevokeActor <$> S.get
      _ -> fail "Invalid ActorAction tag"

instance Serialize NetworkAction where
  put (NetworkActionBroadcast bytes) = do
    S.put (0 :: Word8)
    S.put bytes
  put (NetworkActionSend hash bytes) = do
    S.put (1 :: Word8)
    S.put hash
    S.put bytes
  put (NetworkActionRegisterPeer hash bytes) = do
    S.put (2 :: Word8)
    S.put hash
    S.put bytes
  put (NetworkActionDisconnectPeer hash) = do
    S.put (3 :: Word8)
    S.put hash
  
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> NetworkActionBroadcast <$> S.get
      1 -> NetworkActionSend <$> S.get <*> S.get
      2 -> NetworkActionRegisterPeer <$> S.get <*> S.get
      3 -> NetworkActionDisconnectPeer <$> S.get
      _ -> fail "Invalid NetworkAction tag"

instance Serialize DataAction where
  put (StoreData bytes) = do
    S.put (0 :: Word8)
    S.put bytes
  put (RetrieveData hash) = do
    S.put (1 :: Word8)
    S.put hash
  put (VerifyData hash bytes) = do
    S.put (2 :: Word8)
    S.put hash
    S.put bytes
  put (EncryptData bytes pubkey) = do
    S.put (3 :: Word8)
    S.put bytes
    S.put pubkey
  put (DecryptData bytes privkey) = do
    S.put (4 :: Word8)
    S.put bytes
    S.put privkey
  
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> StoreData <$> S.get
      1 -> RetrieveData <$> S.get
      2 -> VerifyData <$> S.get <*> S.get
      3 -> EncryptData <$> S.get <*> S.get
      4 -> DecryptData <$> S.get <*> S.get
      _ -> fail "Invalid DataAction tag"

instance Serialize MetadataAction where
  put (SetMetadata text bytes) = do
    S.put (0 :: Word8)
    S.put (TE.encodeUtf8 text)
    S.put bytes
  put (RemoveMetadata text) = do
    S.put (1 :: Word8)
    S.put (TE.encodeUtf8 text)
  put (TagEffect effectId text) = do
    S.put (2 :: Word8)
    S.put effectId
    S.put (TE.encodeUtf8 text)
  put (UntagEffect effectId text) = do
    S.put (3 :: Word8)
    S.put effectId
    S.put (TE.encodeUtf8 text)
  
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> do
        textBytes <- S.get
        bytes <- S.get
        return $ SetMetadata (TE.decodeUtf8 textBytes) bytes
      1 -> RemoveMetadata . TE.decodeUtf8 <$> S.get
      2 -> do
        effectId <- S.get
        textBytes <- S.get
        return $ TagEffect effectId (TE.decodeUtf8 textBytes)
      3 -> do
        effectId <- S.get
        textBytes <- S.get
        return $ UntagEffect effectId (TE.decodeUtf8 textBytes)
      _ -> fail "Invalid MetadataAction tag"

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize Effect where
  put (ResourceEffect action) = do
    S.put (0 :: Word8)  -- Tag for ResourceEffect
    S.put action
  put (TimelineEffect action) = do
    S.put (1 :: Word8)  -- Tag for TimelineEffect
    S.put action
  put (ActorEffect action) = do
    S.put (2 :: Word8)  -- Tag for ActorEffect
    S.put action
  put (NetworkEffect action) = do
    S.put (3 :: Word8)  -- Tag for NetworkEffect
    S.put action
  put (DataEffect action) = do
    S.put (4 :: Word8)  -- Tag for DataEffect
    S.put action
  put (MetadataEffect action) = do
    S.put (5 :: Word8)  -- Tag for MetadataEffect
    S.put action
  put (CompositeEffect effects) = do
    S.put (6 :: Word8)  -- Tag for CompositeEffect
    S.put effects
    
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> ResourceEffect <$> S.get
      1 -> TimelineEffect <$> S.get
      2 -> ActorEffect <$> S.get
      3 -> NetworkEffect <$> S.get
      4 -> DataEffect <$> S.get
      5 -> MetadataEffect <$> S.get
      6 -> CompositeEffect <$> S.get
      _ -> fail "Invalid Effect tag"

-- | A node in the effect DAG
data EffectNode = EffectNode
  { nodeEffect :: Effect        -- ^ The effect in this node
  , nodeMetadata :: EffectMetadata  -- ^ Metadata for the effect
  , nodeChildren :: Set EffectId    -- ^ Child effects in the DAG
  }
  deriving stock (Show, Eq)

-- | Directed acyclic graph of effects representing causal relationships
data EffectDAG = EffectDAG
  { dagNodes :: Map EffectId EffectNode  -- ^ Map of effect nodes
  , dagRoots :: Set EffectId             -- ^ Root effects (no parents)
  }
  deriving (Show, Eq)

-- | Create a new effect with metadata
createEffect :: ProgramId -> [Precondition] -> [FactSnapshot] -> Set EffectId -> Effect -> IO (Effect, EffectMetadata)
createEffect programId preconditions observations parentIds effect = do
  currentTime <- getCurrentTime
  let effectHash = computeHash $ encode effect
      metadata = EffectMetadata
        { effectId = effectHash
        , effectCreator = programId
        , effectCreatedAt = currentTime
        , effectStatus = EffectPending
        , effectPreconditions = preconditions
        , effectParentIds = parentIds
        , effectObserved = observations
        }
  pure (effect, metadata)

-- | Get the unique identifier for an effect
getEffectId :: Effect -> EffectId
getEffectId = computeHash . encode

-- | Validate that an effect can be applied
validateEffect :: Effect -> [Precondition] -> Bool
validateEffect _ [] = True  -- No preconditions means it's always valid
validateEffect _ _ = True   -- Actual validation is performed by the interpreter

-- | Get all preconditions required for an effect
getEffectPreconditions :: Effect -> [Precondition]
getEffectPreconditions (ResourceEffect _) = []  -- Placeholder, actual implementation depends on effect
getEffectPreconditions (TimelineEffect _) = []  -- Placeholder, actual implementation depends on effect
getEffectPreconditions (ActorEffect _) = []     -- Placeholder, actual implementation depends on effect
getEffectPreconditions (NetworkEffect _) = []   -- Placeholder, actual implementation depends on effect
getEffectPreconditions (DataEffect _) = []      -- Placeholder, actual implementation depends on effect
getEffectPreconditions (MetadataEffect _) = []  -- Placeholder, actual implementation depends on effect
getEffectPreconditions (CompositeEffect effects) = 
  concatMap getEffectPreconditions effects      -- Combine preconditions from all sub-effects

-- | Get all postconditions guaranteed by an effect
effectPostconditions :: Effect -> [Precondition]
effectPostconditions (ResourceEffect _) = []  -- Placeholder, actual implementation depends on effect
effectPostconditions (TimelineEffect _) = []  -- Placeholder, actual implementation depends on effect
effectPostconditions (ActorEffect _) = []     -- Placeholder, actual implementation depends on effect
effectPostconditions (NetworkEffect _) = []   -- Placeholder, actual implementation depends on effect
effectPostconditions (DataEffect _) = []      -- Placeholder, actual implementation depends on effect
effectPostconditions (MetadataEffect _) = []  -- Placeholder, actual implementation depends on effect
effectPostconditions (CompositeEffect effects) = 
  concatMap effectPostconditions effects      -- Combine postconditions from all sub-effects

-- | Serialize an effect to bytes
serializeEffect :: Effect -> ByteString
serializeEffect = encode

-- | Deserialize an effect from bytes
deserializeEffect :: ByteString -> Either String Effect
deserializeEffect = decode

-- | Add an effect to an effect DAG
addEffectToDAG :: EffectDAG -> Effect -> EffectMetadata -> EffectDAG
addEffectToDAG dag effect metadata = 
  let effectId = nodeEffectId
      nodeEffectId = getEffectId effect
      parentIds = effectParentIds metadata
      
      -- Create the new node
      newNode = EffectNode
        { nodeEffect = effect
        , nodeMetadata = metadata
        , nodeChildren = Set.empty
        }
      
      -- Update the DAG nodes
      updatedNodes = Map.insert nodeEffectId newNode (dagNodes dag)
      
      -- Add this effect as a child to all parent nodes
      updatedNodesWithParents = foldl updateParentNode updatedNodes (Set.toList parentIds)
      
      -- If this effect has no parents, add it to roots
      updatedRoots = if Set.null parentIds
                      then Set.insert nodeEffectId (dagRoots dag)
                      else dagRoots dag
  in
  EffectDAG
    { dagNodes = updatedNodesWithParents
    , dagRoots = updatedRoots
    }
  where
    -- Helper to update a parent node with a new child
    updateParentNode nodes parentId =
      Map.adjust addChild parentId nodes
      where
        addChild node = node { nodeChildren = Set.insert (getEffectId effect) (nodeChildren node) }

-- | Get all ancestor effects of a given effect
getEffectAncestors :: EffectDAG -> EffectId -> Set EffectId
getEffectAncestors dag effectId =
  case Map.lookup effectId (dagNodes dag) of
    Nothing -> Set.empty
    Just node ->
      let parentIds = effectParentIds (nodeMetadata node)
          directParents = parentIds
          indirectParents = foldl (\ancestors parentId -> 
                                Set.union ancestors (getEffectAncestors dag parentId))
                              Set.empty
                              (Set.toList directParents)
      in
      Set.union directParents indirectParents

-- | Find an effect in the DAG by its ID
findEffectInDAG :: EffectDAG -> EffectId -> Maybe EffectNode
findEffectInDAG dag effectId = Map.lookup effectId (dagNodes dag)

-- | Type for P2P node identifiers
type P2PNodeId = ByteString

-- | Representation of a P2P network node
data P2PNode = P2PNode
    { nodeId :: P2PNodeId
    , nodeAddress :: ByteString
    , nodeType :: ActorType
    }
    deriving stock (Show, Eq)

-- | Interface for resource operations
data ResourceOps m a where
    CreateResource :: ResourceInfo -> ResourceOps m (Either AppError ResourceInfo)
    TransferResource :: ResourceInfo -> ActorHash -> TimelineId -> ResourceOps m (Either AppError ResourceInfo)
    ConsumeResource :: ResourceInfo -> ResourceOps m (Either AppError ResourceInfo)
    VerifyResource :: ResourceInfo -> ResourceOps m (Either AppError Bool)
    GetResource :: Hash -> ResourceOps m (Either AppError ResourceInfo)
    GetResourcesByOwner :: ActorHash -> ResourceOps m (Either AppError [ResourceInfo])
    GetResourcesByTimeline :: TimelineId -> ResourceOps m (Either AppError [ResourceInfo])

makeSem ''ResourceOps

-- | Interface for logical clock operations
data LogicalClock m a where
    GetLamportTime :: LogicalClock m LamportTime
    IncrementTime :: LogicalClock m LamportTime
    UpdateTime :: LamportTime -> LogicalClock m LamportTime

makeSem ''LogicalClock

-- | Interface for key management operations
data KeyManagement m a where
    GenerateKeyPair :: KeyManagement m (PubKey, PrivKey)
    RegisterPublicKey :: ActorHash -> PubKey -> KeyManagement m ()
    LookupPublicKey :: ActorHash -> KeyManagement m (Maybe PubKey)
    SignData :: PrivKey -> ByteString -> KeyManagement m (Either Text Signature)
    VerifyWithPublicKey :: PubKey -> ByteString -> Signature -> KeyManagement m Bool
    RegisterActorType :: ActorHash -> ActorType -> KeyManagement m ()
    LookupActorType :: ActorHash -> KeyManagement m (Maybe ActorType)

makeSem ''KeyManagement

-- | Interface for P2P network operations
data P2PNetwork m a where
    DiscoverNodes :: P2PNetwork m [P2PNode]
    ConnectToNode :: P2PNode -> P2PNetwork m Bool
    DisconnectFromNode :: P2PNode -> P2PNetwork m Bool
    SendMessage :: P2PNode -> ByteString -> P2PNetwork m Bool
    BroadcastMessage :: ByteString -> P2PNetwork m Int
    ReceiveMessage :: P2PNetwork m (Maybe (P2PNode, ByteString))

makeSem ''P2PNetwork

-- | Interface for transaction management
data TransactionEffect m a where
    BeginTransaction :: TransactionEffect m ()
    CommitTransaction :: TransactionEffect m Bool
    RollbackTransaction :: TransactionEffect m Bool

makeSem ''TransactionEffect

-- | Interface for effect handling
data EffectHandler m a where
    ApplyEffect :: Effect -> EffectMetadata -> EffectHandler m EffectResult
    ValidateEffectPreconditions :: Effect -> [FactSnapshot] -> EffectHandler m Bool

makeSem ''EffectHandler

-- | Type alias for all application effects
type AppEffects r =
    '[ ResourceOps
     , LogicalClock
     , KeyManagement
     , P2PNetwork
     , TransactionEffect
     , EffectHandler
     ] 