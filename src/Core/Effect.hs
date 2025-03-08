{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: Core.Effect
Description: Defines the core effect system that enables time-travel operations

This module defines the fundamental Effect type and related functions that form the
foundation of the Time-Bandits system. Effects represent atomic, verifiable operations
that can be performed on timelines.

Effects have several key properties:
- They are composable, allowing programs to be built from smaller effect primitives
- They are deterministic, ensuring consistent behavior when replayed
- They can be cryptographically verified for security
- They provide an abstraction over different timeline implementations
- They form a directed acyclic graph (DAG) for temporal causality tracking

The Effect type is used throughout the system as the primary building block for
programs that operate across timelines.
-}
module Core.Effect 
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
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize, encode, decode)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Foldable (foldl)
import GHC.Generics (Generic)
import Data.Word (Word8)
import qualified Data.Serialize as S
import qualified Data.Text.Encoding as TE

-- Import from Core modules only
import Core.Common (Hash, computeHash)
import Core.TimeMap (TimeMap(..))
import Core.ProgramId (ProgramId)
import Core.ResourceId (ResourceId)
import Core.TimelineId (TimelineId)
import Core.ActorId (ActorId)
import Core.AccountProgram (AccountMessage)

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
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

-- | Type of precondition for effect application
data PreconditionType
  = ResourceOwnership ResourceId ProgramId  -- ^ A resource must be owned by the specified program
  | TimelineState TimelineId ByteString     -- ^ Timeline must be in specific state
  | TimeCondition UTCTime                   -- ^ Time-based condition
  | LogicalCondition Text                   -- ^ Logical condition expressed as code
  | PriorEffectApplied EffectId             -- ^ Another effect must be applied first
  | FactObserved FactSnapshot               -- ^ A specific fact must have been observed
  deriving (Show, Eq, Generic)

-- | Precondition that must be satisfied for an effect to be applied
data Precondition = Precondition
  { preconditionType :: PreconditionType  -- ^ Type of precondition
  , preconditionDescription :: Text       -- ^ Human-readable description
  }
  deriving (Show, Eq, Generic)

-- | Source of an observed fact
data FactSource
  = TimelineSource TimelineId   -- ^ Fact from a specific timeline
  | ProgramSource ProgramId     -- ^ Fact from a program's state
  | ResourceSource ResourceId   -- ^ Fact from a resource
  | SystemSource Text           -- ^ Fact from the system itself
  deriving (Show, Eq, Generic)

-- | Method used to observe a fact
data ObservationMethod
  = DirectObservation     -- ^ Fact was directly observed by the effect creator
  | IndirectObservation   -- ^ Fact was indirectly observed via another effect
  | ProvenObservation     -- ^ Fact was observed with cryptographic proof
  | ConsensusObservation  -- ^ Fact was observed via consensus mechanism
  deriving (Show, Eq, Generic)

-- | Snapshot of an observed fact
data FactSnapshot = FactSnapshot
  { factSource :: FactSource           -- ^ Where the fact was observed
  , factContent :: ByteString          -- ^ Content of the fact
  , factHash :: Hash                   -- ^ Hash of the fact content
  , factObservedAt :: UTCTime          -- ^ When the fact was observed
  , factMethod :: ObservationMethod    -- ^ How the fact was observed
  , factTimeMap :: TimeMap             -- ^ TimeMap at the moment of observation
  }
  deriving (Show, Eq, Generic)

-- | Result of attempting to apply an effect
data EffectResult
  = EffectSuccess ByteString              -- ^ Effect was successfully applied with result data
  | EffectFailure Text                    -- ^ Effect application failed with error message
  | EffectDeferred UTCTime                -- ^ Effect will be applied at a later time
  deriving (Show, Eq, Generic)

-- | The core Effect type representing atomic operations
data Effect
  = ResourceEffect 
      { effectResourceId :: ResourceId
      , effectResourceData :: ByteString
      }  -- ^ Effect on a resource
      
  | TimelineEffect 
      { effectTimelineId :: TimelineId
      , effectTimelineData :: ByteString
      }  -- ^ Effect on a timeline
      
  | ProgramEffect 
      { effectProgramId :: ProgramId
      , effectProgramData :: ByteString
      }  -- ^ Effect on a program
      
  | AccountMessageEffect 
      { effectActorId :: ActorId
      , effectMessage :: AccountMessage
      }  -- ^ Message to an account program
      
  | DepositEffect
      { depositResourceId :: ResourceId
      , depositAmount :: Integer
      , depositToProgramId :: ProgramId
      }  -- ^ Deposit resource to a program
      
  | WithdrawEffect
      { withdrawResourceId :: ResourceId
      , withdrawAmount :: Integer
      , withdrawFromProgramId :: ProgramId
      }  -- ^ Withdraw resource from a program
      
  | TransferEffect
      { transferResourceId :: ResourceId
      , transferAmount :: Integer
      , transferFromProgramId :: ProgramId
      , transferToProgramId :: ProgramId
      }  -- ^ Transfer resource between programs
      
  | InvokeEffect
      { invokeTargetProgramId :: ProgramId
      , invokeEntrypoint :: Text
      , invokeArguments :: [ByteString]
      }  -- ^ Invoke another program
      
  | ReceiveCallbackEffect
      { callbackFromProgramId :: ProgramId
      , callbackPayload :: ByteString
      }  -- ^ Receive callback from another program
      
  | InternalStateEffect
      { stateKey :: Text
      , stateValue :: ByteString
      }  -- ^ Update internal program state
      
  | CompositeEffect 
      { subEffects :: [Effect]
      }  -- ^ Composition of multiple effects
  deriving (Show, Eq, Generic)

-- Manual Serialize instance to avoid Text serialization ambiguity
instance Serialize Effect where
  put (ResourceEffect resId data') = do
    S.put (0 :: Word8)  -- Tag for ResourceEffect
    S.put resId
    S.put data'
  put (TimelineEffect tlId data') = do
    S.put (1 :: Word8)  -- Tag for TimelineEffect
    S.put tlId
    S.put data'
  put (ProgramEffect progId data') = do
    S.put (2 :: Word8)  -- Tag for ProgramEffect
    S.put progId
    S.put data'
  put (AccountMessageEffect actorId msg) = do
    S.put (3 :: Word8)  -- Tag for AccountMessageEffect
    S.put actorId
    S.put msg
  put (DepositEffect resId amt progId) = do
    S.put (4 :: Word8)  -- Tag for DepositEffect
    S.put resId
    S.put amt
    S.put progId
  put (WithdrawEffect resId amt progId) = do
    S.put (5 :: Word8)  -- Tag for WithdrawEffect
    S.put resId
    S.put amt
    S.put progId
  put (TransferEffect resId amt fromId toId) = do
    S.put (6 :: Word8)  -- Tag for TransferEffect
    S.put resId
    S.put amt
    S.put fromId
    S.put toId
  put (InvokeEffect progId entrypoint args) = do
    S.put (7 :: Word8)  -- Tag for InvokeEffect
    S.put progId
    S.put (TE.encodeUtf8 entrypoint)
    S.put args
  put (ReceiveCallbackEffect progId payload) = do
    S.put (8 :: Word8)  -- Tag for ReceiveCallbackEffect
    S.put progId
    S.put payload
  put (InternalStateEffect key value) = do
    S.put (9 :: Word8)  -- Tag for InternalStateEffect
    S.put (TE.encodeUtf8 key)
    S.put value
  put (CompositeEffect effects) = do
    S.put (10 :: Word8)  -- Tag for CompositeEffect
    S.put effects
    
  get = do
    tag <- S.get :: S.Get Word8
    case tag of
      0 -> ResourceEffect <$> S.get <*> S.get
      1 -> TimelineEffect <$> S.get <*> S.get
      2 -> ProgramEffect <$> S.get <*> S.get
      3 -> AccountMessageEffect <$> S.get <*> S.get
      4 -> DepositEffect <$> S.get <*> S.get <*> S.get
      5 -> WithdrawEffect <$> S.get <*> S.get <*> S.get
      6 -> TransferEffect <$> S.get <*> S.get <*> S.get <*> S.get
      7 -> do
        progId <- S.get
        entrypointBs <- S.get
        args <- S.get
        return $ InvokeEffect progId (TE.decodeUtf8 entrypointBs) args
      8 -> ReceiveCallbackEffect <$> S.get <*> S.get
      9 -> do
        keyBs <- S.get
        value <- S.get
        return $ InternalStateEffect (TE.decodeUtf8 keyBs) value
      10 -> CompositeEffect <$> S.get
      _ -> fail "Invalid Effect tag"

-- | A node in the effect DAG
data EffectNode = EffectNode
  { nodeEffect :: Effect            -- ^ The effect itself
  , nodeMetadata :: EffectMetadata  -- ^ Metadata for the effect
  , nodeChildren :: Set EffectId    -- ^ Child effects in the DAG
  }
  deriving (Show, Eq, Generic)

-- | Directed acyclic graph of effects representing causal relationships
data EffectDAG = EffectDAG
  { dagNodes :: Map EffectId EffectNode  -- ^ Map of effect nodes
  , dagRoots :: Set EffectId             -- ^ Root effects (no parents)
  }
  deriving (Show, Eq, Generic)

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
getEffectPreconditions (ResourceEffect _ _) = []  -- Placeholder, actual implementation depends on effect
getEffectPreconditions (TimelineEffect _ _) = []  -- Placeholder, actual implementation depends on effect
getEffectPreconditions (ProgramEffect _ _) = []   -- Placeholder, actual implementation depends on effect
getEffectPreconditions (CompositeEffect effects) = 
  concatMap getEffectPreconditions effects        -- Combine preconditions from all sub-effects
getEffectPreconditions _ = []  -- Placeholder for other effect types

-- | Get all postconditions guaranteed by an effect
effectPostconditions :: Effect -> [Precondition]
effectPostconditions (ResourceEffect _ _) = []  -- Placeholder, actual implementation depends on effect
effectPostconditions (TimelineEffect _ _) = []  -- Placeholder, actual implementation depends on effect
effectPostconditions (ProgramEffect _ _) = []   -- Placeholder, actual implementation depends on effect
effectPostconditions (CompositeEffect effects) = 
  concatMap effectPostconditions effects        -- Combine postconditions from all sub-effects
effectPostconditions _ = []  -- Placeholder for other effect types

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