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
Module: Types.Effect
Description: Core Effect type for the DAG model

This module defines the core Effect type that represents nodes in the effect DAG.
It depends on Types.EffectTypes and Types.EffectPayload for its basic structures,
but doesn't depend on any Core or Programs modules, making it suitable for use
by both Core and Programs modules without creating circular dependencies.

The Effect type is designed to:
- Be content-addressed (identifiable by hash)
- Track causal relationships between effects
- Record external facts that were observed during execution
- Support a wide variety of operations via the EffectPayload
-}
module Types.Effect 
  ( -- * Core Effect type
    Effect(..)

  -- * Effect Functions
  , createEffect
  , replayEffect
  , replayEffects
  , topoSortEffects
  
  -- * Re-exports for convenience
  , EffectId
  , FactSnapshot
  , EffectPayload(..)
  , emptyFactSnapshot
  , calculateEffectHash
  ) where

import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import GHC.Generics (Generic)
import Data.Graph (graphFromEdges, topSort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (foldl')

-- Import shared types
import Types.EffectTypes
  ( EffectId(..)
  , FactSnapshot
  , emptyFactSnapshot
  , calculateEffectHash
  )
import Types.EffectPayload (EffectPayload(..))
import Core.Common (Hash(..))

-- | The Effect type represents primitive operations in a program's lifecycle.
-- Each effect is a node in the causal DAG of program execution.
data Effect = Effect
  { effectId :: EffectId                     -- ^ Content-addressed ID of this effect
  , parentEffects :: [EffectId]              -- ^ Causal parent effects
  , payload :: EffectPayload                 -- ^ The actual effect operation
  , observedFacts :: FactSnapshot            -- ^ External facts observed during execution
  , effectTimestamp :: UTCTime               -- ^ When this effect was created
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new effect with the given payload, parents, and observed facts
createEffect :: EffectPayload -> [EffectId] -> FactSnapshot -> UTCTime -> Effect
createEffect payloadData parents facts timestamp =
  let effectWithoutId = Effect 
        { effectId = EffectId (Hash "placeholder")  -- Temporary placeholder
        , parentEffects = parents
        , payload = payloadData
        , observedFacts = facts
        , effectTimestamp = timestamp
        }
      -- Serialize the effect to a ByteString
      serialized = S.encode effectWithoutId
      -- Calculate the effect ID
      effectId' = EffectId (calculateEffectHash serialized [] (S.encode timestamp))
      -- Update the effect with the calculated ID
      effectWithId = effectWithoutId { effectId = effectId' }
  in effectWithId

-- | Replay effect dummy implementation (to be replaced by proper implementation)
-- This will be properly implemented in the Programs module
replayEffect :: a -> Effect -> (a, [Effect])
replayEffect state _ = (state, [])

-- | Replay a sequence of effects in causal order
-- This is the main entry point for effect replay
replayEffects :: a -> [Effect] -> a
replayEffects initialState effects =
  let sortedEffects = topoSortEffects effects
  in foldl' applyReplayedEffect initialState sortedEffects

-- | Apply a single effect during replay
applyReplayedEffect :: a -> Effect -> a
applyReplayedEffect state effect =
  let (newState, _) = replayEffect state effect
  in newState

-- | Topologically sort effects based on their parent-child relationships
-- This ensures that effects are executed in the correct causal order
topoSortEffects :: [Effect] -> [Effect]
topoSortEffects effects =
  -- Create edges for the graph: (effect, effectId, [parent effectIds])
  let edges = [(e, effectId e, parentEffects e) | e <- effects]
      
      -- Build a graph from the edges
      (graph, nodeFromVertex, _) = graphFromEdges edges
      
      -- Get the topologically sorted vertices
      sortedVertices = topSort graph
      
      -- Convert vertices back to effects
      sortedEffects = map ((\(e,_,_) -> e) . nodeFromVertex) sortedVertices
  in sortedEffects 