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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}

{- |
Module: Execution.PreconditionEvaluator
Description: Evaluates effect preconditions against the current time map and resource ledger

This module provides the PreconditionEvaluator, which centralizes all precondition
checks for effects in the Time Bandits system. It ensures that all preconditions
are evaluated consistently across the system.

The PreconditionEvaluator:
1. Evaluates preconditions against the current time map and resource ledger
2. Provides a unified interface for precondition checking
3. Ensures consistent evaluation of preconditions
4. Supports formal verification of precondition schemas
-}
module Execution.PreconditionEvaluator 
  ( -- * Core Types
    PreconditionEvaluator(..)
  , PreconditionResult(..)
  , PreconditionError(..)
  , ProposedEffect(..)
  
  -- * Evaluator Operations
  , createEvaluator
  , evaluatePreconditions
  , checkPrecondition
  
  -- * Precondition Types
  , checkResourceOwnership
  , checkResourceBalance
  , checkTimeMapConsistency
  , checkExternalFacts
  ) where

import Prelude hiding (show)
import qualified Prelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Core.ResourceId (ResourceId)
import Core.ProgramId (ProgramId)
import Core.Effect (Effect, EffectId)
import Core.TimeMap (TimeMap)
import Core.ResourceLedger (ResourceLedger, ResourceState)

-- | Result of evaluating preconditions
data PreconditionResult =
    PreconditionsSatisfied
  | PreconditionsNotSatisfied [PreconditionError]
  deriving (Eq, Show, Generic)

-- | Error types for precondition evaluation
data PreconditionError =
    ResourceNotOwned ResourceId ProgramId
  | InsufficientBalance ResourceId Integer Integer  -- ^ ResourceId, required, actual
  | TimeMapInconsistency T.Text
  | ExternalFactChanged T.Text
  | InvalidPrecondition T.Text
  deriving (Eq, Show, Generic)

-- | A proposed effect with its observed time map
data ProposedEffect = ProposedEffect
  { effect :: Effect
  , observedTimeMap :: TimeMap
  , readSet :: [ResourceId]
  , writeSet :: [ResourceId]
  }
  deriving (Eq, Show, Generic)

-- | Placeholder for timeline adapter
data TimelineAdapter = TimelineAdapter
  { timelineId :: Text
  , queryBalance :: ProgramId -> ResourceId -> IO Integer
  , verifyProof :: BS.ByteString -> IO Bool
  }

-- | Function to convert TimelineAdapter to String for display
timelineAdapterToString :: TimelineAdapter -> String
timelineAdapterToString adapter = "TimelineAdapter { timelineId = " ++ Prelude.show (timelineId adapter) ++ " }"

-- | PreconditionEvaluator evaluates effect preconditions
data PreconditionEvaluator = PreconditionEvaluator
  { timelineAdapters :: Map.Map T.Text TimelineAdapter  -- ^ Adapters for external timelines
  }

-- | Function to convert PreconditionEvaluator to String for display
preconditionEvaluatorToString :: PreconditionEvaluator -> String
preconditionEvaluatorToString _ = "PreconditionEvaluator { ... }"

-- | Create a new precondition evaluator
createEvaluator :: [TimelineAdapter] -> PreconditionEvaluator
createEvaluator adapters =
  let adapterMap = Map.fromList [(timelineId adapter, adapter) | adapter <- adapters]
  in PreconditionEvaluator
    { timelineAdapters = adapterMap
    }

-- | Evaluate all preconditions for a proposed effect
evaluatePreconditions :: 
  (MonadIO m) => 
  PreconditionEvaluator -> 
  ProposedEffect -> 
  ResourceLedger -> 
  TimeMap ->  -- ^ Current time map
  m PreconditionResult
evaluatePreconditions evaluator proposedEffect ledger currentTimeMap = do
  -- Check resource ownership
  ownershipResult <- checkResourceOwnership evaluator proposedEffect ledger
  
  -- Check resource balances
  balanceResult <- checkResourceBalance evaluator proposedEffect ledger
  
  -- Check time map consistency
  timeMapResult <- checkTimeMapConsistency evaluator proposedEffect currentTimeMap
  
  -- Check external facts
  externalFactsResult <- checkExternalFacts evaluator proposedEffect currentTimeMap
  
  -- Combine all results
  let errors = concat
        [ case ownershipResult of
            PreconditionsSatisfied -> []
            PreconditionsNotSatisfied errs -> errs
        , case balanceResult of
            PreconditionsSatisfied -> []
            PreconditionsNotSatisfied errs -> errs
        , case timeMapResult of
            PreconditionsSatisfied -> []
            PreconditionsNotSatisfied errs -> errs
        , case externalFactsResult of
            PreconditionsSatisfied -> []
            PreconditionsNotSatisfied errs -> errs
        ]
  
  -- Return combined result
  pure $ if null errors
    then PreconditionsSatisfied
    else PreconditionsNotSatisfied errors

-- | Check a specific precondition
checkPrecondition :: 
  (MonadIO m) => 
  PreconditionEvaluator -> 
  ProposedEffect -> 
  ResourceLedger -> 
  TimeMap -> 
  T.Text ->  -- ^ Precondition type
  m PreconditionResult
checkPrecondition evaluator proposedEffect ledger currentTimeMap preconditionType =
  case preconditionType of
    "ownership" -> checkResourceOwnership evaluator proposedEffect ledger
    "balance" -> checkResourceBalance evaluator proposedEffect ledger
    "timeMap" -> checkTimeMapConsistency evaluator proposedEffect currentTimeMap
    "externalFacts" -> checkExternalFacts evaluator proposedEffect currentTimeMap
    _ -> pure $ PreconditionsNotSatisfied [InvalidPrecondition $ "Unknown precondition type: " <> preconditionType]

-- | Check that the program owns all resources in the write set
checkResourceOwnership :: 
  (MonadIO m) => 
  PreconditionEvaluator -> 
  ProposedEffect -> 
  ResourceLedger -> 
  m PreconditionResult
checkResourceOwnership _ proposedEffect ledger = do
  -- In a real implementation, this would check that the program owns all resources
  -- in the write set
  pure PreconditionsSatisfied

-- | Check that the program has sufficient balance for all resources
checkResourceBalance :: 
  (MonadIO m) => 
  PreconditionEvaluator -> 
  ProposedEffect -> 
  ResourceLedger -> 
  m PreconditionResult
checkResourceBalance _ proposedEffect ledger = do
  -- In a real implementation, this would check that the program has sufficient
  -- balance for all resources
  pure PreconditionsSatisfied

-- | Check that the observed time map is consistent with the current time map
checkTimeMapConsistency :: 
  (MonadIO m) => 
  PreconditionEvaluator -> 
  ProposedEffect -> 
  TimeMap -> 
  m PreconditionResult
checkTimeMapConsistency _ proposedEffect currentTimeMap = do
  -- In a real implementation, this would check that the observed time map is
  -- consistent with the current time map
  pure PreconditionsSatisfied

-- | Check that external facts (balances, proofs) are still valid
checkExternalFacts :: 
  (MonadIO m) => 
  PreconditionEvaluator -> 
  ProposedEffect -> 
  TimeMap -> 
  m PreconditionResult
checkExternalFacts _ proposedEffect currentTimeMap = do
  -- In a real implementation, this would check that external facts (balances, proofs)
  -- are still valid
  pure PreconditionsSatisfied 