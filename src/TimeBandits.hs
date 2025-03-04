{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: TimeBandits
Description: Main module for the Time-Bandits library.
This module re-exports functionality from all Time-Bandits components.
-}
module TimeBandits 
  ( -- * Core Exports
    module TimeBandits.Core
  
    -- * Program-related Exports
  , module TimeBandits.Programs.Program
  , module TimeBandits.Programs.ProgramEffect
  , module TimeBandits.Programs.PreconditionEvaluator
  , module TimeBandits.Programs.Scenario
  
    -- * Actor-related Exports
  , module TimeBandits.Actors.Actor
  , module TimeBandits.Actors.TimeTraveler
  , module TimeBandits.Actors.TimeKeeper
  , module TimeBandits.Actors.TimeBandit
  
    -- * Execution-related Exports
  , module TimeBandits.Execution.EffectInterpreter
  , module TimeBandits.Execution.EffectExecutor
  , module TimeBandits.Execution.ExecutionLog
  
    -- * Adapter-related Exports
  , module TimeBandits.Adapters.TimelineAdapter
  
    -- * Proof-related Exports
  , module TimeBandits.Proofs.ZKProof
  , module TimeBandits.Proofs.TimelineProof
  
    -- * CLI-related Exports
  , module TimeBandits.CLI.Controller
  ) where

-- Core imports
import TimeBandits.Core

-- Program imports
import TimeBandits.Programs.Program
import TimeBandits.Programs.ProgramEffect
import TimeBandits.Programs.PreconditionEvaluator
import TimeBandits.Programs.Scenario

-- Actor imports
import TimeBandits.Actors.Actor
import TimeBandits.Actors.TimeTraveler
import TimeBandits.Actors.TimeKeeper
import TimeBandits.Actors.TimeBandit

-- Execution imports
import TimeBandits.Execution.EffectInterpreter
import TimeBandits.Execution.EffectExecutor
import TimeBandits.Execution.ExecutionLog

-- Adapter imports
import TimeBandits.Adapters.TimelineAdapter

-- Proof imports
import TimeBandits.Proofs.ZKProof
import TimeBandits.Proofs.TimelineProof

-- CLI imports
import TimeBandits.CLI.Controller 