{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: TimeBandits
Description: Main module for the Time-Bandits library.
This module provides access to all Time-Bandits components.

Note: To avoid name conflicts, this module does not re-export the contents
of the submodules. Instead, import the specific modules directly.
-}
module TimeBandits 
  ( -- * Core Modules
    -- $core
    
    -- * Program Modules
    -- $programs
    
    -- * Actor Modules
    -- $actors
    
    -- * Execution Modules
    -- $execution
    
    -- * Adapter Modules
    -- $adapters
    
    -- * Proof Modules
    -- $proofs
    
    -- * Simulation Modules
    -- $simulation
    
    -- * CLI Modules
    -- $cli
  ) where

-- $core
-- Core modules provide the fundamental types and functions for the Time Bandits system.
-- 
-- * "Core" - Main module re-exporting all Core functionality
-- * "Core.Common" - Common types and utilities
-- * "Core.Timeline" - Timeline-related types and functions
-- * "Core.Resource" - Resource management
-- * "Core.ActorId" - Actor identification
-- * "Core.ProgramId" - Program identification
-- * "Core.Types" - Core type definitions

-- $programs
-- Program modules provide the program model and execution environment.
--
-- * "Programs" - Main module re-exporting all Program functionality
-- * "Programs.Program" - Program definition and execution
-- * "Programs.AccountProgram" - Account program implementation
-- * "Programs.ProgramState" - Program state management
-- * "Programs.Types" - Program-specific types

-- $actors
-- Actor modules provide the actor model and communication.
--
-- * "Actors" - Main module re-exporting all Actor functionality
-- * "Actors.Actor" - Actor definition and behavior
-- * "Actors.ActorCommunication" - Actor communication protocols

-- $execution
-- Execution modules provide the execution environment for effects.
--
-- * "Execution" - Main module re-exporting all Execution functionality
-- * "Execution.EffectInterpreter" - Effect interpretation
-- * "Execution.ExecutionLog" - Execution logging
-- * "Execution.ResourceLedger" - Resource ledger management

-- $adapters
-- Adapter modules provide adapters for external systems.
--
-- * "Adapters" - Main module re-exporting all Adapter functionality
-- * "Adapters.TimelineAdapter" - Timeline adapter
-- * "Adapters.MockAdapter" - Mock adapter for testing

-- $proofs
-- Proof modules provide verification and proof generation.
--
-- * "Proofs" - Main module re-exporting all Proof functionality
-- * "Proofs.SecurityVerifier" - Security verification
-- * "Proofs.TimelineProof" - Timeline proof generation
-- * "Proofs.ZKProof" - Zero-knowledge proof generation

-- $simulation
-- Simulation modules provide simulation capabilities.
--
-- * "Simulation" - Main module re-exporting all Simulation functionality
-- * "Simulation.Controller" - Simulation controller
-- * "Simulation.Messaging" - Simulation messaging
-- * "Simulation.Scenario" - Simulation scenarios

-- $cli
-- CLI modules provide command-line interface capabilities.
--
-- * "CLI" - Main module re-exporting all CLI functionality
-- * "CLI.Controller" - CLI controller
-- * "CLI.Deployment" - CLI deployment 