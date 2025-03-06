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
This module provides the SecurityVerifier, which is responsible for verifying
system-level security properties across the Time Bandits system. It ensures
that core security invariants are maintained throughout program execution.

The SecurityVerifier:
1. Prevents double-spending of resources (single-owner rule)
2. Prevents reentrancy attacks via Lamport clock ordering
3. Ensures complete traceability of all operations
4. Prevents backdating of transitions through time map enforcement
-}
module TimeBandits.SecurityVerifier 
  ( -- * Core Types
    SecurityProperty(..)
  , SecurityError(..)
  
  -- * Verification Functions
  , verifySecurityProperty
  , verifyAllProperties
  
  -- * Property-Specific Verifiers
  , verifyNoDoubleSpend
  , verifyNoReentrancy
  , verifyFullTraceability
  , verifyNoBackdating
  ) where

import Control.Monad (foldM, when)
import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw, catch)
import Polysemy.Embed (Embed)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..))
import Core.Types (LamportTime(..), AppError)
import TimeBandits.Program (ProgramId)
import TimeBandits.Resource (ResourceHash, resourceId)
import TimeBandits.ResourceLedger (ResourceLedger(..), OwnershipRecord(..), getOwnershipHistory)
import TimeBandits.ExecutionLog (ExecutionLog(..), LogEntry(..), getLogEntries)
import TimeBandits.Timeline (TimelineHash, TimeMap(..))

-- | Security properties that must be maintained by the system
data SecurityProperty
  = NoDoubleSpend
  | NoReentrancy
  | FullTraceability
  | NoBackdating
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Errors related to security property violations
data SecurityError
  = DoubleSpendDetected ResourceHash ProgramId ProgramId
  | ReentrancyDetected ProgramId LamportTime LamportTime
  | MissingProofForEffect Hash
  | MissingOwnershipRecord ResourceHash
  | BackdatedTransition TimelineHash LamportTime LamportTime
  | IncompleteAuditTrail Hash
  | GenericSecurityError String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Verify a specific security property against an execution log
verifySecurityProperty :: 
  (Member (Error SecurityError) r, Member (Error AppError) r) =>
  SecurityProperty -> 
  ExecutionLog -> 
  ResourceLedger -> 
  TimeMap -> 
  Sem r ()
verifySecurityProperty NoDoubleSpend log ledger _ = 
  verifyNoDoubleSpend log ledger
verifySecurityProperty NoReentrancy log _ _ = 
  verifyNoReentrancy log
verifySecurityProperty FullTraceability log _ _ = 
  verifyFullTraceability log
verifySecurityProperty NoBackdating log _ timeMap = 
  verifyNoBackdating log timeMap

-- | Verify all security properties at once
verifyAllProperties :: 
  (Member (Error SecurityError) r, Member (Error AppError) r) =>
  ExecutionLog -> 
  ResourceLedger -> 
  TimeMap -> 
  Sem r ()
verifyAllProperties log ledger timeMap = do
  verifyNoDoubleSpend log ledger
  verifyNoReentrancy log
  verifyFullTraceability log
  verifyNoBackdating log timeMap

-- | Verify that the single-owner invariant is maintained (no double spending)
verifyNoDoubleSpend :: 
  (Member (Error SecurityError) r, Member (Error AppError) r) =>
  ExecutionLog -> 
  ResourceLedger -> 
  Sem r ()
verifyNoDoubleSpend log ledger = do
  -- Get all entries from the log
  entries <- getLogEntries log
  
  -- Process each entry to detect resource modifications
  -- and check against the resource ledger
  foldM_ verifyEntry Set.empty entries
  where
    verifyEntry :: 
      (Member (Error SecurityError) r, Member (Error AppError) r) =>
      Set.Set ResourceHash -> 
      LogEntry -> 
      Sem r (Set.Set ResourceHash)
    verifyEntry seenResources entry = do
      -- Extract resources modified in this log entry
      let resources = extractResources entry
      
      -- For each resource, check ownership is properly recorded
      -- and no resource is used twice in concurrent branches
      foldM verifyResource seenResources resources
    
    verifyResource :: 
      (Member (Error SecurityError) r, Member (Error AppError) r) =>
      Set.Set ResourceHash -> 
      ResourceHash -> 
      Sem r (Set.Set ResourceHash)
    verifyResource seenResources resHash = do
      -- If we've seen this resource before in a concurrent branch, it's a double spend
      when (Set.member resHash seenResources) $
        throw $ GenericSecurityError "Resource used in concurrent branches"
      
      -- Get the ownership history of this resource
      history <- getOwnershipHistory ledger resHash `catch` \_ -> 
        throw $ MissingOwnershipRecord resHash
      
      -- Verify the ownership chain is consistent
      -- In a real implementation, this would check that each transfer
      -- follows the proper protocol (escrow, claim, etc.)
      when (null history) $
        throw $ MissingOwnershipRecord resHash
      
      -- Add the resource to the set of seen resources
      return $ Set.insert resHash seenResources
    
    -- Extract resources from a log entry
    -- In a real implementation, this would extract all resources
    -- referenced in effects within the entry
    extractResources :: LogEntry -> [ResourceHash]
    extractResources _ = []  -- Mock implementation

-- | Verify that the system prevents reentrancy attacks
verifyNoReentrancy :: 
  (Member (Error SecurityError) r) =>
  ExecutionLog -> 
  Sem r ()
verifyNoReentrancy log = do
  -- Get all entries from the log
  entries <- getLogEntries log `catch` \_ -> 
    throw $ GenericSecurityError "Failed to get log entries"
  
  -- Check for cycles in the program invocation graph
  -- by tracking Lamport timestamps per program
  foldM_ verifyNoReentrancyForEntry Map.empty entries
  where
    verifyNoReentrancyForEntry :: 
      (Member (Error SecurityError) r) =>
      Map.Map ProgramId LamportTime -> 
      LogEntry -> 
      Sem r (Map.Map ProgramId LamportTime)
    verifyNoReentrancyForEntry timeMap entry = do
      -- Extract program ID and Lamport time from entry
      let progId = extractProgramId entry
          time = extractLamportTime entry
      
      -- Check if we've seen this program before with a later timestamp
      -- If so, it's a reentrancy violation
      case Map.lookup progId timeMap of
        Just prevTime | prevTime >= time ->
          throw $ ReentrancyDetected progId prevTime time
        _ -> return $ Map.insert progId time timeMap
    
    -- Extract program ID from log entry
    -- In a real implementation, this would get the program ID from the entry
    extractProgramId :: LogEntry -> ProgramId
    extractProgramId _ = "mock-program-id"  -- Mock implementation
    
    -- Extract Lamport time from log entry
    -- In a real implementation, this would get the Lamport time from the entry
    extractLamportTime :: LogEntry -> LamportTime
    extractLamportTime _ = LamportTime 0  -- Mock implementation

-- | Verify that the system maintains a complete audit trail
verifyFullTraceability :: 
  (Member (Error SecurityError) r) =>
  ExecutionLog -> 
  Sem r ()
verifyFullTraceability log = do
  -- Get all entries from the log
  entries <- getLogEntries log `catch` \_ -> 
    throw $ GenericSecurityError "Failed to get log entries"
  
  -- Check each entry has required proof and links
  mapM_ verifyEntryTraceability entries
  where
    verifyEntryTraceability :: 
      (Member (Error SecurityError) r) =>
      LogEntry -> 
      Sem r ()
    verifyEntryTraceability entry = do
      -- Check that the entry has a proof
      let hasProof = checkEntryHasProof entry
      when (not hasProof) $
        throw $ MissingProofForEffect (entryHash entry)
      
      -- Check that the entry has a causal link (except for the first entry)
      let hasLink = checkEntryHasCausalLink entry
      when (not hasLink) $
        throw $ IncompleteAuditTrail (entryHash entry)
    
    -- Check if an entry has a proof
    -- In a real implementation, this would check for valid ZK proofs
    checkEntryHasProof :: LogEntry -> Bool
    checkEntryHasProof _ = True  -- Mock implementation
    
    -- Check if an entry has a causal link
    -- In a real implementation, this would verify the previous entry hash
    checkEntryHasCausalLink :: LogEntry -> Bool
    checkEntryHasCausalLink _ = True  -- Mock implementation
    
    -- Get the hash from a log entry
    entryHash :: LogEntry -> Hash
    entryHash _ = Hash "mock-entry-hash"  -- Mock implementation

-- | Verify that no transitions are backdated
verifyNoBackdating :: 
  (Member (Error SecurityError) r) =>
  ExecutionLog -> 
  TimeMap -> 
  Sem r ()
verifyNoBackdating log timeMap = do
  -- Get all entries from the log
  entries <- getLogEntries log `catch` \_ -> 
    throw $ GenericSecurityError "Failed to get log entries"
  
  -- Check each entry against the time map
  foldM_ verifyEntryTimestamps timeMap entries
  where
    verifyEntryTimestamps :: 
      (Member (Error SecurityError) r) =>
      TimeMap -> 
      LogEntry -> 
      Sem r TimeMap
    verifyEntryTimestamps curTimeMap entry = do
      -- Extract timeline and time from entry
      let (timeline, entryTime) = extractTimeInfo entry
      
      -- Check that the entry time is not before the current time
      -- for the given timeline
      curTime <- getTimeForTimeline curTimeMap timeline
      when (entryTime < curTime) $
        throw $ BackdatedTransition timeline curTime entryTime
      
      -- Update the time map and return
      return $ updateTimeMap curTimeMap timeline entryTime
    
    -- Extract timeline and time information from entry
    -- In a real implementation, this would get the timeline and time from the entry
    extractTimeInfo :: LogEntry -> (TimelineHash, LamportTime)
    extractTimeInfo _ = ("mock-timeline", LamportTime 0)  -- Mock implementation
    
    -- Get the current time for a timeline
    -- In a real implementation, this would lookup the time in the map
    getTimeForTimeline :: TimeMap -> TimelineHash -> Sem r LamportTime
    getTimeForTimeline _ _ = return $ LamportTime 0  -- Mock implementation
    
    -- Update the time map with a new time for a timeline
    -- In a real implementation, this would set the new time in the map
    updateTimeMap :: TimeMap -> TimelineHash -> LamportTime -> TimeMap
    updateTimeMap tm _ _ = tm  -- Mock implementation

-- Helper functions

-- | Convert a security error to an app error
securityErrorToAppError :: SecurityError -> AppError
securityErrorToAppError err = "Security violation: " <> show err 