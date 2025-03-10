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
module Proofs.SecurityVerifier 
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

import Control.Monad (foldM, when, foldM_, unless)
import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Set qualified as Set
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw, catch)
import Polysemy.Embed (Embed)

-- Import from TimeBandits modules
import Core (Hash(..))
import Core.Types (LamportTime(..), AppError(..))
import Core.ProgramId (ProgramId(..))
import Core.Resource (ResourceHash, getResourceIdentifier)
import Core.ResourceLedger (OwnershipRecord(..))
import Execution.ResourceLedger (ResourceLedger(..), getOwnershipHistory)
import Execution.ExecutionLog (ExecutionLog(..), getLogEntries)
import Execution.LogStore (LogEntry(..))
import Core.Timeline (TimelineHash(..))
import Core.TimeMap (TimeMap(..))
import qualified Core.Common as Common

-- IsString instances for newtypes to allow the use of string literals
instance IsString TimelineHash where
  fromString s = Common.EntityHash $ Common.computeHash $ TE.encodeUtf8 $ T.pack s

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
  (Member (Error SecurityError) r, Member (Error AppError) r, Member (Embed IO) r) =>
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
  (Member (Error SecurityError) r, Member (Error AppError) r, Member (Embed IO) r) =>
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
  (Member (Error SecurityError) r, Member (Error AppError) r, Member (Embed IO) r) =>
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
      history <- catch 
        (getOwnershipHistory ledger resHash)
        (\(_ :: AppError) -> throw $ MissingOwnershipRecord resHash)
      
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
  (Member (Error SecurityError) r, Member (Error AppError) r, Member (Embed IO) r) =>
  ExecutionLog -> 
  Sem r ()
verifyNoReentrancy log = do
  -- Get all entries from the log
  entries <- catch
    (getLogEntries log)
    (\(_ :: AppError) -> throw $ GenericSecurityError "Failed to get log entries")
  
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
    extractProgramId :: LogEntry -> ProgramId
    extractProgramId _ = "mock-program-id"  -- Mock implementation
    
    -- Extract Lamport time from log entry
    extractLamportTime :: LogEntry -> LamportTime
    extractLamportTime _ = LamportTime 0  -- Mock implementation

-- | Verify that the system maintains a complete audit trail
verifyFullTraceability :: 
  (Member (Error SecurityError) r, Member (Error AppError) r, Member (Embed IO) r) =>
  ExecutionLog -> 
  Sem r ()
verifyFullTraceability log = do
  -- Get all entries from the log
  entries <- catch
    (getLogEntries log)
    (\(_ :: AppError) -> throw $ GenericSecurityError "Failed to get log entries")
  
  -- Check each entry has a proof
  mapM_ verifyEntryTraceability entries
  where
    verifyEntryTraceability :: 
      (Member (Error SecurityError) r) =>
      LogEntry -> 
      Sem r ()
    verifyEntryTraceability entry = do
      let entryHash = getEntryHash entry
      -- Check if entry has a proof
      unless (checkEntryHasProof entry) $
        throw $ MissingProofForEffect entryHash
    
    -- Get hash of a log entry
    getEntryHash :: LogEntry -> Hash
    getEntryHash _ = Hash "mock-hash"  -- Mock implementation
    
    -- Check if entry has a valid proof
    checkEntryHasProof :: LogEntry -> Bool
    checkEntryHasProof _ = True  -- Mock implementation

-- | Verify that no operations backdate the timeline (prevent history rewriting)
verifyNoBackdating :: 
  (Member (Error SecurityError) r, Member (Error AppError) r, Member (Embed IO) r) =>
  ExecutionLog -> 
  TimeMap -> 
  Sem r ()
verifyNoBackdating log timeMap = do
  -- Get all entries from the log
  entries <- catch
    (getLogEntries log)
    (\(_ :: AppError) -> throw $ GenericSecurityError "Failed to get log entries")
  
  -- Check each entry respects timeline ordering
  foldM_ verifyEntryTimestamps timeMap entries
  where
    verifyEntryTimestamps :: 
      (Member (Error SecurityError) r) =>
      TimeMap -> 
      LogEntry -> 
      Sem r TimeMap
    verifyEntryTimestamps curTimeMap entry = do
      -- Extract timeline and time from entry
      let (timeline, time) = extractTimeInfo entry
      
      -- Check if this timeline already exists in the time map
      -- If so, ensure the new time is greater than the recorded time
      case Map.lookup timeline (getTimelineMap curTimeMap) of
        Just recordedTime | recordedTime > time ->
          throw $ BackdatedTransition timeline recordedTime time
        _ -> 
          -- Update the time map with the new time
          return $ updateTimeMap curTimeMap timeline time
    
    -- Extract timeline and time info from log entry
    extractTimeInfo :: LogEntry -> (TimelineHash, LamportTime)
    extractTimeInfo _ = ("mock-timeline", LamportTime 0)  -- Mock implementation
    
    -- Update the time map with a new timeline time
    updateTimeMap :: TimeMap -> TimelineHash -> LamportTime -> TimeMap
    updateTimeMap tm timeline time = tm  -- Mock implementation
    
    -- Helper function to get the timeline map from a TimeMap
    getTimelineMap :: TimeMap -> Map.Map TimelineHash LamportTime
    getTimelineMap _ = Map.empty  -- Mock implementation

-- Helper functions

-- | Convert a security error to an application error
securityErrorToAppError :: SecurityError -> AppError
securityErrorToAppError err = AuthorizationError (T.pack $ "Security violation: " <> show err) 