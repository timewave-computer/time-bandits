{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: Helpers
Description: Consolidated test helpers for Time Bandits tests.

This module provides shared test helpers that are used across different test suites.
Rather than duplicating utility functions across test files, we centralize them here to:

1. Reduce code duplication
2. Ensure consistent test setup
3. Make tests more maintainable
4. Provide a single place to update test infrastructure

Test helpers include utilities for:
- Setting up test environments
- Creating test scenarios
- Generating test data
- Mocking subsystems
- Asserting on expected outcomes
-}
module TimeBandits.Helpers
  ( -- * Environment Setup
    setupTestEnv
  , withTestEnv
  , cleanupTestEnv
  
  -- * Timeline Helpers
  , createTestTimeline
  , createTestTimeMap
  , advanceTimeline
  
  -- * Program Helpers
  , createTestProgram
  , compileTestProgram
  , deployTestProgram
  , executeTestProgram
  
  -- * Actor Helpers
  , createTestActor
  , createTestActorSet
  , withActors
  
  -- * Effect Helpers
  , mockEffectInterpreter
  , captureEffects
  
  -- * Resource Helpers
  , createTestResources
  , verifyResourceState
  
  -- * Assertion Helpers
  , assertTimelineIntegrity
  , assertProgramState
  , assertEffectsApplied
  , assertResourceConsistency
  
  -- * Scenario Helpers
  , loadTestScenario
  , runTestScenario
  , verifyScenarioOutcome
  
  -- * Mock Adapters
  , mockStorageAdapter
  , mockNetworkAdapter
  , mockTimelineAdapter
  
  -- * Test Data Generation
  , genRandomHash
  , genRandomTimestamp
  , genRandomResourceSet
  , genRandomProgram
  
  -- * Proof Helpers
  , isValidProof
  , createActor
  ) where

import Control.Monad (void, forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Tasty.HUnit (Assertion, assertEqual, assertBool)
import Test.QuickCheck (Gen, arbitrary, oneof, elements, listOf, listOf1, choose)
import Polysemy (Sem, Member, embed, interpret, makeSem, run, runM, runFinal)
import Polysemy.Error (Error, fromEither, throw, runError)
import Polysemy.State (State, evalState, get, put, modify)

-- Import TimeBandits modules
import TimeBandits.Core.Types (Timeline, TimeMap, Program, ActorSpec, ActorRole(..), 
                             ResourceId, ResourceAmount, PrivKey, PubKey, AppError)
import TimeBandits.Proofs.ZKProof (ZKProof(..), proofData, proofMetadata)
import TimeBandits.Actors.Actor (Actor(..), ActorType(..), computePubKeyHash)
import TimeBandits.Core.Effects (KeyManagement, generateKeyPair, registerPublicKey, registerActorType)
import TimeBandits.Programs.Scenario (Scenario, ScenarioStep, ScenarioResult)

-- | Set up a test environment
setupTestEnv :: MonadIO m => m FilePath
setupTestEnv = liftIO $ do
  tempDir <- (<>) <$> pure "/tmp/time-bandits-test-" <*> (show <$> getCurrentTime)
  createDirectory tempDir
  createDirectory (tempDir </> "logs")
  createDirectory (tempDir </> "data")
  return tempDir

-- | Clean up the test environment
cleanupTestEnv :: MonadIO m => FilePath -> m ()
cleanupTestEnv tempDir = liftIO $
  removeDirectoryRecursive tempDir

-- | Run action with a test environment
withTestEnv :: MonadIO m => (FilePath -> m a) -> m a
withTestEnv action = liftIO $ withSystemTempDirectory "time-bandits-test" action

-- | Create a test timeline
createTestTimeline :: MonadIO m => Text -> m Timeline
createTestTimeline name = do
  -- Implementation would create a test timeline with the given name
  error "Not implemented: createTestTimeline"

-- | Create a test time map with multiple timelines
createTestTimeMap :: MonadIO m => [Timeline] -> m TimeMap
createTestTimeMap timelines = do
  -- Build a time map from the list of timelines
  pure $ foldr addTimeline empty timelines

-- | Advance a timeline to the next state
advanceTimeline :: MonadIO m => Timeline -> m Timeline
advanceTimeline timeline = do
  -- Implementation would create the next state of the timeline
  error "Not implemented: advanceTimeline"

-- | Create a test program
createTestProgram :: MonadIO m => Text -> m Program
createTestProgram name = do
  -- Implementation would create a test program with the given name
  error "Not implemented: createTestProgram"

-- | Compile a test program
compileTestProgram :: MonadIO m => Program -> m ByteString
compileTestProgram program = do
  -- Implementation would compile the program to bytecode
  error "Not implemented: compileTestProgram"

-- | Deploy a test program to a timeline
deployTestProgram :: MonadIO m => Timeline -> Program -> m ProgramId
deployTestProgram timeline program = do
  -- Implementation would deploy the program to the timeline
  error "Not implemented: deployTestProgram"

-- | Execute a test program
executeTestProgram :: MonadIO m => Timeline -> ProgramId -> m ProgramState
executeTestProgram timeline programId = do
  -- Implementation would execute the program on the timeline
  error "Not implemented: executeTestProgram"

-- | Create a test actor
createTestActor :: MonadIO m => ActorRole -> Text -> m ActorSpec
createTestActor role name = do
  -- Implementation would create a test actor with the given role and name
  error "Not implemented: createTestActor"

-- | Create a set of test actors (one of each role)
createTestActorSet :: MonadIO m => m [ActorSpec]
createTestActorSet = do
  -- Implementation would create one actor of each role
  error "Not implemented: createTestActorSet"

-- | Run an action with a set of actors
withActors :: MonadIO m => [ActorSpec] -> (Map Text ActorSpec -> m a) -> m a
withActors actors action = do
  -- Create a map of actors by ID for the action to use
  let actorMap = error "Not implemented: withActors"
  action actorMap

-- | Create a mock effect interpreter
mockEffectInterpreter :: [Effect] -> Sem r a -> Sem r a
mockEffectInterpreter _ sem = sem  -- Placeholder implementation

-- | Capture effects applied during execution
captureEffects :: Sem r a -> Sem r ([Effect], a)
captureEffects sem = do
  -- Implementation would capture effects during execution
  result <- sem
  pure ([], result)  -- Placeholder implementation

-- | Create test resources
createTestResources :: MonadIO m => Int -> m (Map ResourceId ResourceAmount)
createTestResources count = do
  -- Implementation would create test resources
  error "Not implemented: createTestResources"

-- | Verify the state of resources
verifyResourceState :: MonadIO m => Map ResourceId ResourceAmount -> Map ResourceId ResourceAmount -> m Bool
verifyResourceState expected actual = do
  -- Implementation would verify resources match expected state
  pure $ expected == actual

-- | Assert that a timeline maintains integrity
assertTimelineIntegrity :: Timeline -> Assertion
assertTimelineIntegrity _ = do
  -- Implementation would assert timeline integrity
  pure ()

-- | Assert that program state matches expected state
assertProgramState :: ProgramState -> ProgramState -> Assertion
assertProgramState expected actual = do
  -- Implementation would assert program states match
  assertEqual "Program states should match" expected actual

-- | Assert that effects were applied correctly
assertEffectsApplied :: [Effect] -> Assertion
assertEffectsApplied _ = do
  -- Implementation would assert effects were applied
  pure ()

-- | Assert that resources are consistent across timelines
assertResourceConsistency :: Map ResourceId ResourceAmount -> Map ResourceId ResourceAmount -> Assertion
assertResourceConsistency expected actual = do
  assertEqual "Resources should be consistent" expected actual

-- | Load a test scenario
loadTestScenario :: MonadIO m => FilePath -> m Scenario
loadTestScenario path = do
  -- Implementation would load a scenario from the file
  error "Not implemented: loadTestScenario"

-- | Run a test scenario
runTestScenario :: MonadIO m => Scenario -> m ScenarioResult
runTestScenario scenario = do
  -- Implementation would run the scenario
  error "Not implemented: runTestScenario"

-- | Verify the outcome of a scenario
verifyScenarioOutcome :: MonadIO m => ScenarioResult -> m Bool
verifyScenarioOutcome result = do
  -- Implementation would verify the scenario succeeded
  pure $ error "Not implemented: verifyScenarioOutcome"

-- | Create a mock storage adapter
mockStorageAdapter :: MonadIO m => m ()
mockStorageAdapter = do
  -- Implementation would create a mock storage adapter
  pure ()

-- | Create a mock network adapter
mockNetworkAdapter :: MonadIO m => m ()
mockNetworkAdapter = do
  -- Implementation would create a mock network adapter
  pure ()

-- | Create a mock timeline adapter
mockTimelineAdapter :: MonadIO m => m ()
mockTimelineAdapter = do
  -- Implementation would create a mock timeline adapter
  pure ()

-- | Generate a random hash
genRandomHash :: Gen ByteString
genRandomHash = BS.pack <$> listOf1 arbitrary

-- | Generate a random timestamp
genRandomTimestamp :: Gen UTCTime
genRandomTimestamp = error "Not implemented: genRandomTimestamp"

-- | Generate a random resource set
genRandomResourceSet :: Gen (Map ResourceId ResourceAmount)
genRandomResourceSet = error "Not implemented: genRandomResourceSet"

-- | Generate a random program
genRandomProgram :: Gen Program
genRandomProgram = error "Not implemented: genRandomProgram"

-- | Helper function to check if a proof is valid
isValidProof :: ZKProof -> Bool
isValidProof proof = 
  not (BS.null (proofData proof)) && 
  not (BS.null (proofMetadata proof))

-- | Create an actor with a given name and type
createActor :: Member (Error AppError) r => Member KeyManagement r => 
              String -> ActorType -> Sem r ((PrivKey, PubKey), Actor)
createActor name actorType = do
  -- Generate cryptographic identity
  (privKey, pubKey) <- generateKeyPair
  let actorId = computePubKeyHash pubKey
      actor = Actor actorId actorType
  
  -- Register the actor in the key management system
  registerPublicKey actorId pubKey
  registerActorType actorId actorType
  
  return ((privKey, pubKey), actor) 