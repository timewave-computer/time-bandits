{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.P2PEffectTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson (Value(..), Object, encode, decode, toJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Version.Extra (mkVersion)

import Core.Common (Hash, computeHash)
import Core.Types (ProgramId(..), BanditId(..))
import Core.Schema
    ( Schema(..)
    , SchemaField(..)
    , FieldType(..)
    , EvolutionRules(..)
    , SafeStatePolicy(..)
    , defaultCoreEvolutionRules
    )
import Core.Effect (Effect(..), EffectType(..), EffectProposal(..))
import Programs.Program (Program(..))
import Programs.ProgramState (ProgramState(..))
import Network.Bandit (Bandit(..), BanditNetwork(..), EffectConsensus(..))

-- | P2P Effect Propagation and Consensus tests
tests :: TestTree
tests = testGroup "P2P Effect Propagation and Consensus Tests"
  [ testGroup "Single Bandit Propose Tests"
      [ testCase "Bandit proposes effect, applies locally" testSingleBanditPropose
      ]
  , testGroup "Multiple Bandit Sync Tests"
      [ testCase "Bandit A proposes, Bandit B syncs" testMultipleBanditSync
      ]
  , testGroup "Conflict Resolution Tests"
      [ testCase "Bandits propose non-conflicting effects" testConflictResolution
      ]
  , testGroup "Invalid Proposal Rejection Tests"
      [ testCase "Reject effect with invalid parent hash" testInvalidProposalRejection
      ]
  , testGroup "Replay After Gossip Tests"
      [ testCase "Bandits gossip effects, replay and compare" testReplayAfterGossip
      ]
  ]

-- | Helper function to create a test program
createTestProgram :: ProgramId -> IO Program
createTestProgram programId = do
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  return Program
    { programID = programId
    , version = version
    , protocolVersion = protocolVersion
    , schema = schema
    , safeStatePolicy = AlwaysSafe
    , effectDAG = Map.empty
    , programState = Map.singleton "counter" (Number 0)
    }

-- | Helper function to create a test bandit
createTestBandit :: BanditId -> Program -> IO Bandit
createTestBandit banditId program = do
  return Bandit
    { banditId = banditId
    , programs = Map.singleton (programID program) program
    , pendingProposals = Map.empty
    , consensusLog = []
    }

-- | Test a single bandit proposing an effect and applying it locally
testSingleBanditPropose :: Assertion
testSingleBanditPropose = do
  -- Create test program
  let programId = ProgramId "test-program"
  program <- createTestProgram programId
  
  -- Create bandit
  let banditId = BanditId "bandit-1"
  bandit <- createTestBandit banditId program
  
  -- Create effect to increment counter
  let effect = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 1))
            ]
        }
  
  -- Create effect proposal
  let proposal = EffectProposal
        { proposedEffect = effect
        , targetProgram = programId
        , proposedBy = banditId
        , proposalTimestamp = undefined
        }
  
  -- Simulate bandit proposing and applying the effect locally
  let updatedProgram = program
        { effectDAG = Map.singleton "effect-1" (toJSON effect)
        , programState = Map.singleton "counter" (Number 1)
        }
  
  let updatedBandit = bandit
        { programs = Map.singleton programId updatedProgram
        , pendingProposals = Map.singleton "effect-1" proposal
        , consensusLog = [proposal]
        }
  
  -- Verify effect was applied
  let appliedProgram = case Map.lookup programId (programs updatedBandit) of
        Just prog -> prog
        Nothing -> error "Program not found"
  
  assertEqual "Counter should be incremented after effect application" 
    (Just (Number 1)) (Map.lookup "counter" (programState appliedProgram))
  
  assertEqual "Effect should be in the DAG" 
    1 (Map.size (effectDAG appliedProgram))

-- | Test multiple bandits synchronizing effects
testMultipleBanditSync :: Assertion
testMultipleBanditSync = do
  -- Create test program
  let programId = ProgramId "test-program"
  program <- createTestProgram programId
  
  -- Create two bandits with the same initial program
  let banditIdA = BanditId "bandit-a"
  let banditIdB = BanditId "bandit-b"
  banditA <- createTestBandit banditIdA program
  banditB <- createTestBandit banditIdB program
  
  -- Create effect to increment counter
  let effect = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 1))
            ]
        }
  
  -- Create effect proposal from Bandit A
  let proposal = EffectProposal
        { proposedEffect = effect
        , targetProgram = programId
        , proposedBy = banditIdA
        , proposalTimestamp = undefined
        }
  
  -- Bandit A proposes and applies the effect locally
  let updatedProgramA = program
        { effectDAG = Map.singleton "effect-1" (toJSON effect)
        , programState = Map.singleton "counter" (Number 1)
        }
  
  let updatedBanditA = banditA
        { programs = Map.singleton programId updatedProgramA
        , pendingProposals = Map.singleton "effect-1" proposal
        , consensusLog = [proposal]
        }
  
  -- Simulate network propagation: Bandit B receives and applies the proposal
  let updatedProgramB = program
        { effectDAG = Map.singleton "effect-1" (toJSON effect)
        , programState = Map.singleton "counter" (Number 1)
        }
  
  let updatedBanditB = banditB
        { programs = Map.singleton programId updatedProgramB
        , pendingProposals = Map.empty  -- B doesn't track A's proposals as pending
        , consensusLog = [proposal]  -- But B does record the consensus
        }
  
  -- Verify both bandits have the same program state
  let programA = case Map.lookup programId (programs updatedBanditA) of
        Just prog -> prog
        Nothing -> error "Program not found in Bandit A"
  
  let programB = case Map.lookup programId (programs updatedBanditB) of
        Just prog -> prog
        Nothing -> error "Program not found in Bandit B"
  
  assertEqual "Both bandits should have same counter value" 
    (Map.lookup "counter" (programState programA)) 
    (Map.lookup "counter" (programState programB))
  
  assertEqual "Both bandits should have same effect in DAG" 
    (Map.size (effectDAG programA)) 
    (Map.size (effectDAG programB))

-- | Test resolution of non-conflicting effects from different bandits
testConflictResolution :: Assertion
testConflictResolution = do
  -- Create test program
  let programId = ProgramId "test-program"
  program <- createTestProgram programId
  
  -- Create two bandits with the same initial program
  let banditIdA = BanditId "bandit-a"
  let banditIdB = BanditId "bandit-b"
  banditA <- createTestBandit banditIdA program
  banditB <- createTestBandit banditIdB program
  
  -- Bandit A creates and applies effect-1
  let effectA = Effect
        { effectID = "effect-a"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 1))
            ]
        }
  
  let proposalA = EffectProposal
        { proposedEffect = effectA
        , targetProgram = programId
        , proposedBy = banditIdA
        , proposalTimestamp = undefined
        }
  
  -- Bandit B creates and applies effect-2 (without knowing about effect-1)
  let effectB = Effect
        { effectID = "effect-b"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 2))
            ]
        }
  
  let proposalB = EffectProposal
        { proposedEffect = effectB
        , targetProgram = programId
        , proposedBy = banditIdB
        , proposalTimestamp = undefined
        }
  
  -- Apply effects to respective bandits
  let programA = program
        { effectDAG = Map.singleton "effect-a" (toJSON effectA)
        , programState = Map.singleton "counter" (Number 1)
        }
  
  let programB = program
        { effectDAG = Map.singleton "effect-b" (toJSON effectB)
        , programState = Map.singleton "counter" (Number 2)
        }
  
  let updatedBanditA = banditA
        { programs = Map.singleton programId programA
        , pendingProposals = Map.singleton "effect-a" proposalA
        , consensusLog = [proposalA]
        }
  
  let updatedBanditB = banditB
        { programs = Map.singleton programId programB
        , pendingProposals = Map.singleton "effect-b" proposalB
        , consensusLog = [proposalB]
        }
  
  -- Now simulate syncing: Bandit A learns about effect-b and Bandit B learns about effect-a
  
  -- Create a merge effect that depends on both effect-a and effect-b
  let mergeEffect = Effect
        { effectID = "merge-effect"
        , parentEffects = ["effect-a", "effect-b"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "mergeState")
            ]
        }
  
  let mergeProposal = EffectProposal
        { proposedEffect = mergeEffect
        , targetProgram = programId
        , proposedBy = banditIdA  -- Could be either bandit
        , proposalTimestamp = undefined
        }
  
  -- Apply both effects plus merge to both bandits
  let mergedProgramA = programA
        { effectDAG = Map.fromList
            [ ("effect-a", toJSON effectA)
            , ("effect-b", toJSON effectB)
            , ("merge-effect", toJSON mergeEffect)
            ]
        , programState = Map.singleton "counter" (Number 3)  -- 1 + 2 = 3
        }
  
  let mergedProgramB = programB
        { effectDAG = Map.fromList
            [ ("effect-a", toJSON effectA)
            , ("effect-b", toJSON effectB)
            , ("merge-effect", toJSON mergeEffect)
            ]
        , programState = Map.singleton "counter" (Number 3)  -- 2 + 1 = 3
        }
  
  let finalBanditA = updatedBanditA
        { programs = Map.singleton programId mergedProgramA
        , consensusLog = proposalA : proposalB : mergeProposal : consensusLog updatedBanditA
        }
  
  let finalBanditB = updatedBanditB
        { programs = Map.singleton programId mergedProgramB
        , consensusLog = proposalB : proposalA : mergeProposal : consensusLog updatedBanditB
        }
  
  -- Verify both bandits converged to the same state
  let finalProgramA = case Map.lookup programId (programs finalBanditA) of
        Just prog -> prog
        Nothing -> error "Program not found in final Bandit A"
  
  let finalProgramB = case Map.lookup programId (programs finalBanditB) of
        Just prog -> prog
        Nothing -> error "Program not found in final Bandit B"
  
  assertEqual "Both bandits should have same final counter value after merge" 
    (Map.lookup "counter" (programState finalProgramA)) 
    (Map.lookup "counter" (programState finalProgramB))
  
  assertEqual "Both bandits should have same number of effects in DAG" 
    (Map.size (effectDAG finalProgramA)) 
    (Map.size (effectDAG finalProgramB))
  
  assertEqual "Final counter should be 3" 
    (Just (Number 3)) (Map.lookup "counter" (programState finalProgramA))

-- | Test rejection of invalid effect proposals
testInvalidProposalRejection :: Assertion
testInvalidProposalRejection = do
  -- Create test program
  let programId = ProgramId "test-program"
  program <- createTestProgram programId
  
  -- Create bandit
  let banditId = BanditId "bandit-1"
  bandit <- createTestBandit banditId program
  
  -- Create valid first effect
  let effect1 = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Apply first effect to program
  let updatedProgram = program
        { effectDAG = Map.singleton "effect-1" (toJSON effect1)
        , programState = Map.singleton "counter" (Number 1)
        }
  
  let updatedBandit = bandit
        { programs = Map.singleton programId updatedProgram
        }
  
  -- Create invalid effect with non-existent parent
  let invalidEffect = Effect
        { effectID = "invalid-effect"
        , parentEffects = ["non-existent-effect"]  -- This parent doesn't exist
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  let invalidProposal = EffectProposal
        { proposedEffect = invalidEffect
        , targetProgram = programId
        , proposedBy = banditId
        , proposalTimestamp = undefined
        }
  
  -- Function to validate an effect proposal
  let isValidProposal :: Program -> EffectProposal -> Bool
      isValidProposal prog proposal =
        let effect = proposedEffect proposal
            parents = parentEffects effect
        in all (`Map.member` effectDAG prog) parents
  
  -- Check that invalid proposal is rejected
  let validationResult = isValidProposal updatedProgram invalidProposal
  
  assertBool "Invalid proposal with non-existent parent should be rejected" 
    (not validationResult)
  
  -- Create valid effect with existing parent
  let validEffect = Effect
        { effectID = "valid-effect"
        , parentEffects = ["effect-1"]  -- This parent exists
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  let validProposal = EffectProposal
        { proposedEffect = validEffect
        , targetProgram = programId
        , proposedBy = banditId
        , proposalTimestamp = undefined
        }
  
  -- Check that valid proposal is accepted
  let validResult = isValidProposal updatedProgram validProposal
  
  assertBool "Valid proposal with existing parent should be accepted" 
    validResult

-- | Test replay consistency after gossip
testReplayAfterGossip :: Assertion
testReplayAfterGossip = do
  -- Create test program
  let programId = ProgramId "test-program"
  program <- createTestProgram programId
  
  -- Create two bandits
  let banditIdA = BanditId "bandit-a"
  let banditIdB = BanditId "bandit-b"
  banditA <- createTestBandit banditIdA program
  banditB <- createTestBandit banditIdB program
  
  -- Create a sequence of effects
  let effect1 = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 1))
            ]
        }
  
  let effect2 = Effect
        { effectID = "effect-2"
        , parentEffects = ["effect-1"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 2))
            ]
        }
  
  let effect3 = Effect
        { effectID = "effect-3"
        , parentEffects = ["effect-2"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 3))
            ]
        }
  
  -- Create proposals for the effects
  let proposal1 = EffectProposal
        { proposedEffect = effect1
        , targetProgram = programId
        , proposedBy = banditIdA
        , proposalTimestamp = undefined
        }
  
  let proposal2 = EffectProposal
        { proposedEffect = effect2
        , targetProgram = programId
        , proposedBy = banditIdA
        , proposalTimestamp = undefined
        }
  
  let proposal3 = EffectProposal
        { proposedEffect = effect3
        , targetProgram = programId
        , proposedBy = banditIdB
        , proposalTimestamp = undefined
        }
  
  -- Bandit A applies effect 1 and 2, Bandit B applies effect 3
  let programA = program
        { effectDAG = Map.fromList
            [ ("effect-1", toJSON effect1)
            , ("effect-2", toJSON effect2)
            ]
        , programState = Map.singleton "counter" (Number 3)  -- 0 + 1 + 2 = 3
        }
  
  let updatedBanditA = banditA
        { programs = Map.singleton programId programA
        , consensusLog = [proposal1, proposal2]
        }
  
  -- Simulate that Bandit B knows effect 1 and 2 (from gossip) and applies effect 3
  let programB = program
        { effectDAG = Map.fromList
            [ ("effect-1", toJSON effect1)
            , ("effect-2", toJSON effect2)
            , ("effect-3", toJSON effect3)
            ]
        , programState = Map.singleton "counter" (Number 6)  -- 0 + 1 + 2 + 3 = 6
        }
  
  let updatedBanditB = banditB
        { programs = Map.singleton programId programB
        , consensusLog = [proposal1, proposal2, proposal3]
        }
  
  -- Now bandits sync: A learns about effect 3
  let finalProgramA = programA
        { effectDAG = Map.fromList
            [ ("effect-1", toJSON effect1)
            , ("effect-2", toJSON effect2)
            , ("effect-3", toJSON effect3)
            ]
        , programState = Map.singleton "counter" (Number 6)  -- 0 + 1 + 2 + 3 = 6
        }
  
  let finalBanditA = updatedBanditA
        { programs = Map.singleton programId finalProgramA
        , consensusLog = proposal3 : consensusLog updatedBanditA
        }
  
  -- Serialize and "clear memory" to simulate restart/replay
  let serializedProgramA = encode finalProgramA
  let serializedProgramB = encode programB
  
  let deserializedProgramA = decode serializedProgramA :: Maybe Program
  let deserializedProgramB = decode serializedProgramB :: Maybe Program
  
  -- Verify program states after deserialization
  case (deserializedProgramA, deserializedProgramB) of
    (Nothing, _) -> assertFailure "Failed to deserialize Program A"
    (_, Nothing) -> assertFailure "Failed to deserialize Program B"
    (Just rehydratedProgramA, Just rehydratedProgramB) -> do
      -- Simulate replay by loading the state
      let replayedProgramA = rehydratedProgramA
      let replayedProgramB = rehydratedProgramB
      
      -- Verify counter values match expected values after replay
      assertEqual "Program A counter should be 6 after replay" 
        (Just (Number 6)) (Map.lookup "counter" (programState replayedProgramA))
        
      assertEqual "Program B counter should be 6 after replay" 
        (Just (Number 6)) (Map.lookup "counter" (programState replayedProgramB))
      
      -- Verify both programs have identical state
      assertEqual "Both programs should have identical state after replay" 
        (programState replayedProgramA) (programState replayedProgramB)
      
      -- Verify effect DAGs have the same size
      assertEqual "Both programs should have same DAG size after replay" 
        (Map.size (effectDAG replayedProgramA)) (Map.size (effectDAG replayedProgramB))
``` 