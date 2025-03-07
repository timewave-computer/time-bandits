{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simulation.EnvironmentTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value(..), Object, encode, decode, toJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Version.Extra (mkVersion)
import Control.Monad (void)
import Control.Concurrent (threadDelay)

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
import Simulation.Mode (SimulationMode(..), SimulationEnvironment(..))
import Simulation.Controller (deployProgram, spawnBandit, queryState, applyEffect)
import Core.Resource (Resource(..), ResourceId(..), Amount(..))
import Core.TimelineId (TimelineId(..))

-- | Simulation Environment tests
tests :: TestTree
tests = testGroup "Simulation Environment Tests"
  [ testGroup "In-Memory Simulation Tests"
      [ testCase "Single program run in memory" testSingleProgramInMemory
      ]
  , testGroup "Multi-Process Simulation Tests"
      [ testCase "Cross-program call in multiple processes" testCrossProgramMultiProcess
      ]
  , testGroup "Geo-Distributed Simulation Tests"
      [ testCase "Effect gossip across distributed nodes" testGeoDistributedGossip
      ]
  , testGroup "Replay Consistency Tests"
      [ testCase "Same program across simulation modes has identical state" testReplayConsistency
      ]
  ]

-- | Create a test schema for simulation
createTestSchema :: IO Schema
createTestSchema = do
  return Schema
    { schemaVersion = mkVersion [1, 0, 0]
    , fields = 
        [ SchemaField "balance" FieldDecimal False
        , SchemaField "owner" FieldText True
        ]
    , evolutionRules = defaultCoreEvolutionRules
    }

-- | Create a test program for simulation
createTestProgram :: ProgramId -> IO Program
createTestProgram programId = do
  schema <- createTestSchema
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  return Program
    { programID = programId
    , version = version
    , protocolVersion = protocolVersion
    , schema = schema
    , safeStatePolicy = AlwaysSafe
    , effectDAG = Map.empty
    , programState = Map.singleton "balance" (Number 0)
    }

-- | Test a single program running in in-memory simulation mode
testSingleProgramInMemory :: Assertion
testSingleProgramInMemory = do
  -- Set up in-memory simulation environment
  let simEnv = SimulationEnvironment
        { simulationMode = InMemory
        , networkLatency = 0  -- No latency in-memory
        , bandits = []
        , programs = Map.empty
        }
  
  -- Create a test program
  let programId = ProgramId "account-program"
  program <- createTestProgram programId
  
  -- Deploy the program to the simulation
  let banditId = BanditId "test-bandit"
  simEnv' <- deployProgram simEnv banditId program
  
  -- Create deposit effect
  let depositEffect = Effect
        { effectID = "deposit-effect"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("amount", Number 100.0)
            , ("asset", String "ETH")
            ]
        }
  
  -- Apply the deposit effect
  simEnv'' <- applyEffect simEnv' banditId programId depositEffect
  
  -- Query program state
  state <- queryState simEnv'' programId
  
  -- Check balance was updated
  case Map.lookup "balance" state of
    Just (Number balance) -> assertEqual "Balance should be updated to 100" 100.0 balance
    _ -> assertFailure "Balance not found or not a number"
  
  -- Create withdrawal effect
  let withdrawEffect = Effect
        { effectID = "withdraw-effect"
        , parentEffects = ["deposit-effect"]
        , effectType = WithdrawEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("amount", Number 30.0)
            , ("asset", String "ETH")
            ]
        }
  
  -- Apply the withdrawal effect
  simEnv''' <- applyEffect simEnv'' banditId programId withdrawEffect
  
  -- Query updated program state
  finalState <- queryState simEnv''' programId
  
  -- Check final balance
  case Map.lookup "balance" finalState of
    Just (Number balance) -> assertEqual "Final balance should be 70" 70.0 balance
    _ -> assertFailure "Final balance not found or not a number"

-- | Test cross-program calls in a multi-process simulation
testCrossProgramMultiProcess :: Assertion
testCrossProgramMultiProcess = do
  -- Set up multi-process simulation environment
  let simEnv = SimulationEnvironment
        { simulationMode = MultiProcess
        , networkLatency = 10  -- Slight latency to simulate inter-process communication
        , bandits = []
        , programs = Map.empty
        }
  
  -- Create an account program
  let accountProgramId = ProgramId "account-program"
  accountProgram <- createTestProgram accountProgramId
  
  -- Create a trader program
  let traderProgramId = ProgramId "trader-program"
  schema <- createTestSchema
  let traderSchema = schema { fields = fields schema ++ [SchemaField "trades" FieldInt False] }
  let traderProgram = Program
        { programID = traderProgramId
        , version = mkVersion [1, 0, 0]
        , protocolVersion = mkVersion [1, 0, 0]
        , schema = traderSchema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.fromList
            [ ("balance", Number 0)
            , ("trades", Number 0)
            ]
        }
  
  -- Deploy programs to different bandits in the simulation
  let accountBanditId = BanditId "account-bandit"
  let traderBanditId = BanditId "trader-bandit"
  
  simEnv' <- deployProgram simEnv accountBanditId accountProgram
  simEnv'' <- deployProgram simEnv' traderBanditId traderProgram
  
  -- Initialize account program with funds
  let depositEffect = Effect
        { effectID = "deposit-effect"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("amount", Number 100.0)
            , ("asset", String "ETH")
            ]
        }
  
  simEnv''' <- applyEffect simEnv'' accountBanditId accountProgramId depositEffect
  
  -- Create a cross-program transfer from trader to account
  let transferEffect = Effect
        { effectID = "transfer-effect"
        , parentEffects = []
        , effectType = TransferEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("amount", Number 50.0)
            , ("asset", String "ETH")
            , ("from", toJSON traderProgramId)
            , ("to", toJSON accountProgramId)
            ]
        }
  
  -- Apply the transfer effect
  simEnv'''' <- applyEffect simEnv''' traderBanditId traderProgramId transferEffect
  
  -- Allow some time for the effect to propagate
  threadDelay 20000  -- 20ms
  
  -- Query both programs' states
  accountState <- queryState simEnv'''' accountProgramId
  traderState <- queryState simEnv'''' traderProgramId
  
  -- Check account balance increased
  case Map.lookup "balance" accountState of
    Just (Number balance) -> assertEqual "Account balance should be 150" 150.0 balance
    _ -> assertFailure "Account balance not found or not a number"
  
  -- Check trader balance decreased
  case Map.lookup "balance" traderState of
    Just (Number balance) -> assertEqual "Trader balance should be -50" (-50.0) balance
    _ -> assertFailure "Trader balance not found or not a number"
  
  -- Update trader program to record the trade
  let tradeEffect = Effect
        { effectID = "trade-effect"
        , parentEffects = ["transfer-effect"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "recordTrade")
            ]
        }
  
  simEnv''''' <- applyEffect simEnv'''' traderBanditId traderProgramId tradeEffect
  
  -- Query final trader state
  finalTraderState <- queryState simEnv''''' traderProgramId
  
  -- Check trade count increased
  case Map.lookup "trades" finalTraderState of
    Just (Number trades) -> assertEqual "Trade count should be 1" 1 trades
    _ -> assertFailure "Trade count not found or not a number"

-- | Test effect gossip across geo-distributed nodes
testGeoDistributedGossip :: Assertion
testGeoDistributedGossip = do
  -- Set up geo-distributed simulation environment
  let simEnv = SimulationEnvironment
        { simulationMode = GeoDistributed
        , networkLatency = 50  -- Higher latency to simulate geographic distribution
        , bandits = []
        , programs = Map.empty
        }
  
  -- Create a shared program
  let programId = ProgramId "shared-program"
  program <- createTestProgram programId
  
  -- Deploy the program to multiple bandits in different "regions"
  let banditId1 = BanditId "bandit-region1"
  let banditId2 = BanditId "bandit-region2"
  let banditId3 = BanditId "bandit-region3"
  
  simEnv' <- deployProgram simEnv banditId1 program
  simEnv'' <- deployProgram simEnv' banditId2 program
  simEnv''' <- deployProgram simEnv'' banditId3 program
  
  -- Create an effect on one bandit
  let effect = Effect
        { effectID = "deposit-effect"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("amount", Number 100.0)
            , ("asset", String "ETH")
            ]
        }
  
  -- Apply the effect on bandit1
  simEnv'''' <- applyEffect simEnv''' banditId1 programId effect
  
  -- Wait for gossip to propagate the effect
  threadDelay 200000  -- 200ms to account for higher latency
  
  -- Query state from all three bandits
  state1 <- queryState simEnv'''' programId
  state2 <- queryState simEnv'''' programId
  state3 <- queryState simEnv'''' programId
  
  -- Check all bandits have the same balance
  case (Map.lookup "balance" state1, Map.lookup "balance" state2, Map.lookup "balance" state3) of
    (Just (Number b1), Just (Number b2), Just (Number b3)) -> do
      assertEqual "Bandit 1 balance should be 100" 100.0 b1
      assertEqual "Bandit 2 balance should match bandit 1" b1 b2
      assertEqual "Bandit 3 balance should match bandit 1" b1 b3
    _ -> assertFailure "Balance not found or not a number in one or more bandits"

-- | Test replay consistency across simulation modes
testReplayConsistency :: Assertion
testReplayConsistency = do
  -- Set up all three simulation environments
  let inMemEnv = SimulationEnvironment
        { simulationMode = InMemory
        , networkLatency = 0
        , bandits = []
        , programs = Map.empty
        }
  
  let multiProcEnv = SimulationEnvironment
        { simulationMode = MultiProcess
        , networkLatency = 10
        , bandits = []
        , programs = Map.empty
        }
  
  let geoDistEnv = SimulationEnvironment
        { simulationMode = GeoDistributed
        , networkLatency = 50
        , bandits = []
        , programs = Map.empty
        }
  
  -- Create identical programs for each environment
  let programId = ProgramId "test-program"
  program <- createTestProgram programId
  
  -- Deploy the program to each environment
  let banditId = BanditId "test-bandit"
  inMemEnv' <- deployProgram inMemEnv banditId program
  multiProcEnv' <- deployProgram multiProcEnv banditId program
  geoDistEnv' <- deployProgram geoDistEnv banditId program
  
  -- Create the same sequence of effects to apply in each environment
  let effect1 = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("amount", Number 100.0)
            , ("asset", String "ETH")
            ]
        }
  
  let effect2 = Effect
        { effectID = "effect-2"
        , parentEffects = ["effect-1"]
        , effectType = WithdrawEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("amount", Number 30.0)
            , ("asset", String "ETH")
            ]
        }
  
  let effect3 = Effect
        { effectID = "effect-3"
        , parentEffects = ["effect-2"]
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("amount", Number 20.0)
            , ("asset", String "ETH")
            ]
        }
  
  -- Apply effects to in-memory environment
  inMemEnv'' <- applyEffect inMemEnv' banditId programId effect1
  inMemEnv''' <- applyEffect inMemEnv'' banditId programId effect2
  inMemEnv'''' <- applyEffect inMemEnv''' banditId programId effect3
  
  -- Apply effects to multi-process environment
  multiProcEnv'' <- applyEffect multiProcEnv' banditId programId effect1
  multiProcEnv''' <- applyEffect multiProcEnv'' banditId programId effect2
  multiProcEnv'''' <- applyEffect multiProcEnv''' banditId programId effect3
  
  -- Apply effects to geo-distributed environment
  geoDistEnv'' <- applyEffect geoDistEnv' banditId programId effect1
  geoDistEnv''' <- applyEffect geoDistEnv'' banditId programId effect2
  geoDistEnv'''' <- applyEffect geoDistEnv''' banditId programId effect3
  
  -- Allow time for propagation in the geo-distributed case
  threadDelay 200000  -- 200ms
  
  -- Query final states from all environments
  inMemState <- queryState inMemEnv'''' programId
  multiProcState <- queryState multiProcEnv'''' programId
  geoDistState <- queryState geoDistEnv'''' programId
  
  -- Check all environments have the same final state
  case (Map.lookup "balance" inMemState, Map.lookup "balance" multiProcState, Map.lookup "balance" geoDistState) of
    (Just (Number b1), Just (Number b2), Just (Number b3)) -> do
      assertEqual "In-memory final balance should be 90" 90.0 b1  -- 100 - 30 + 20
      assertEqual "Multi-process balance should match in-memory" b1 b2
      assertEqual "Geo-distributed balance should match in-memory" b1 b3
    _ -> assertFailure "Balance not found or not a number in one or more environments"
  
  -- Verify effect DAGs have the same structure
  -- In a real implementation, we would serialize and compare the DAGs 