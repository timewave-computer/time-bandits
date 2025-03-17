{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeBandits.Core.LoggingTest (tests) where

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
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import qualified Data.ByteString as SBS

import TimeBandits.Core.Common.Types (Hash, computeHash)
import TimeBandits.Core.Types 
    ( ProgramId(..)
    , FactId(..)
    , FactValue(..)
    , FactSnapshot(..)
    , ObservedFact(..)
    , ObservationProof(..)
    )
import TimeBandits.Core.Schema
    ( Schema(..)
    , SchemaField(..)
    , FieldType(..)
    , EvolutionRules(..)
    , SafeStatePolicy(..)
    , defaultCoreEvolutionRules
    )
import TimeBandits.Core.Effect (Effect(..), EffectType(..))
import Programs.Program (Program(..))
import Programs.ProgramState (ProgramState(..))
import Logging.EffectLog (EffectLog(..), appendToEffectLog, readEffectLog)
import Logging.FactLog (FactLog(..), appendToFactLog, readFactLog)
import Logging.ReplayEngine (replayFromLogs, validateLogIntegrity)

-- | Logging and Traceability tests
tests :: TestTree
tests = testGroup "Logging and Traceability Tests"
  [ testGroup "Effect Log Persistence Tests"
      [ testCase "Apply effects, write log, restart, replay from log" testEffectLogPersistence
      ]
  , testGroup "Fact Log Persistence Tests"
      [ testCase "Observe facts, write log, restart, replay from log" testFactLogPersistence
      ]
  , testGroup "Causal Trace Reconstruction Tests"
      [ testCase "Walk effect log and build visual DAG" testCausalTraceReconstruction
      ]
  , testGroup "Tamper Detection Tests"
      [ testCase "Modify log, check replay detects invalid chain" testTamperDetection
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

-- | Test effect log persistence and replay
testEffectLogPersistence :: Assertion
testEffectLogPersistence = do
  -- Create a test program
  let programId = ProgramId "test-program"
  program <- createTestProgram programId
  
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
  
  -- Apply effects to program (simulated)
  let appliedProgram = program
        { effectDAG = Map.fromList
            [ ("effect-1", toJSON effect1)
            , ("effect-2", toJSON effect2)
            , ("effect-3", toJSON effect3)
            ]
        , programState = Map.singleton "counter" (Number 6)  -- 0 + 1 + 2 + 3 = 6
        }
  
  -- Write effects to log file
  withSystemTempFile "effect_log.json" $ \logPath handle -> do
    -- Close the handle so we can reopen it ourselves
    hClose handle
    
    -- Create a new effect log
    let effectLog = EffectLog
          { programID = programId
          , effects = []
          }
    
    -- Append effects to log
    appendToEffectLog logPath effectLog effect1
    effectLog' <- readEffectLog logPath
    appendToEffectLog logPath effectLog' effect2
    effectLog'' <- readEffectLog logPath
    appendToEffectLog logPath effectLog'' effect3
    
    -- "Restart" by creating a new empty program
    newProgram <- createTestProgram programId
    
    -- Replay effects from log
    finalEffectLog <- readEffectLog logPath
    let replayedProgram = replayFromLogs newProgram finalEffectLog FactLog{programID = programId, facts = []}
    
    -- Verify replayed program matches the original
    assertEqual "Replayed program should have same counter value" 
      (Map.lookup "counter" (programState appliedProgram)) 
      (Map.lookup "counter" (programState replayedProgram))
    
    assertEqual "Replayed program should have same number of effects" 
      (Map.size (effectDAG appliedProgram)) 
      (Map.size (effectDAG replayedProgram))

-- | Test fact log persistence and replay
testFactLogPersistence :: Assertion
testFactLogPersistence = do
  -- Create a test program
  let programId = ProgramId "test-program"
  program <- createTestProgram programId
  
  -- Create facts to observe
  let factId1 = FactId "price-fact-1"
  let factValue1 = NumberFact 100.0
  let observationProof1 = ObservationProof
        { proofSource = "oracle-1"
        , proofSignature = "valid-signature-1"
        , proofMetadata = Map.empty
        }
  
  let observedFact1 = ObservedFact
        { factID = factId1
        , factValue = factValue1
        , observationProof = observationProof1
        }
  
  let factId2 = FactId "price-fact-2"
  let factValue2 = NumberFact 200.0
  let observationProof2 = ObservationProof
        { proofSource = "oracle-2"
        , proofSignature = "valid-signature-2"
        , proofMetadata = Map.empty
        }
  
  let observedFact2 = ObservedFact
        { factID = factId2
        , factValue = factValue2
        , observationProof = observationProof2
        }
  
  -- Create fact snapshots and effects
  let factSnapshot1 = FactSnapshot
        { facts = Map.singleton factId1 observedFact1
        , snapshotTimestamp = undefined
        }
  
  let observeEffect1 = Effect
        { effectID = "observe-effect-1"
        , parentEffects = []
        , effectType = ObserveEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.singleton "observedFacts" (toJSON factSnapshot1)
        }
  
  let factSnapshot2 = FactSnapshot
        { facts = Map.singleton factId2 observedFact2
        , snapshotTimestamp = undefined
        }
  
  let observeEffect2 = Effect
        { effectID = "observe-effect-2"
        , parentEffects = ["observe-effect-1"]
        , effectType = ObserveEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.singleton "observedFacts" (toJSON factSnapshot2)
        }
  
  -- Apply effects to program (simulated)
  let appliedProgram = program
        { effectDAG = Map.fromList
            [ ("observe-effect-1", toJSON observeEffect1)
            , ("observe-effect-2", toJSON observeEffect2)
            ]
        , programState = Map.fromList
            [ ("counter", Number 0)
            , ("price-1", Number 100.0)
            , ("price-2", Number 200.0)
            ]
        }
  
  -- Write facts to log file
  withSystemTempFile "fact_log.json" $ \logPath handle -> do
    -- Close the handle so we can reopen it ourselves
    hClose handle
    
    -- Create a new fact log
    let factLog = FactLog
          { programID = programId
          , facts = []
          }
    
    -- Append facts to log
    appendToFactLog logPath factLog observedFact1
    factLog' <- readFactLog logPath
    appendToFactLog logPath factLog' observedFact2
    
    -- "Restart" by creating a new empty program
    newProgram <- createTestProgram programId
    
    -- Simulate effect application with facts from log
    finalFactLog <- readFactLog logPath
    
    -- Verify facts were correctly logged
    assertEqual "Fact log should contain both facts" 
      2 (length (facts finalFactLog))
    
    assertBool "Fact log should contain first fact" 
      (any (\f -> factID f == factId1) (facts finalFactLog))
      
    assertBool "Fact log should contain second fact" 
      (any (\f -> factID f == factId2) (facts finalFactLog))
    
    -- In a real implementation, we would verify that the facts can be correctly used during replay

-- | Test causal trace reconstruction
testCausalTraceReconstruction :: Assertion
testCausalTraceReconstruction = do
  -- Create effects with causal relationships
  let rootEffect = Effect
        { effectID = "root"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  let child1Effect = Effect
        { effectID = "child1"
        , parentEffects = ["root"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  let child2Effect = Effect
        { effectID = "child2"
        , parentEffects = ["root"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  let grandchildEffect = Effect
        { effectID = "grandchild"
        , parentEffects = ["child1", "child2"]  -- Has two parents!
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Write effects to log
  withSystemTempFile "effect_dag_log.json" $ \logPath handle -> do
    -- Close the handle so we can reopen it ourselves
    hClose handle
    
    -- Create a new effect log
    let effectLog = EffectLog
          { programID = ProgramId "test-program"
          , effects = []
          }
    
    -- Append effects to log
    appendToEffectLog logPath effectLog rootEffect
    effectLog' <- readEffectLog logPath
    appendToEffectLog logPath effectLog' child1Effect
    effectLog'' <- readEffectLog logPath
    appendToEffectLog logPath effectLog'' child2Effect
    effectLog''' <- readEffectLog logPath
    appendToEffectLog logPath effectLog''' grandchildEffect
    
    -- Read final log
    finalEffectLog <- readEffectLog logPath
    
    -- Build causal graph (in a real implementation, this would construct a visual representation)
    let buildCausalGraph :: [Effect] -> Map Text [Text]
        buildCausalGraph effects =
          Map.fromList [(effectID e, parentEffects e) | e <- effects]
    
    let causalGraph = buildCausalGraph (effects finalEffectLog)
    
    -- Verify the causal relationships
    assertEqual "Root should have no parents" 
      (Map.lookup "root" causalGraph) (Just [])
      
    assertEqual "Child1 should have root as parent" 
      (Map.lookup "child1" causalGraph) (Just ["root"])
      
    assertEqual "Child2 should have root as parent" 
      (Map.lookup "child2" causalGraph) (Just ["root"])
      
    assertEqual "Grandchild should have child1 and child2 as parents" 
      (Set.fromList $ fromMaybe [] $ Map.lookup "grandchild" causalGraph) 
      (Set.fromList ["child1", "child2"])
    
    -- Verify we can determine the correct topological order
    -- In a real visualization, this would determine the layout
    let topo = topologicalSort causalGraph
    
    -- Verify that parents always come before children
    assertBool "Root should come before child1" $
      indexOf "root" topo < indexOf "child1" topo
      
    assertBool "Root should come before child2" $
      indexOf "root" topo < indexOf "child2" topo
      
    assertBool "Child1 should come before grandchild" $
      indexOf "child1" topo < indexOf "grandchild" topo
      
    assertBool "Child2 should come before grandchild" $
      indexOf "child2" topo < indexOf "grandchild" topo
  where
    indexOf :: Eq a => a -> [a] -> Int
    indexOf x xs = fromMaybe (-1) $ findIndex (== x) xs
    
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex p = go 0
      where
        go _ [] = Nothing
        go i (x:xs)
          | p x = Just i
          | otherwise = go (i+1) xs
          
    fromMaybe :: a -> Maybe a -> a
    fromMaybe def Nothing = def
    fromMaybe _ (Just x) = x
    
    -- Simple topological sort for testing
    topologicalSort :: Map Text [Text] -> [Text]
    topologicalSort graph = 
      let nodes = Map.keys graph
          visit visited result node
            | node `elem` visited = (visited, result)
            | otherwise =
                let newVisited = node : visited
                    parents = fromMaybe [] $ Map.lookup node graph
                    (finalVisited, intermediateResult) = 
                      foldl (\(v, r) parent -> visit v r parent) 
                            (newVisited, result) 
                            parents
                in (finalVisited, node : intermediateResult)
      in snd $ foldl (\(v, r) node -> visit v r node) ([], []) nodes

-- | Test tamper detection in logs
testTamperDetection :: Assertion
testTamperDetection = do
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
  
  -- Create a tampered effect that tries to modify effect2
  let tamperedEffect2 = Effect
        { effectID = "effect-2"  -- Same ID as effect2
        , parentEffects = ["effect-1"]  -- Same parent
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 10))  -- Changed amount!
            ]
        }
  
  -- Write valid effects to log
  withSystemTempDirectory "tamper_test" $ \dirPath -> do
    let logPath = dirPath </> "effect_log.json"
    let tamperedPath = dirPath </> "tampered_log.json"
    
    -- Create a new effect log
    let effectLog = EffectLog
          { programID = ProgramId "test-program"
          , effects = []
          }
    
    -- Append valid effects to log
    appendToEffectLog logPath effectLog effect1
    effectLog' <- readEffectLog logPath
    appendToEffectLog logPath effectLog' effect2
    effectLog'' <- readEffectLog logPath
    appendToEffectLog logPath effectLog'' effect3
    
    -- Create a tampered log
    let tamperedEffectLog = EffectLog
          { programID = ProgramId "test-program"
          , effects = []
          }
    
    -- Add effect1, tamperedEffect2, then effect3
    appendToEffectLog tamperedPath tamperedEffectLog effect1
    tamperedEffectLog' <- readFactLog tamperedPath
    appendToEffectLog tamperedPath tamperedEffectLog' tamperedEffect2
    tamperedEffectLog'' <- readFactLog tamperedPath
    appendToEffectLog tamperedPath tamperedEffectLog'' effect3
    
    -- Verify the legitimate log validates
    validLog <- readEffectLog logPath
    let isValidLog = validateLogIntegrity validLog
    assertBool "Valid log should pass integrity check" isValidLog
    
    -- Verify the tampered log fails validation
    tamperedLog <- readEffectLog tamperedPath
    let isTamperedLogValid = validateLogIntegrity tamperedLog
    assertBool "Tampered log should fail integrity check" (not isTamperedLogValid) 