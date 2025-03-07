{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visualization.VisualizerTest (tests) where

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
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import System.FilePath ((</>))
import System.IO (hClose)
import Control.Monad (forM_)
import qualified Data.ByteString as SBS

import Core.Common (Hash, computeHash)
import Core.Types (ProgramId(..), FactId(..), FactValue(..), FactSnapshot(..), ObservedFact(..), ObservationProof(..))
import Core.Schema
    ( Schema(..)
    , SchemaField(..)
    , FieldType(..)
    , EvolutionRules(..)
    , SafeStatePolicy(..)
    , defaultCoreEvolutionRules
    , SchemaEvolution(..)
    , SchemaChange(..)
    )
import Core.Effect (Effect(..), EffectType(..))
import Programs.Program (Program(..))
import Programs.ProgramState (ProgramState(..))
import Visualization.ProgramVisualizer (visualizeProgram, visualizationFormats)
import Visualization.EffectDAGVisualizer (visualizeEffectDAG, exportToGraphFormat)
import Visualization.FactVisualizer (visualizeFactFlow)
import Visualization.SchemaVisualizer (visualizeSchemaEvolution)

-- | Visualization and Developer Tools tests
tests :: TestTree
tests = testGroup "Visualization and Developer Tools Tests"
  [ testGroup "Single Program Visualization Tests"
      [ testCase "Generate DAG visualization for program with 5 effects" testSingleProgramVisualization
      ]
  , testGroup "Cross-Program Visualization Tests"
      [ testCase "Show full causality across multiple programs" testCrossProgramVisualization
      ]
  , testGroup "Fact Observation Visualization Tests"
      [ testCase "Show facts entering program state over time" testFactObservationVisualization
      ]
  , testGroup "Schema Evolution Visualization Tests"
      [ testCase "Show schema versions alongside effects" testSchemaEvolutionVisualization
      ]
  ]

-- | Helper function to create a test program with effects
createTestProgramWithEffects :: ProgramId -> Int -> IO Program
createTestProgramWithEffects programId numEffects = do
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Create a chain of numEffects effects
  let effects = createEffectChain numEffects
  
  -- Build effect DAG
  let effectDAG = Map.fromList [(effectID e, toJSON e) | e <- effects]
  
  -- Calculate final counter value (each effect adds 1)
  let finalCounter = fromIntegral numEffects
  
  return Program
    { programID = programId
    , version = version
    , protocolVersion = protocolVersion
    , schema = schema
    , safeStatePolicy = AlwaysSafe
    , effectDAG = effectDAG
    , programState = Map.singleton "counter" (Number finalCounter)
    }
  where
    createEffectChain :: Int -> [Effect]
    createEffectChain n = go n []
      where
        go 0 _ = []
        go i parents = 
          let effectId = "effect-" <> T.pack (show i)
              effect = Effect
                { effectID = effectId
                , parentEffects = if null parents then [] else [head parents]
                , effectType = CallEffect
                , effectTimestamp = undefined
                , effectMetadata = Map.fromList
                    [ ("function", String "incrementCounter")
                    , ("parameters", Object $ Map.singleton "amount" (Number 1))
                    ]
                }
          in effect : go (i-1) (effectId : parents)

-- | Test generating a visualization for a single program
testSingleProgramVisualization :: Assertion
testSingleProgramVisualization = do
  -- Create a program with 5 effects
  let programId = ProgramId "test-program"
  program <- createTestProgramWithEffects programId 5
  
  -- Generate a visualization
  let visualization = visualizeProgram program
  
  -- Check the visualization contains all effects
  forM_ [1..5] $ \i -> do
    let effectId = "effect-" <> T.pack (show i)
    assertBool ("Visualization should include " <> T.unpack effectId) $
      T.isInfixOf effectId visualization
  
  -- Check visualization includes program ID
  assertBool "Visualization should include program ID" $
    T.isInfixOf (T.pack $ show programId) visualization
  
  -- Verify we can export to different formats
  forM_ visualizationFormats $ \format -> do
    let exportedVisual = visualizeProgram program format
    assertBool ("Visualization in " <> T.unpack format <> " format should not be empty") $
      not (T.null exportedVisual)

-- | Test generating a visualization for multiple programs with cross-program causality
testCrossProgramVisualization :: Assertion
testCrossProgramVisualization = do
  -- Create two programs
  let programAId = ProgramId "program-a"
  let programBId = ProgramId "program-b"
  
  -- Create base programs
  programA <- createTestProgramWithEffects programAId 2  -- Has effect-1, effect-2
  programB <- createTestProgramWithEffects programBId 0  -- Empty initially
  
  -- Create cross-program effects
  
  -- A calls B
  let crossCallEffect = Effect
        { effectID = "cross-call"
        , parentEffects = ["effect-1"]  -- Depends on effect-1 in program A
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "remoteCall")
            , ("targetProgram", toJSON programBId)
            ]
        }
  
  -- B processes the call
  let processEffect = Effect
        { effectID = "process-call"
        , parentEffects = []  -- First effect in program B
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "processCall")
            , ("sourceProgram", toJSON programAId)
            , ("sourceEffect", String "cross-call")
            ]
        }
  
  -- B returns result to A
  let returnEffect = Effect
        { effectID = "return-result"
        , parentEffects = ["process-call"]  -- Depends on B processing
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "returnResult")
            , ("targetProgram", toJSON programAId)
            , ("value", Number 42)
            ]
        }
  
  -- A processes the result
  let processResultEffect = Effect
        { effectID = "process-result"
        , parentEffects = ["cross-call"]  -- Depends on the original cross-call
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "processResult")
            , ("sourceProgram", toJSON programBId)
            , ("sourceEffect", String "return-result")
            , ("result", Number 42)
            ]
        }
  
  -- Update program states
  let programA' = programA
        { effectDAG = Map.insert "cross-call" (toJSON crossCallEffect) $
                      Map.insert "process-result" (toJSON processResultEffect) $
                      effectDAG programA
        }
  
  let programB' = programB
        { effectDAG = Map.insert "process-call" (toJSON processEffect) $
                      Map.insert "return-result" (toJSON returnEffect) $
                      effectDAG programB
        }
  
  -- Generate a cross-program visualization
  let programs = Map.fromList
        [ (programAId, programA')
        , (programBId, programB')
        ]
  
  -- Visualize the cross-program causality
  let visualization = visualizeEffectDAG programs
  
  -- Verify visualization contains all programs and effects
  assertBool "Visualization should include Program A" $
    T.isInfixOf (T.pack $ show programAId) visualization
    
  assertBool "Visualization should include Program B" $
    T.isInfixOf (T.pack $ show programBId) visualization
  
  -- Check for cross-program causality links
  assertBool "Visualization should show link from cross-call to process-call" $
    T.isInfixOf "cross-call" visualization && T.isInfixOf "process-call" visualization
    
  assertBool "Visualization should show link from process-call to return-result" $
    T.isInfixOf "process-call -> return-result" visualization || 
    T.isInfixOf "process-call\" -> \"return-result" visualization
    
  assertBool "Visualization should show link from return-result to process-result" $
    T.isInfixOf "return-result" visualization && T.isInfixOf "process-result" visualization
  
  -- Export to graph format (e.g., DOT)
  let graphFormat = exportToGraphFormat programs
  assertBool "Graph format export should not be empty" $
    not (T.null graphFormat)

-- | Test generating a visualization for fact observations over time
testFactObservationVisualization :: Assertion
testFactObservationVisualization = do
  -- Create a test program
  let programId = ProgramId "test-program"
  program <- createTestProgramWithEffects programId 0  -- Start with empty program
  
  -- Create facts to observe
  let createFact i = 
        let factId = FactId $ "price-fact-" <> T.pack (show i)
            factValue = NumberFact (fromIntegral i * 100)
            observationProof = ObservationProof
              { proofSource = "oracle-" <> T.pack (show i)
              , proofSignature = "valid-signature-" <> T.pack (show i)
              , proofMetadata = Map.empty
              }
        in ObservedFact
            { factID = factId
            , factValue = factValue
            , observationProof = observationProof
            }
  
  -- Create series of fact observations
  let createObservation i prevEffectId =
        let factId = "price-fact-" <> T.pack (show i)
            fact = createFact i
            factSnapshot = FactSnapshot
              { facts = Map.singleton (FactId factId) fact
              , snapshotTimestamp = undefined
              }
            effectId = "observe-" <> T.pack (show i)
            parents = if T.null prevEffectId then [] else [prevEffectId]
        in (effectId, Effect
              { effectID = effectId
              , parentEffects = parents 
              , effectType = ObserveEffect
              , effectTimestamp = undefined
              , effectMetadata = Map.singleton "observedFacts" (toJSON factSnapshot)
              })
  
  -- Create 5 observations in sequence
  let observations = scanl 
        (\(prevId, _) i -> createObservation i prevId) 
        ("", undefined) 
        [1..5]
  
  -- Build effect DAG and extract just the effects
  let effects = map snd $ tail observations  -- Skip the initial empty tuple
  let effectDAG = Map.fromList [(effectID e, toJSON e) | e <- effects]
  
  -- Update program with observations
  let programWithObs = program { effectDAG = effectDAG }
  
  -- Generate fact flow visualization
  let visualization = visualizeFactFlow programWithObs
  
  -- Verify visualization contains all fact IDs
  forM_ [1..5] $ \i -> do
    let factId = "price-fact-" <> T.pack (show i)
    assertBool ("Visualization should include fact " <> T.unpack factId) $
      T.isInfixOf factId visualization
  
  -- Verify visualization shows progression over time
  assertBool "Visualization should show temporal progression" $
    T.isInfixOf "Timeline" visualization || T.isInfixOf "time" visualization
  
  -- Verify visualization includes fact values
  forM_ [1..5] $ \i -> do
    let factValue = T.pack (show (i * 100))
    assertBool ("Visualization should include fact value " <> T.unpack factValue) $
      T.isInfixOf factValue visualization

-- | Test generating a visualization for schema evolution
testSchemaEvolutionVisualization :: Assertion
testSchemaEvolutionVisualization = do
  -- Create initial schema
  let initialSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Create program with initial schema
  let programId = ProgramId "test-program"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Create an initial effect
  let initialEffect = Effect
        { effectID = "initial-effect"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 1))
            ]
        }
  
  let initialProgram = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = initialSchema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.singleton "initial-effect" (toJSON initialEffect)
        , programState = Map.singleton "counter" (Number 1)
        }
  
  -- Create first schema evolution
  let evolution1 = SchemaEvolution
        { fromVersion = mkVersion [1, 0, 0]
        , toVersion = mkVersion [1, 1, 0]
        , changes = 
            [ AddField (SchemaField "description" FieldText True)
            ]
        }
  
  -- Create evolved schema
  let evolvedSchema = Schema
        { schemaVersion = mkVersion [1, 1, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            , SchemaField "description" FieldText True
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Create evolution effect
  let evolutionEffect1 = Effect
        { effectID = "evolution-effect-1"
        , parentEffects = ["initial-effect"]
        , effectType = SchemaEvolutionEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("schemaEvolution", toJSON evolution1)
            ]
        }
  
  -- Apply evolution
  let evolvedProgram1 = initialProgram
        { version = mkVersion [1, 1, 0]
        , schema = evolvedSchema
        , effectDAG = Map.fromList
            [ ("initial-effect", toJSON initialEffect)
            , ("evolution-effect-1", toJSON evolutionEffect1)
            ]
        , programState = Map.fromList
            [ ("counter", Number 1)
            , ("description", String "Added description")
            ]
        }
  
  -- Create second schema evolution
  let evolution2 = SchemaEvolution
        { fromVersion = mkVersion [1, 1, 0]
        , toVersion = mkVersion [2, 0, 0]
        , changes = 
            [ AddField (SchemaField "tags" (FieldArray FieldText) True)
            ]
        }
  
  -- Create second evolved schema
  let finalSchema = Schema
        { schemaVersion = mkVersion [2, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            , SchemaField "description" FieldText True
            , SchemaField "tags" (FieldArray FieldText) True
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Create second evolution effect
  let evolutionEffect2 = Effect
        { effectID = "evolution-effect-2"
        , parentEffects = ["evolution-effect-1"]
        , effectType = SchemaEvolutionEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("schemaEvolution", toJSON evolution2)
            ]
        }
  
  -- Apply second evolution
  let finalProgram = evolvedProgram1
        { version = mkVersion [2, 0, 0]
        , schema = finalSchema
        , effectDAG = Map.fromList
            [ ("initial-effect", toJSON initialEffect)
            , ("evolution-effect-1", toJSON evolutionEffect1)
            , ("evolution-effect-2", toJSON evolutionEffect2)
            ]
        , programState = Map.fromList
            [ ("counter", Number 1)
            , ("description", String "Added description")
            , ("tags", Array [String "test", String "example"])
            ]
        }
  
  -- Generate schema evolution visualization
  let visualization = visualizeSchemaEvolution finalProgram
  
  -- Verify visualization includes all schema versions
  assertBool "Visualization should include initial schema version" $
    T.isInfixOf "1.0.0" visualization
    
  assertBool "Visualization should include first evolved schema version" $
    T.isInfixOf "1.1.0" visualization
    
  assertBool "Visualization should include final schema version" $
    T.isInfixOf "2.0.0" visualization
  
  -- Verify visualization includes field changes
  assertBool "Visualization should show counter field" $
    T.isInfixOf "counter" visualization
    
  assertBool "Visualization should show description field addition" $
    T.isInfixOf "description" visualization && T.isInfixOf "add" visualization
    
  assertBool "Visualization should show tags field addition" $
    T.isInfixOf "tags" visualization && T.isInfixOf "add" visualization 