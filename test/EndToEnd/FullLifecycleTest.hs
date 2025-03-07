{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EndToEnd.FullLifecycleTest (tests) where

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
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)

import Core.Common (Hash, computeHash)
import Core.Types 
    ( ProgramId(..)
    , BanditId(..)
    , FactId(..)
    , FactValue(..)
    , FactSnapshot(..)
    , ObservedFact(..)
    , ObservationProof(..)
    )
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
import Core.Resource (Resource(..), ResourceId(..), Amount(..))
import Core.TimelineId (TimelineId(..))
import Simulation.Mode (SimulationMode(..), SimulationEnvironment(..))
import Simulation.Controller 
    ( deployProgram
    , spawnBandit
    , queryState
    , applyEffect
    , createProgram
    , upgradeProgram
    )

-- | Full Lifecycle Test (End-to-End)
tests :: TestTree
tests = testGroup "Full Lifecycle Tests (End-to-End)"
  [ testCase "Complete program lifecycle from deployment to replay" testFullLifecycle
  ]

-- | End-to-end test of the full system lifecycle
testFullLifecycle :: Assertion
testFullLifecycle = do
  -- Set up simulation environment
  let simEnv = SimulationEnvironment
        { simulationMode = InMemory
        , networkLatency = 0
        , bandits = []
        , programs = Map.empty
        }
  
  -- Create traveler and deploy initial program (v1)
  let travelerId = BanditId "traveler"
  let programId = ProgramId "traveler-program"
  
  -- Define initial schema (v1)
  let v1Schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "eth_balance" FieldDecimal False
            , SchemaField "tia_balance" FieldDecimal False
            , SchemaField "trades" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Create initial program
  let initialProgram = createProgram programId v1Schema
  
  -- Deploy program to simulation
  simEnv1 <- deployProgram simEnv travelerId initialProgram
  
  -- Set up timeline resources
  let ethResource = Resource
        { resourceId = ResourceId "ETH"
        , resourceTimeline = TimelineId "Ethereum"
        , resourceMetadata = Map.empty
        }
  
  let tiaResource = Resource
        { resourceId = ResourceId "TIA"
        , resourceTimeline = TimelineId "Celestia"
        , resourceMetadata = Map.empty
        }
  
  -- Step 1: Deposit assets from Ethereum
  let ethDepositEffect = Effect
        { effectID = "eth-deposit"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON ethResource)
            , ("amount", Number 5.0)
            , ("timeline", String "Ethereum")
            ]
        }
  
  simEnv2 <- applyEffect simEnv1 travelerId programId ethDepositEffect
  
  -- Step 2: Deposit assets from Celestia
  let tiaDepositEffect = Effect
        { effectID = "tia-deposit"
        , parentEffects = ["eth-deposit"]
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON tiaResource)
            , ("amount", Number 100.0)
            , ("timeline", String "Celestia")
            ]
        }
  
  simEnv3 <- applyEffect simEnv2 travelerId programId tiaDepositEffect
  
  -- Step 3: Execute cross-program calls
  
  -- Set up a destination program (e.g., a DEX)
  let dexBanditId = BanditId "dex-operator"
  let dexProgramId = ProgramId "dex-program"
  
  -- Define DEX schema
  let dexSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "eth_reserve" FieldDecimal False
            , SchemaField "tia_reserve" FieldDecimal False
            , SchemaField "trade_count" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Create and deploy DEX program
  let dexProgram = createProgram dexProgramId dexSchema
  simEnv4 <- deployProgram simEnv3 dexBanditId dexProgram
  
  -- Initialize DEX with liquidity
  let initDexEffect = Effect
        { effectID = "init-dex"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("eth_reserve", Number 100.0)
            , ("tia_reserve", Number 2000.0)
            ]
        }
  
  simEnv5 <- applyEffect simEnv4 dexBanditId dexProgramId initDexEffect
  
  -- Execute a cross-program trade (traveler program calls DEX)
  let crossProgramTradeEffect = Effect
        { effectID = "cross-trade"
        , parentEffects = ["tia-deposit"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "executeTrade")
            , ("targetProgram", toJSON dexProgramId)
            , ("sell_token", String "ETH")
            , ("sell_amount", Number 1.0)
            , ("min_buy_amount", Number 19.0)
            ]
        }
  
  simEnv6 <- applyEffect simEnv5 travelerId programId crossProgramTradeEffect
  
  -- DEX processes the trade
  let dexTradeEffect = Effect
        { effectID = "dex-trade"
        , parentEffects = ["init-dex"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "processTrade")
            , ("sourceProgram", toJSON programId)
            , ("sourceEffect", String "cross-trade")
            , ("sell_token", String "ETH")
            , ("sell_amount", Number 1.0)
            , ("buy_token", String "TIA")
            , ("buy_amount", Number 19.6)  -- Calculated price
            ]
        }
  
  simEnv7 <- applyEffect simEnv6 dexBanditId dexProgramId dexTradeEffect
  
  -- Transfer tokens between programs
  let transferEthToDexEffect = Effect
        { effectID = "transfer-eth-to-dex"
        , parentEffects = ["cross-trade"]
        , effectType = TransferEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON ethResource)
            , ("amount", Number 1.0)
            , ("from", toJSON programId)
            , ("to", toJSON dexProgramId)
            ]
        }
  
  simEnv8 <- applyEffect simEnv7 travelerId programId transferEthToDexEffect
  
  let transferTiaToTravelerEffect = Effect
        { effectID = "transfer-tia-to-traveler"
        , parentEffects = ["dex-trade"]
        , effectType = TransferEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON tiaResource)
            , ("amount", Number 19.6)
            , ("from", toJSON dexProgramId)
            , ("to", toJSON programId)
            ]
        }
  
  simEnv9 <- applyEffect simEnv8 dexBanditId dexProgramId transferTiaToTravelerEffect
  
  -- Step 4: Observe price and derive spread
  
  -- Create a price observation fact
  let ethPriceFactId = FactId "eth-price"
  let ethPriceValue = NumberFact 2900.0
  let oracleProof = ObservationProof
        { proofSource = "oracle"
        , proofSignature = "valid-oracle-signature"
        , proofMetadata = Map.empty
        }
  
  let ethPriceFact = ObservedFact
        { factID = ethPriceFactId
        , factValue = ethPriceValue
        , observationProof = oracleProof
        }
  
  -- Create fact snapshot
  let priceFactSnapshot = FactSnapshot
        { facts = Map.singleton ethPriceFactId ethPriceFact
        , snapshotTimestamp = undefined
        }
  
  -- Create observe effect
  let observePriceEffect = Effect
        { effectID = "observe-price"
        , parentEffects = ["transfer-eth-to-dex", "transfer-tia-to-traveler"]
        , effectType = ObserveEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.singleton "observedFacts" (toJSON priceFactSnapshot)
        }
  
  simEnv10 <- applyEffect simEnv9 travelerId programId observePriceEffect
  
  -- Calculate spread based on observed price
  let calculateSpreadEffect = Effect
        { effectID = "calculate-spread"
        , parentEffects = ["observe-price"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "calculateSpread")
            , ("dex_price", Number 19.6)  -- Price from DEX: 19.6 TIA for 1 ETH
            , ("oracle_price", Number 2900.0)  -- External price: 2900 USD per ETH
            ]
        }
  
  simEnv11 <- applyEffect simEnv10 travelerId programId calculateSpreadEffect
  
  -- Step 5: Upgrade to v2 with schema change
  
  -- Define schema evolution
  let schemaEvolution = SchemaEvolution
        { fromVersion = mkVersion [1, 0, 0]
        , toVersion = mkVersion [2, 0, 0]
        , changes = 
            [ AddField (SchemaField "price_observations" (FieldArray FieldDecimal) True)
            , AddField (SchemaField "spread_history" (FieldArray FieldDecimal) True)
            ]
        }
  
  -- Create evolution effect
  let evolutionEffect = Effect
        { effectID = "evolution-effect"
        , parentEffects = ["calculate-spread"]
        , effectType = SchemaEvolutionEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("schemaEvolution", toJSON schemaEvolution)
            ]
        }
  
  -- Apply evolution
  simEnv12 <- applyEffect simEnv11 travelerId programId evolutionEffect
  
  -- Update program with new fields
  let updateV2FieldsEffect = Effect
        { effectID = "update-v2-fields"
        , parentEffects = ["evolution-effect"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "updateHistoryFields")
            , ("price_observations", Array [Number 2900.0])
            , ("spread_history", Array [Number 0.148])  -- Calculated spread
            ]
        }
  
  simEnv13 <- applyEffect simEnv12 travelerId programId updateV2FieldsEffect
  
  -- Step 6: Complete cross-program settlement
  
  -- Final trade settlement effect
  let settlementEffect = Effect
        { effectID = "settlement"
        , parentEffects = ["update-v2-fields"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "completeTrade")
            , ("trade_id", String "cross-trade")
            ]
        }
  
  simEnv14 <- applyEffect simEnv13 travelerId programId settlementEffect
  
  -- Update trade count on both programs
  let updateTravelerTradeCountEffect = Effect
        { effectID = "update-traveler-trades"
        , parentEffects = ["settlement"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementTradeCount")
            ]
        }
  
  simEnv15 <- applyEffect simEnv14 travelerId programId updateTravelerTradeCountEffect
  
  let updateDexTradeCountEffect = Effect
        { effectID = "update-dex-trades"
        , parentEffects = ["transfer-tia-to-traveler"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementTradeCount")
            ]
        }
  
  simEnv16 <- applyEffect simEnv15 dexBanditId dexProgramId updateDexTradeCountEffect
  
  -- Step 7: Query final states and verify balances
  
  -- Query traveler program state
  travelerState <- queryState simEnv16 programId
  
  -- Query DEX program state
  dexState <- queryState simEnv16 dexProgramId
  
  -- Verify traveler balances
  case Map.lookup "eth_balance" travelerState of
    Just (Number ethBalance) -> assertEqual "Traveler ETH balance should be 4.0" 4.0 ethBalance
    _ -> assertFailure "ETH balance not found or not a number"
  
  case Map.lookup "tia_balance" travelerState of
    Just (Number tiaBalance) -> assertEqual "Traveler TIA balance should be 119.6" 119.6 tiaBalance
    _ -> assertFailure "TIA balance not found or not a number"
  
  case Map.lookup "trades" travelerState of
    Just (Number trades) -> assertEqual "Traveler trade count should be 1" 1 trades
    _ -> assertFailure "Trade count not found or not a number"
  
  -- Verify v2 fields are present
  case Map.lookup "price_observations" travelerState of
    Just (Array priceObs) -> assertBool "Price observations should not be empty" (not $ null priceObs)
    _ -> assertFailure "Price observations not found or not an array"
  
  case Map.lookup "spread_history" travelerState of
    Just (Array spreadHist) -> assertBool "Spread history should not be empty" (not $ null spreadHist)
    _ -> assertFailure "Spread history not found or not an array"
  
  -- Verify DEX reserves
  case Map.lookup "eth_reserve" dexState of
    Just (Number ethReserve) -> assertEqual "DEX ETH reserve should be 101.0" 101.0 ethReserve
    _ -> assertFailure "ETH reserve not found or not a number"
  
  case Map.lookup "tia_reserve" dexState of
    Just (Number tiaReserve) -> assertEqual "DEX TIA reserve should be 1980.4" 1980.4 tiaReserve
    _ -> assertFailure "TIA reserve not found or not a number"
  
  case Map.lookup "trade_count" dexState of
    Just (Number tradeCount) -> assertEqual "DEX trade count should be 1" 1 tradeCount
    _ -> assertFailure "DEX trade count not found or not a number"
  
  -- Step 8: Test the entire lifecycle was properly recorded
  
  -- Count effects in traveler program
  let travelerEffectCount = 
        length $ filter (\(eid, _) -> T.isPrefixOf "transfer" eid || 
                                     not (T.isPrefixOf "dex" eid)) $ 
                Map.toList $ effectDAG (programs simEnv16 Map.! programId)
  
  -- Count effects in DEX program
  let dexEffectCount = 
        length $ filter (\(eid, _) -> T.isPrefixOf "dex" eid || 
                                     T.isPrefixOf "init" eid ||
                                     T.isPrefixOf "transfer" eid) $ 
                Map.toList $ effectDAG (programs simEnv16 Map.! dexProgramId)
  
  -- Verify effect counts
  assertBool "Traveler program should have at least 9 effects" (travelerEffectCount >= 9)
  assertBool "DEX program should have at least 4 effects" (dexEffectCount >= 4) 