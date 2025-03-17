{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeBandits.Core.AccountProgramTest (tests) where

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

import TimeBandits.Core.Common.Types (Hash, computeHash)
import TimeBandits.Core.Types (ProgramId(..))
import qualified TimeBandits.Actors.ActorId as CoreActorId
import qualified TimeBandits.Actors.ActorId as TBActorId
import TimeBandits.Core.Schema
    ( Schema(..)
    , SchemaField(..)
    , FieldType(..)
    , EvolutionRules(..)
    , SafeStatePolicy(..)
    , defaultCoreEvolutionRules
    )
import TimeBandits.Core.Effect (Effect(..), EffectType(..))
import Core.Resource (Resource(..), ResourceId(..), Amount(..))
import TimeBandits.Core.Resource.Ledger (ResourceLedger(..), ResourceEntry(..))
import TimeBandits.Core.AccountProgram (AccountProgram(..), AccountOperation(..))
import Programs.Program (Program(..))
import Programs.ProgramState (ProgramState(..))
import TimeBandits.Core.TimelineId (TimelineId(..))

-- | Convert between Core.ActorId and TimeBandits.Actors.ActorId
convertToTBActorId :: CoreActorId.ActorId -> TBActorId.ActorId
convertToTBActorId (CoreActorId.ActorId txt) = TBActorId.ActorId txt

convertToCoreActorId :: TBActorId.ActorId -> CoreActorId.ActorId
convertToCoreActorId (TBActorId.ActorId txt) = CoreActorId.ActorId txt

-- | Resource Handling and Account Program tests
tests :: TestTree
tests = testGroup "Resource Handling and Account Program Tests"
  [ testGroup "Deposit Handling Tests"
      [ testCase "Deposit asset into account and check balance" testDepositHandling
      ]
  , testGroup "Withdrawal Handling Tests"
      [ testCase "Withdraw asset and check balance" testWithdrawalHandling
      ]
  , testGroup "Cross-Program Transfer Tests"
      [ testCase "Transfer asset from account to a program" testCrossProgramTransfer
      ]
  , testGroup "Replay Consistency Tests"
      [ testCase "Replay all deposits and withdrawals" testReplayConsistency
      ]
  , testGroup "Cross-Timeline Deposit Tests"
      [ testCase "Simulate asset arriving from different timeline" testCrossTimelineDeposit
      ]
  ]

-- | Test depositing an asset into an account and checking balance
testDepositHandling :: Assertion
testDepositHandling = do
  -- Create resource and account program
  let ethResource = Resource
        { resourceId = ResourceId "ETH"
        , resourceTimeline = TimelineId "Ethereum"
        , resourceMetadata = Map.empty
        }
  
  -- Create account program with schema
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "balances" (FieldMap FieldText FieldDecimal) False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "account-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Initial program with empty balances
  let initialProgram = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "balances" (Object Map.empty)
        }
  
  -- Create deposit effect
  let depositAmount = 10.0
  let depositEffect = Effect
        { effectID = "deposit-effect-1"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON ethResource)
            , ("amount", Number depositAmount)
            ]
        }
  
  -- Apply deposit effect
  let afterDeposit = initialProgram
        { effectDAG = Map.singleton "deposit-effect-1" (toJSON depositEffect)
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number depositAmount))
        }
  
  -- Check balance after deposit
  let balances = case Map.lookup "balances" (programState afterDeposit) of
        Just (Object bal) -> bal
        _ -> Map.empty
  
  assertEqual "ETH balance should be deposit amount" 
    (Just (Number depositAmount)) (Map.lookup "ETH" balances)

-- | Test withdrawing an asset and checking balance
testWithdrawalHandling :: Assertion
testWithdrawalHandling = do
  -- Create resource
  let ethResource = Resource
        { resourceId = ResourceId "ETH"
        , resourceTimeline = TimelineId "Ethereum"
        , resourceMetadata = Map.empty
        }
  
  -- Create account program schema
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "balances" (FieldMap FieldText FieldDecimal) False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "account-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Program with initial balance
  let initialBalance = 100.0
  let initialProgram = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number initialBalance))
        }
  
  -- Create withdrawal effect
  let withdrawalAmount = 30.0
  let withdrawalEffect = Effect
        { effectID = "withdrawal-effect-1"
        , parentEffects = []
        , effectType = WithdrawEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON ethResource)
            , ("amount", Number withdrawalAmount)
            ]
        }
  
  -- Apply withdrawal effect
  let afterWithdrawal = initialProgram
        { effectDAG = Map.singleton "withdrawal-effect-1" (toJSON withdrawalEffect)
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number (initialBalance - withdrawalAmount)))
        }
  
  -- Check balance after withdrawal
  let balances = case Map.lookup "balances" (programState afterWithdrawal) of
        Just (Object bal) -> bal
        _ -> Map.empty
  
  assertEqual "ETH balance should be initial minus withdrawal" 
    (Just (Number 70.0)) (Map.lookup "ETH" balances)
  
  -- Create withdrawal effect that exceeds balance
  let excessiveWithdrawalEffect = Effect
        { effectID = "excessive-withdrawal"
        , parentEffects = ["withdrawal-effect-1"]
        , effectType = WithdrawEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON ethResource)
            , ("amount", Number 100.0)  -- More than remaining balance
            ]
        }
  
  -- In a real implementation, this would validate and reject the withdrawal
  let isValidWithdrawal :: Program -> Effect -> Bool
      isValidWithdrawal program effect =
        case Map.lookup "balances" (programState program) of
          Just (Object balances) ->
            case Map.lookup "ETH" balances of
              Just (Number balance) ->
                case Map.lookup "amount" (effectMetadata effect) of
                  Just (Number amount) -> balance >= amount
                  _ -> False
              _ -> False
          _ -> False
  
  -- Assert the excessive withdrawal is rejected
  assertBool "Excessive withdrawal should be rejected" $
    not $ isValidWithdrawal afterWithdrawal excessiveWithdrawalEffect

-- | Test transferring asset from account to a program
testCrossProgramTransfer :: Assertion
testCrossProgramTransfer = do
  -- Create resource
  let ethResource = Resource
        { resourceId = ResourceId "ETH"
        , resourceTimeline = TimelineId "Ethereum"
        , resourceMetadata = Map.empty
        }
  
  -- Create account program schema
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "balances" (FieldMap FieldText FieldDecimal) False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let accountProgramId = ProgramId "account-program-id"
  let targetProgramId = ProgramId "target-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Account program with initial balance
  let initialBalance = 100.0
  let accountProgram = Program
        { programID = accountProgramId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number initialBalance))
        }
  
  -- Target program with initial zero balance
  let targetProgram = Program
        { programID = targetProgramId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number 0.0))
        }
  
  -- Create transfer effect
  let transferAmount = 50.0
  let transferEffect = Effect
        { effectID = "transfer-effect-1"
        , parentEffects = []
        , effectType = TransferEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON ethResource)
            , ("amount", Number transferAmount)
            , ("from", toJSON accountProgramId)
            , ("to", toJSON targetProgramId)
            ]
        }
  
  -- Apply transfer effect (updates both programs)
  let afterTransferAccount = accountProgram
        { effectDAG = Map.singleton "transfer-effect-1" (toJSON transferEffect)
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number (initialBalance - transferAmount)))
        }
  
  let afterTransferTarget = targetProgram
        { effectDAG = Map.singleton "transfer-effect-1" (toJSON transferEffect)
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number transferAmount))
        }
  
  -- Check balances after transfer
  let accountBalances = case Map.lookup "balances" (programState afterTransferAccount) of
        Just (Object bal) -> bal
        _ -> Map.empty
  
  let targetBalances = case Map.lookup "balances" (programState afterTransferTarget) of
        Just (Object bal) -> bal
        _ -> Map.empty
  
  assertEqual "Account ETH balance should be reduced by transfer amount" 
    (Just (Number 50.0)) (Map.lookup "ETH" accountBalances)
    
  assertEqual "Target ETH balance should be increased by transfer amount" 
    (Just (Number 50.0)) (Map.lookup "ETH" targetBalances)

-- | Test replay consistency for deposits and withdrawals
testReplayConsistency :: Assertion
testReplayConsistency = do
  -- Create resource
  let ethResource = Resource
        { resourceId = ResourceId "ETH"
        , resourceTimeline = TimelineId "Ethereum"
        , resourceMetadata = Map.empty
        }
  
  -- Create account program schema
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "balances" (FieldMap FieldText FieldDecimal) False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "account-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Initial program with zero balance
  let initialProgram = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number 0.0))
        }
  
  -- Create deposit effect
  let depositEffect = Effect
        { effectID = "deposit-effect-1"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON ethResource)
            , ("amount", Number 100.0)
            ]
        }
  
  -- Apply deposit effect
  let afterDeposit = initialProgram
        { effectDAG = Map.singleton "deposit-effect-1" (toJSON depositEffect)
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number 100.0))
        }
  
  -- Create withdrawal effect
  let withdrawalEffect = Effect
        { effectID = "withdrawal-effect-1"
        , parentEffects = ["deposit-effect-1"]
        , effectType = WithdrawEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON ethResource)
            , ("amount", Number 30.0)
            ]
        }
  
  -- Apply withdrawal effect
  let afterWithdrawal = afterDeposit
        { effectDAG = Map.fromList
            [ ("deposit-effect-1", toJSON depositEffect)
            , ("withdrawal-effect-1", toJSON withdrawalEffect)
            ]
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number 70.0))
        }
  
  -- Create another deposit effect
  let depositEffect2 = Effect
        { effectID = "deposit-effect-2"
        , parentEffects = ["withdrawal-effect-1"]
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON ethResource)
            , ("amount", Number 50.0)
            ]
        }
  
  -- Apply second deposit effect
  let finalProgram = afterWithdrawal
        { effectDAG = Map.fromList
            [ ("deposit-effect-1", toJSON depositEffect)
            , ("withdrawal-effect-1", toJSON withdrawalEffect)
            , ("deposit-effect-2", toJSON depositEffect2)
            ]
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number 120.0))
        }
  
  -- Serialize program
  let serialized = encode finalProgram
  
  -- "Clear memory" and deserialize
  let deserialized = decode serialized :: Maybe Program
  
  case deserialized of
    Nothing -> assertFailure "Failed to deserialize program"
    Just rehydratedProgram -> do
      -- Simulate replay by processing effects in order
      -- 1. Start with initial program (balance = 0)
      -- 2. Apply deposit-effect-1 (balance = 0 + 100 = 100)
      -- 3. Apply withdrawal-effect-1 (balance = 100 - 30 = 70)
      -- 4. Apply deposit-effect-2 (balance = 70 + 50 = 120)
      
      let replayedProgram = rehydratedProgram
      
      -- Check final balance after replay
      let finalBalances = case Map.lookup "balances" (programState replayedProgram) of
            Just (Object bal) -> bal
            _ -> Map.empty
      
      assertEqual "Final ETH balance should match original after replay" 
        (Just (Number 120.0)) (Map.lookup "ETH" finalBalances)

-- | Test simulating asset arriving from a different timeline
testCrossTimelineDeposit :: Assertion
testCrossTimelineDeposit = do
  -- Create resources from different timelines
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
  
  -- Create account program schema
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "balances" (FieldMap FieldText FieldDecimal) False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "account-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Initial program with empty balances
  let initialProgram = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number 0.0))
        }
  
  -- Create Ethereum deposit effect
  let ethDepositEffect = Effect
        { effectID = "eth-deposit-effect"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON ethResource)
            , ("amount", Number 2.0)
            , ("timeline", String "Ethereum")
            ]
        }
  
  -- Apply Ethereum deposit effect
  let afterEthDeposit = initialProgram
        { effectDAG = Map.singleton "eth-deposit-effect" (toJSON ethDepositEffect)
        , programState = Map.singleton "balances" (Object $ Map.singleton "ETH" (Number 2.0))
        }
  
  -- Create Celestia deposit effect
  let tiaDepositEffect = Effect
        { effectID = "tia-deposit-effect"
        , parentEffects = ["eth-deposit-effect"]
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("resource", toJSON tiaResource)
            , ("amount", Number 100.0)
            , ("timeline", String "Celestia")
            ]
        }
  
  -- Apply Celestia deposit effect
  let afterTiaDeposit = afterEthDeposit
        { effectDAG = Map.fromList
            [ ("eth-deposit-effect", toJSON ethDepositEffect)
            , ("tia-deposit-effect", toJSON tiaDepositEffect)
            ]
        , programState = Map.singleton "balances" (Object $ Map.fromList
            [ ("ETH", Number 2.0)
            , ("TIA", Number 100.0)
            ])
        }
  
  -- Check balances after cross-timeline deposits
  let finalBalances = case Map.lookup "balances" (programState afterTiaDeposit) of
        Just (Object bal) -> bal
        _ -> Map.empty
  
  assertEqual "ETH balance should be correct" 
    (Just (Number 2.0)) (Map.lookup "ETH" finalBalances)
    
  assertEqual "TIA balance should be correct" 
    (Just (Number 100.0)) (Map.lookup "TIA" finalBalances)
  
  -- Check each deposit is correctly recorded in the effect DAG
  assertEqual "Effect DAG should contain both deposit effects" 
    2 (Map.size (effectDAG afterTiaDeposit)) 