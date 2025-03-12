{-# LANGUAGE LambdaCase #-}
-- | Standalone test to verify toEffect implementation
module Main where

import System.IO

-- Define the necessary types for testing
data ResourceId = ResourceId String deriving (Show, Eq)
data ProgramId = ProgramId String deriving (Show, Eq)
data TimelineId = TimelineId String deriving (Show, Eq)

-- Core Effect type (simplified version)
data Effect
  = DepositEffect ResourceId Integer ProgramId
  | WithdrawEffect ResourceId Integer ProgramId
  | TransferEffect ResourceId Integer ProgramId ProgramId
  | TimelineEffect TimelineId String
  | ProgramEffect ProgramId String
  | CompositeEffect [Effect]
  deriving (Show, Eq)

-- TEL internal effect types (simplified)
data TELDepositEffect = TELDepositEffect ResourceId Integer ProgramId deriving (Show, Eq)
data TELWithdrawEffect = TELWithdrawEffect ResourceId Integer ProgramId deriving (Show, Eq)
data TELTransferEffect = TELTransferEffect ResourceId ProgramId ProgramId Integer deriving (Show, Eq)
data TELObserveEffect = TELObserveEffect TimelineId String deriving (Show, Eq)
data TELEmitEffect = TELEmitEffect String deriving (Show, Eq)
data TELInvokeEffect = TELInvokeEffect String deriving (Show, Eq)

-- Core effect type (simplified)
data CoreEffect
  = DepositEff TELDepositEffect
  | WithdrawEff TELWithdrawEffect
  | TransferEff TELTransferEffect
  | ObserveEff TELObserveEffect
  | EmitEff TELEmitEffect
  | InvokeEff TELInvokeEffect
  | SequenceEff CoreEffect CoreEffect
  | ParallelEff CoreEffect CoreEffect
  | ChoiceEff CoreEffect CoreEffect
  deriving (Show, Eq)

-- | Convert internal CoreEffect to Effect for serialization
toEffect :: CoreEffect -> Effect
toEffect = \case
  DepositEff (TELDepositEffect rid amt pid) -> DepositEffect rid amt pid
  WithdrawEff (TELWithdrawEffect rid amt pid) -> WithdrawEffect rid amt pid
  TransferEff (TELTransferEffect rid spid dpid amt) -> TransferEffect rid amt spid dpid
  ObserveEff (TELObserveEffect tid fact) -> TimelineEffect tid fact
  EmitEff (TELEmitEffect event) -> ProgramEffect (ProgramId "self") event
  InvokeEff (TELInvokeEffect program) -> ProgramEffect (ProgramId "invoke") program
  SequenceEff e1 e2 -> CompositeEffect [toEffect e1, toEffect e2]
  ParallelEff e1 e2 -> CompositeEffect [toEffect e1, toEffect e2]
  ChoiceEff e1 e2 -> CompositeEffect [toEffect e1, toEffect e2]

-- Test helpers
assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq msg expected actual =
  if expected == actual
  then putStrLn $ "PASS: " ++ msg
  else do
    putStrLn $ "FAIL: " ++ msg
    putStrLn $ "  Expected: " ++ show expected
    putStrLn $ "  Actual:   " ++ show actual

-- Test cases
testSequenceEffect :: IO ()
testSequenceEffect = do
  let effect1 = DepositEff (TELDepositEffect (ResourceId "res1") 100 (ProgramId "prog1"))
      effect2 = WithdrawEff (TELWithdrawEffect (ResourceId "res2") 200 (ProgramId "prog2"))
      compositeEff = toEffect (SequenceEff effect1 effect2)
      expected = CompositeEffect [
        DepositEffect (ResourceId "res1") 100 (ProgramId "prog1"),
        WithdrawEffect (ResourceId "res2") 200 (ProgramId "prog2")
        ]
  
  assertEq "SequenceEff converts to CompositeEffect with both effects in order" expected compositeEff

testParallelEffect :: IO ()
testParallelEffect = do
  let effect1 = DepositEff (TELDepositEffect (ResourceId "res1") 100 (ProgramId "prog1"))
      effect2 = WithdrawEff (TELWithdrawEffect (ResourceId "res2") 200 (ProgramId "prog2"))
      compositeEff = toEffect (ParallelEff effect1 effect2)
      expected = CompositeEffect [
        DepositEffect (ResourceId "res1") 100 (ProgramId "prog1"),
        WithdrawEffect (ResourceId "res2") 200 (ProgramId "prog2")
        ]
  
  assertEq "ParallelEff converts to CompositeEffect with both effects" expected compositeEff

testChoiceEffect :: IO ()
testChoiceEffect = do
  let effect1 = DepositEff (TELDepositEffect (ResourceId "res1") 100 (ProgramId "prog1"))
      effect2 = WithdrawEff (TELWithdrawEffect (ResourceId "res2") 200 (ProgramId "prog2"))
      compositeEff = toEffect (ChoiceEff effect1 effect2)
      expected = CompositeEffect [
        DepositEffect (ResourceId "res1") 100 (ProgramId "prog1"),
        WithdrawEffect (ResourceId "res2") 200 (ProgramId "prog2")
        ]
  
  assertEq "ChoiceEff converts to CompositeEffect with both effects" expected compositeEff

-- Main test runner
main :: IO ()
main = do
  putStrLn "Testing toEffect function implementation..."
  testSequenceEffect
  testParallelEffect
  testChoiceEffect
  putStrLn "All tests completed." 