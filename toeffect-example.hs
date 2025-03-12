#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Simple type definitions for demonstration purposes
newtype ResourceId = ResourceId ByteString deriving (Show, Eq)
newtype ProgramId = ProgramId ByteString deriving (Show, Eq)
newtype TimelineId = TimelineId ByteString deriving (Show, Eq)

-- TEL internal effect types
data TELDepositEffect = TELDepositEffect ResourceId Integer ProgramId
  deriving (Show, Eq)
data TELWithdrawEffect = TELWithdrawEffect ResourceId Integer ProgramId
  deriving (Show, Eq)
data TELTransferEffect = TELTransferEffect ResourceId ProgramId ProgramId Integer
  deriving (Show, Eq)
data TELObserveEffect = TELObserveEffect TimelineId ByteString
  deriving (Show, Eq)
data TELEmitEffect = TELEmitEffect ByteString
  deriving (Show, Eq)
data TELInvokeEffect = TELInvokeEffect ByteString
  deriving (Show, Eq)

-- Core effect types for TEL
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

-- Effect type used in the core system
data Effect
  = DepositEffect ResourceId Integer ProgramId
  | WithdrawEffect ResourceId Integer ProgramId
  | TransferEffect ResourceId Integer ProgramId ProgramId
  | TimelineEffect TimelineId ByteString
  | ProgramEffect ProgramId ByteString
  | CompositeEffect [Effect]
  deriving (Show, Eq)

-- Convert TEL CoreEffect to Core.Effect
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

-- Test cases
testSequenceEffect :: IO ()
testSequenceEffect = do
  let rid1 = ResourceId "resource1"
      pid1 = ProgramId "program1"
      rid2 = ResourceId "resource2"
      pid2 = ProgramId "program2"
  
  let effect1 = DepositEff (TELDepositEffect rid1 100 pid1)
      effect2 = WithdrawEff (TELWithdrawEffect rid2 200 pid2)
      compositeEff = toEffect (SequenceEff effect1 effect2)
      expected = CompositeEffect [
                  DepositEffect rid1 100 pid1,
                  WithdrawEffect rid2 200 pid2
                 ]
  
  putStrLn "Testing SequenceEff conversion:"
  putStrLn $ "Input: " ++ show (SequenceEff effect1 effect2)
  putStrLn $ "Result: " ++ show compositeEff
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Correct: " ++ show (compositeEff == expected)
  putStrLn ""

testParallelEffect :: IO ()
testParallelEffect = do
  let rid1 = ResourceId "resource1"
      pid1 = ProgramId "program1"
      rid2 = ResourceId "resource2"
      pid2 = ProgramId "program2"
  
  let effect1 = DepositEff (TELDepositEffect rid1 100 pid1)
      effect2 = WithdrawEff (TELWithdrawEffect rid2 200 pid2)
      compositeEff = toEffect (ParallelEff effect1 effect2)
      expected = CompositeEffect [
                  DepositEffect rid1 100 pid1,
                  WithdrawEffect rid2 200 pid2
                 ]
  
  putStrLn "Testing ParallelEff conversion:"
  putStrLn $ "Input: " ++ show (ParallelEff effect1 effect2)
  putStrLn $ "Result: " ++ show compositeEff
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Correct: " ++ show (compositeEff == expected)
  putStrLn ""

testChoiceEffect :: IO ()
testChoiceEffect = do
  let rid1 = ResourceId "resource1"
      pid1 = ProgramId "program1"
      rid2 = ResourceId "resource2"
      pid2 = ProgramId "program2"
  
  let effect1 = DepositEff (TELDepositEffect rid1 100 pid1)
      effect2 = WithdrawEff (TELWithdrawEffect rid2 200 pid2)
      compositeEff = toEffect (ChoiceEff effect1 effect2)
      expected = CompositeEffect [
                  DepositEffect rid1 100 pid1,
                  WithdrawEffect rid2 200 pid2
                 ]
  
  putStrLn "Testing ChoiceEff conversion:"
  putStrLn $ "Input: " ++ show (ChoiceEff effect1 effect2)
  putStrLn $ "Result: " ++ show compositeEff
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Correct: " ++ show (compositeEff == expected)
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Testing toEffect function implementation for composite effects"
  putStrLn "==========================================================="
  putStrLn ""
  testSequenceEffect
  testParallelEffect
  testChoiceEffect
  putStrLn "All tests completed." 