{-# LANGUAGE OverloadedStrings #-}

module Core.TEL.CompositeEffectTest (testCompositeEffects) where

import Test.Hspec
import Core.TEL.Interpreter
import Core.TEL.AST
import Core.Effect (Effect(..))
import Core.ResourceId (ResourceId(..))
import Core.ProgramId (ProgramId(..))

-- | Test suite for composite effect handling in the TEL interpreter
testCompositeEffects :: Spec
testCompositeEffects = do
  describe "Composite Effects in TEL" $ do
    describe "Sequence Effects (>>)" $ do
      it "correctly converts a sequence of deposit effects" $ do
        -- Create a sequence of two deposit effects
        let effect1 = DepositEff (TELDepositEffect (ResourceId "resource1") 100 (ProgramId "program1"))
        let effect2 = DepositEff (TELDepositEffect (ResourceId "resource2") 200 (ProgramId "program2"))
        let sequenceEff = SequenceEff effect1 effect2
        
        -- Convert to CompositeEffect
        let result = toEffect sequenceEff
        case result of
          CompositeEffect ops -> length ops `shouldBe` 2
          _ -> expectationFailure "Expected CompositeEffect"
          
    describe "Parallel Effects (<|>)" $ do
      it "correctly converts parallel effects" $ do
        -- Create parallel effects
        let effect1 = DepositEff (TELDepositEffect (ResourceId "resource1") 100 (ProgramId "program1"))
        let effect2 = WithdrawEff (TELWithdrawEffect (ResourceId "resource2") 200 (ProgramId "program2"))
        let parallelEff = ParallelEff effect1 effect2
        
        -- Convert to CompositeEffect
        let result = toEffect parallelEff
        case result of
          CompositeEffect ops -> length ops `shouldBe` 2
          _ -> expectationFailure "Expected CompositeEffect"
          
    describe "Choice Effects (<|)" $ do
      it "correctly converts choice effects" $ do
        -- Create choice effects
        let effect1 = DepositEff (TELDepositEffect (ResourceId "resource1") 100 (ProgramId "program1"))
        let effect2 = WithdrawEff (TELWithdrawEffect (ResourceId "resource2") 200 (ProgramId "program2"))
        let choiceEff = ChoiceEff effect1 effect2
        
        -- Convert to CompositeEffect
        let result = toEffect choiceEff
        case result of
          CompositeEffect ops -> length ops `shouldBe` 2
          _ -> expectationFailure "Expected CompositeEffect"
          
    describe "Nested Composite Effects" $ do
      it "correctly handles nested effect compositions" $ do
        -- Create a complex nested composition
        let deposit1 = DepositEff (TELDepositEffect (ResourceId "resource1") 100 (ProgramId "program1"))
        let deposit2 = DepositEff (TELDepositEffect (ResourceId "resource2") 200 (ProgramId "program2"))
        let withdraw1 = WithdrawEff (TELWithdrawEffect (ResourceId "resource3") 300 (ProgramId "program3"))
        
        -- Create a sequence of deposit1 followed by parallel(deposit2, withdraw1)
        let parallelInner = ParallelEff deposit2 withdraw1
        let nestedEff = SequenceEff deposit1 parallelInner
        
        -- Convert to CompositeEffect
        let result = toEffect nestedEff
        case result of
          CompositeEffect _ -> True `shouldBe` True -- Successfully converted
          _ -> expectationFailure "Expected CompositeEffect" 