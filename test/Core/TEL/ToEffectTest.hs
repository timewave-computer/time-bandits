{-# LANGUAGE OverloadedStrings #-}

module Core.TEL.ToEffectTest (testToEffect) where

import Test.Hspec

import Core.TEL.Interpreter (toEffect)
import Core.TEL.Interpreter (CoreEffect(..), TELDepositEffect(..), TELWithdrawEffect(..), TELTransferEffect(..))
import Core.Effect (Effect(..))
import Core.ResourceId (ResourceId(..))
import Core.ProgramId (ProgramId(..))
import Core.TimelineId (TimelineId(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE

-- | Direct test for toEffect function
testToEffect :: Spec
testToEffect = do
  describe "toEffect function" $ do
    -- Simple tests to see if toEffect is defined
    it "toEffect is defined and can be called" $ do
      let depositEff = DepositEff (TELDepositEffect (ResourceId "resource1") 100 (ProgramId "program1"))
      let result = toEffect depositEff
      case result of
        DepositEffect _ _ _ -> True `shouldBe` True -- If we got here, toEffect worked
        _ -> expectationFailure "Expected DepositEffect"
      
    it "converts a sequence of effects to CompositeEffect" $ do
      let effect1 = DepositEff (TELDepositEffect (ResourceId "resource1") 100 (ProgramId "program1"))
      let effect2 = WithdrawEff (TELWithdrawEffect (ResourceId "resource2") 200 (ProgramId "program2"))
      
      let compositeEff = toEffect (SequenceEff effect1 effect2)
      case compositeEff of
        CompositeEffect _ -> True `shouldBe` True -- If we got here, toEffect converted to CompositeEffect
        _ -> expectationFailure "Expected CompositeEffect"
      
    it "converts a parallel combination of effects to CompositeEffect" $ do
      let effect1 = DepositEff (TELDepositEffect (ResourceId "resource1") 100 (ProgramId "program1"))
      let effect2 = WithdrawEff (TELWithdrawEffect (ResourceId "resource2") 200 (ProgramId "program2"))
      
      let compositeEff = toEffect (ParallelEff effect1 effect2)
      case compositeEff of
        CompositeEffect _ -> True `shouldBe` True -- If we got here, toEffect converted to CompositeEffect
        _ -> expectationFailure "Expected CompositeEffect"
      
    it "converts a choice between effects to CompositeEffect" $ do
      let effect1 = DepositEff (TELDepositEffect (ResourceId "resource1") 100 (ProgramId "program1"))
      let effect2 = WithdrawEff (TELWithdrawEffect (ResourceId "resource2") 200 (ProgramId "program2"))
      
      let compositeEff = toEffect (ChoiceEff effect1 effect2)
      case compositeEff of
        CompositeEffect _ -> True `shouldBe` True -- If we got here, toEffect converted to CompositeEffect
        _ -> expectationFailure "Expected CompositeEffect" 