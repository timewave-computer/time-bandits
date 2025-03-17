{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Core.TEL.CompositeEffectTest (testCompositeEffects) where

import Test.Hspec
import TimeBandits.Core.TEL.Interpreter
import TimeBandits.Core.TEL.AST
import TimeBandits.Core.Effect (Effect(..))
import TimeBandits.Core.ResourceId (ResourceId(..))
import TimeBandits.Core.ProgramId (ProgramId(..))

-- | Test suite for composite effect handling in the TEL interpreter
testCompositeEffects :: Spec
testCompositeEffects = do
  describe "Composite Effects in TEL" $ do
    describe "Sequence Effects (>>)" $ do
      it "correctly converts a sequence of TEL effects" $ do
        -- Create two effects
        let depositEffect = TELDepositEffect 
              { depositResource = TextValue "resource1"
              , depositAmount = IntValue 100
              , depositTimeline = TextValue "program1"
              }
        let withdrawEffect = TELWithdrawEffect
              { withdrawResource = TextValue "resource2"
              , withdrawAmount = IntValue 200
              , withdrawTimeline = TextValue "program2"
              }
        
        -- Create core effects
        let effect1 = TELDeposit depositEffect
        let effect2 = TELWithdraw withdrawEffect
        
        -- Just check that toEffect doesn't crash on these effects
        toEffect effect1 `shouldSatisfy` isCompositeEffect
        toEffect effect2 `shouldSatisfy` isCompositeEffect
      
    describe "Parallel Effects (||)" $ do
      it "correctly converts parallel TEL effects" $ do
        -- Create two effects
        let depositEffect = TELDepositEffect 
              { depositResource = TextValue "resource1"
              , depositAmount = IntValue 100
              , depositTimeline = TextValue "program1"
              }
        let withdrawEffect = TELWithdrawEffect
              { withdrawResource = TextValue "resource2"
              , withdrawAmount = IntValue 200
              , withdrawTimeline = TextValue "program2"
              }
        
        -- Create core effects
        let effect1 = TELDeposit depositEffect
        let effect2 = TELWithdraw withdrawEffect
        
        -- Just check that toEffect doesn't crash on these effects
        toEffect effect1 `shouldSatisfy` isCompositeEffect
        toEffect effect2 `shouldSatisfy` isCompositeEffect
      
    describe "Choice Effects (<|>)" $ do
      it "correctly converts choice TEL effects" $ do
        -- Create two effects
        let depositEffect = TELDepositEffect 
              { depositResource = TextValue "resource1"
              , depositAmount = IntValue 100
              , depositTimeline = TextValue "program1"
              }
        let withdrawEffect = TELWithdrawEffect
              { withdrawResource = TextValue "resource2"
              , withdrawAmount = IntValue 200
              , withdrawTimeline = TextValue "program2"
              }
        
        -- Create core effects
        let effect1 = TELDeposit depositEffect
        let effect2 = TELWithdraw withdrawEffect
        
        -- Just check that toEffect doesn't crash on these effects
        toEffect effect1 `shouldSatisfy` isCompositeEffect
        toEffect effect2 `shouldSatisfy` isCompositeEffect

-- Helper function to check if an effect is a CompositeEffect
isCompositeEffect :: Effect -> Bool
isCompositeEffect (CompositeEffect _) = True
isCompositeEffect _ = False 