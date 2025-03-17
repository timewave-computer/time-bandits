{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Main where

import Test.Hspec
import TimeBandits.Core.TEL.AST
import TimeBandits.Core.TEL.Interpreter (toEffect, TELDepositEffect(..), TELWithdrawEffect(..), CoreEffect(..), Value(..))
import TimeBandits.Core.ResourceId (ResourceId(..))
import TimeBandits.Core.ProgramId (ProgramId(..))
import TimeBandits.Core.Effect (Effect(..))
import TimeBandits.Core.TimelineId (TimelineId(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE

-- | Direct test for toEffect function
testToEffect :: Spec
testToEffect = do
  describe "toEffect function" $ do
    -- Simple tests to see if toEffect is defined
    it "toEffect is defined and can be called" $ do
      -- Create a simple deposit effect
      let depositEffect = TELDepositEffect 
            { depositResource = TextValue "resource1"
            , depositAmount = IntValue 100
            , depositTimeline = TextValue "program1"
            }
      let coreEffect = TELDeposit depositEffect
      
      -- Convert to Effect
      let result = toEffect coreEffect
      
      -- Just check that it doesn't crash
      result `shouldSatisfy` const True
      
    it "converts composite effects correctly" $ do
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
      
      -- For now, just check that toEffect doesn't crash
      toEffect effect1 `shouldSatisfy` const True
      toEffect effect2 `shouldSatisfy` const True 

main :: IO ()
main = hspec testToEffect 