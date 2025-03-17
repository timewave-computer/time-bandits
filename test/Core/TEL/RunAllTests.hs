module Main where

import Test.Hspec
import qualified TimeBandits.Core.TEL.ToEffectTest
import qualified TimeBandits.Core.TEL.CompositeEffectTest
import qualified TimeBandits.ConcurrentEffectsSpec
import qualified TimeBandits.Core.FactObservation.BasicRulesSpec
import qualified TimeBandits.Core.FactObservation.BasicEngineSpec

main :: IO ()
main = hspec $ do
  describe "TEL Tests" $ do
    describe "ToEffect Tests" $ do
      TimeBandits.Core.TEL.ToEffectTest.testToEffect
    
    describe "CompositeEffect Tests" $ do
      TimeBandits.Core.TEL.CompositeEffectTest.testCompositeEffects
    
    describe "ConcurrentEffects Tests" $ do
      TimeBandits.ConcurrentEffectsSpec.spec
  
  describe "FactObservation Tests" $ do
    describe "BasicRules Tests" $ do
      TimeBandits.Core.FactObservation.BasicRulesSpec.spec
    
    describe "BasicEngine Tests" $ do
      TimeBandits.Core.FactObservation.BasicEngineSpec.spec 