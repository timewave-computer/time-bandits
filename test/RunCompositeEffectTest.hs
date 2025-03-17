module Main where

import Test.Hspec
import qualified TimeBandits.Core.TEL.CompositeEffectTest

main :: IO ()
main = hspec $ do
  describe "CompositeEffect Tests" $ do
    TimeBandits.Core.TEL.CompositeEffectTest.testCompositeEffects 