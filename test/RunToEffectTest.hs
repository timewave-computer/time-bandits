module Main where

import Test.Hspec
import qualified TimeBandits.Core.TEL.ToEffectTest

main :: IO ()
main = hspec $ do
  describe "ToEffect Tests" $ do
    TimeBandits.Core.TEL.ToEffectTest.testToEffect 