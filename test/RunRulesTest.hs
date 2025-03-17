module Main where

import Test.Hspec
import qualified TimeBandits.Core.FactObservation.BasicRulesSpec

main :: IO ()
main = hspec $ do
  describe "Basic Rules Tests" $ do
    describe "TimeBandits" $ do
      TimeBandits.Core.FactObservation.BasicRulesSpec.spec 