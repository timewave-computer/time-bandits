module Main where

import Test.Hspec
import qualified TimeBandits.ConcurrentEffectsSpec

main :: IO ()
main = hspec $ do
  describe "ConcurrentEffects Tests" $ do
    TimeBandits.ConcurrentEffectsSpec.spec 