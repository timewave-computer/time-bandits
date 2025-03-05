module Main where

import Test.Hspec
import qualified ResourceOwnershipSpec
import qualified AccountProgramSpec
import qualified ConcurrentEffectsSpec
import qualified CrossChainScenarioSpec
import qualified TimeMapCausalitySpec
import qualified EffectReplayabilitySpec
import qualified MessagePropagationSpec
import qualified BasicFunctionalitySpec

main :: IO ()
main = hspec $ do
  describe "ResourceOwnership" ResourceOwnershipSpec.spec
  describe "AccountProgram" AccountProgramSpec.spec
  describe "ConcurrentEffects" ConcurrentEffectsSpec.spec
  describe "CrossChainScenario" CrossChainScenarioSpec.spec
  describe "TimeMapCausality" TimeMapCausalitySpec.spec
  describe "EffectReplayability" EffectReplayabilitySpec.spec
  describe "MessagePropagation" MessagePropagationSpec.spec
  describe "BasicFunctionality" BasicFunctionalitySpec.spec 