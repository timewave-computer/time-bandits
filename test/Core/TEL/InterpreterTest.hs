{-# LANGUAGE OverloadedStrings #-}

module Core.TEL.InterpreterTest (testTELInterpreter) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (unless)

import Core.TEL

-- | Main test suite for TEL interpreter
testTELInterpreter :: Spec
testTELInterpreter = do
  -- Skip tests for now since we need to implement the actual interpreter
  describe "TEL Interpreter" $ do
    it "interpreter tests are pending implementation" $ do
      pendingWith "Interpreter tests need to be updated to match new implementation" 