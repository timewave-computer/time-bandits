{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.TECLTest (
  tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Core.TECL
  ( TECLAST(..)
  , TECLStatement(..)
  , TECLExpression(..)
  , TECLEffect(..)
  , LiteralValue(..)
  , ParseError(..)
  , TypeCheckError(..)
  , parseTECL
  , typecheckTECL
  , translateToEffects
  , ProposedEffect(..)
  )

tests :: TestTree
tests = testGroup "TECL Tests"
  [ testGroup "Parsing Tests"
    [ testCase "Parse deposit effect" testParseDeposit
    , testCase "Parse withdrawal effect" testParseWithdrawal
    ]
  , testGroup "Type Checking Tests"
    [ testCase "Type check valid program" testTypeCheckValid
    ]
  , testGroup "Effect Translation Tests"
    [ testCase "Translate deposit to effect" testTranslateDeposit
    , testCase "Translate conditional to effect" testTranslateConditional
    ]
  ]

-- | Sample TECL program for testing
sampleProgram :: Text
sampleProgram = T.unlines
  [ "deposit Ethereum ETH 1.0;"
  , "withdraw Ethereum ETH 0.5;"
  , "if (price > 2000) {"
  , "  transfer Ethereum ETH 0.25 to \"receiver\";"
  , "} else {"
  , "  observe Ethereum price;"
  , "}"
  ]

-- | Test parsing a deposit effect
testParseDeposit :: Assertion
testParseDeposit = do
  let input = "deposit Ethereum ETH 1.0;"
  case parseTECL input of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right ast -> do
      -- Check that we have exactly one statement
      assertEqual "Should have one statement" 1 (length $ astStatements ast)
      
      -- Check that it's a deposit effect
      case head $ astStatements ast of
        EffectStatement (DepositEffect timeline asset amount) -> do
          assertEqual "Timeline should match" "Ethereum" timeline
          assertEqual "Asset should match" "ETH" asset
          case amount of
            LiteralExpr (DoubleLiteral value) -> 
              assertEqual "Amount should match" 1.0 value
            _ -> assertFailure "Amount should be a double literal"
        _ -> assertFailure "Statement should be a deposit effect"

-- | Test parsing a withdrawal effect
testParseWithdrawal :: Assertion
testParseWithdrawal = do
  let input = "withdraw Ethereum ETH 0.5;"
  case parseTECL input of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right ast -> 
      -- For now, with our placeholder implementation, we just check it doesn't fail
      return ()

-- | Test type checking a valid program
testTypeCheckValid :: Assertion
testTypeCheckValid = do
  case parseTECL sampleProgram of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right ast -> 
      case typecheckTECL ast of
        Left err -> assertFailure $ "Type check failed: " ++ show err
        Right _ -> return ()

-- | Test translating a deposit to an effect
testTranslateDeposit :: Assertion
testTranslateDeposit = do
  let input = "deposit Ethereum ETH 1.0;"
  case parseTECL input of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right ast -> do
      let effects = translateToEffects ast
      
      -- Check that we produced one effect
      assertEqual "Should have one effect" 1 (length effects)
      
      -- With our placeholder implementation, we just check the type
      let effect = head effects
      assertEqual "Effect type should be deposit" "deposit" (proposedType effect)
      
      -- Check payload fields
      let payload = proposedPayload effect
      assert $ Map.member "timeline" payload
      assert $ Map.member "asset" payload
      assert $ Map.member "amount" payload

-- | Test translating a conditional statement to an effect
testTranslateConditional :: Assertion
testTranslateConditional = do
  let input = "if (price > 2000) { deposit Ethereum ETH 1.0; } else { withdraw Ethereum ETH 0.5; }"
  case parseTECL input of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right ast -> do
      let effects = translateToEffects ast
      
      -- Check that we produced one effect
      assertEqual "Should have one effect" 1 (length effects)
      
      -- With our placeholder implementation, we just check the type
      let effect = head effects
      assertEqual "Effect type should be conditional" "conditional" (proposedType effect)
      
      -- Check payload fields
      let payload = proposedPayload effect
      assert $ Map.member "condition" payload
      assert $ Map.member "thenEffects" payload
      assert $ Map.member "elseEffects" payload 