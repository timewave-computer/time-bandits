{-# LANGUAGE OverloadedStrings #-}

module Core.TEL.InterpreterTest (testTELInterpreter) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (unless)

import Core.TEL
import Core.TEL.Interpreter

-- | Main test suite for TEL interpreter
testTELInterpreter :: Spec
testTELInterpreter = do
  describe "TEL Interpreter" $ do
    describe "Expression Evaluation" $ do
      testLiteralEvaluation
      testOperatorEvaluation
      testFunctionEvaluation
    
    describe "Effect Execution" $ do
      testEffectEvaluation

-- | Test evaluation of literals
testLiteralEvaluation :: Spec
testLiteralEvaluation = do
  it "evaluates integer literals" $ do
    let expr = LiteralExpr (IntLiteral 42)
    runTestExpression expr `shouldReturn` Right (VInt 42)

  it "evaluates boolean literals" $ do
    let expr = LiteralExpr (BoolLiteral True)
    runTestExpression expr `shouldReturn` Right (VBool True)

  it "evaluates text literals" $ do
    let expr = LiteralExpr (TextLiteral "hello")
    runTestExpression expr `shouldReturn` Right (VText "hello")

  it "evaluates list literals" $ do
    let expr = LiteralExpr (ListLiteral [
                  LiteralExpr (IntLiteral 1),
                  LiteralExpr (IntLiteral 2),
                  LiteralExpr (IntLiteral 3)
                ])
    runTestExpression expr `shouldReturn` Right (VList [VInt 1, VInt 2, VInt 3])

-- | Test evaluation of operators
testOperatorEvaluation :: Spec
testOperatorEvaluation = do
  it "evaluates arithmetic operators" $ do
    let add = InfixExpr (LiteralExpr (IntLiteral 5)) AddOp (LiteralExpr (IntLiteral 3))
    runTestExpression add `shouldReturn` Right (VInt 8)

    let sub = InfixExpr (LiteralExpr (IntLiteral 5)) SubOp (LiteralExpr (IntLiteral 3))
    runTestExpression sub `shouldReturn` Right (VInt 2)

    let mul = InfixExpr (LiteralExpr (IntLiteral 5)) MulOp (LiteralExpr (IntLiteral 3))
    runTestExpression mul `shouldReturn` Right (VInt 15)

  it "evaluates comparison operators" $ do
    let eq = InfixExpr (LiteralExpr (IntLiteral 5)) EqOp (LiteralExpr (IntLiteral 5))
    runTestExpression eq `shouldReturn` Right (VBool True)

    let neq = InfixExpr (LiteralExpr (IntLiteral 5)) NeqOp (LiteralExpr (IntLiteral 3))
    runTestExpression neq `shouldReturn` Right (VBool True)

    let lt = InfixExpr (LiteralExpr (IntLiteral 3)) LtOp (LiteralExpr (IntLiteral 5))
    runTestExpression lt `shouldReturn` Right (VBool True)

  it "evaluates logical operators" $ do
    let andOp = InfixExpr (LiteralExpr (BoolLiteral True)) AndOp (LiteralExpr (BoolLiteral False))
    runTestExpression andOp `shouldReturn` Right (VBool False)

    let orOp = InfixExpr (LiteralExpr (BoolLiteral True)) OrOp (LiteralExpr (BoolLiteral False))
    runTestExpression orOp `shouldReturn` Right (VBool True)

-- | Test evaluation of functions
testFunctionEvaluation :: Spec
testFunctionEvaluation = do
  it "evaluates simple function application" $ do
    let program = Program [
            Definition 
              (Just (TypeSignature "add" (FunctionType (BasicType "Int") (FunctionType (BasicType "Int") (BasicType "Int"))))) 
              (SimpleFunctionDef "add" [VarPattern "x", VarPattern "y"] 
                (InfixExpr (VariableExpr "x") AddOp (VariableExpr "y"))),
            Definition 
              (Just (TypeSignature "main" (BasicType "Int")))
              (SimpleFunctionDef "main" [] 
                (ApplicationExpr (VariableExpr "add") [LiteralExpr (IntLiteral 3), LiteralExpr (IntLiteral 4)]))
          ] Nothing
    
    runTestProgram program `shouldReturn` Right (VInt 7)

-- | Test evaluation of effects
testEffectEvaluation :: Spec
testEffectEvaluation = do
  it "evaluates deposit effect" $ do
    let depositExpr = EffectExpr $ DepositExpr 
                        (LiteralExpr (IntLiteral 100))
                        (LiteralExpr (TextLiteral "account"))
                        (LiteralExpr (TextLiteral "ethereum"))
    
    result <- runTestExpression depositExpr
    case result of
      Right (VEffect _) -> return () -- Success if we get an effect
      _ -> expectationFailure $ "Expected effect result, got: " ++ show result

  it "evaluates transfer effect" $ do
    let transferExpr = EffectExpr $ TransferExpr 
                         (LiteralExpr (IntLiteral 50))
                         (LiteralExpr (TextLiteral "source"))
                         (LiteralExpr (TextLiteral "target"))
                         (LiteralExpr (TextLiteral "ethereum"))
    
    result <- runTestExpression transferExpr
    case result of
      Right (VEffect _) -> return () -- Success if we get an effect
      _ -> expectationFailure $ "Expected effect result, got: " ++ show result

-- Helper to run a test expression
runTestExpression :: Expression -> IO (Either InterpreterError Value)
runTestExpression expr = runM $
  runError $
  runReader emptyEnv $
  evalState (LamportTime 0) $
  mockResourceOps $
  mockEffectHandler $
  interpretExpr expr 