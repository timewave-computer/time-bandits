{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Core.TEL.Interpreter
Description : Interpreter for the Temporal Effect Language
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides an interpreter for the Temporal Effect Language (TEL) that translates
TEL expressions and programs into Polysemy effects that can be executed in the Time Bandits
framework. It serves as a bridge between the TEL language and the Core.Effects system.
-}
module Core.TEL.Interpreter
  ( -- * Interpreter
    interpret
  , interpretExpr
  , interpretProgram
  
  -- * Execution Environment
  , EvalEnv
  , emptyEnv
  , extendEnv
  
  -- * Execution Error
  , InterpreterError(..)
  
  -- * Value type
  , Value(..)
  
  -- * Helpers for testing
  , runTestProgram
  ) where

import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy hiding (interpret)
import qualified Polysemy as P
import Polysemy.Error
import qualified Polysemy.Reader as PR
import qualified Polysemy.State as PS
import Data.Foldable (foldl')
import Control.Monad (foldM)
import Data.String (IsString(..))

import Core.TEL.AST
import Core.Effects
import qualified Core.Effect as CE
import Core.Common (Hash(..), LamportTime(..), computeHash, EntityHash(..))
import Core.Types (ActorInfo(..), ResourceInfo(..))
import qualified Core.Effect as TE

-- | Instances to allow using string literals for hash types (for mock implementations only)
instance IsString Hash where
  fromString s = computeHash (TE.encodeUtf8 (T.pack s))

instance IsString (EntityHash a) where
  fromString s = EntityHash (fromString s)

-- | Evaluation environment mapping variables to values
data Value
  = VInt Integer
  | VDouble Double
  | VText Text
  | VBool Bool
  | VList [Value]
  | VTuple [Value]
  | VClosure [Identifier] Expression EvalEnv
  | VEffect CE.Effect
  deriving (Show, Eq)

-- | Evaluation environment mapping variables to values
type EvalEnv = Map Identifier Value

-- | Interpreter error
data InterpreterError
  = UnboundVariable Identifier
  | TypeMismatch String Value
  | PatternMatchFailed Pattern Value
  | UnsupportedOperation String
  | EffectError Text
  | InternalError Text
  deriving (Show, Eq)

-- | Empty evaluation environment
emptyEnv :: EvalEnv
emptyEnv = Map.empty

-- | Extend evaluation environment with a new binding
extendEnv :: Identifier -> Value -> EvalEnv -> EvalEnv
extendEnv = Map.insert

-- | The interpreter monad stack
type Interpreter r a = 
  Sem (PR.Reader EvalEnv ': Error InterpreterError ': PS.State LamportTime ': ResourceOps ': EffectHandler ': r) a

-- | Interpret a TEL program, returning the final program state
interpretProgram :: 
  Member (Embed IO) r => 
  Program -> 
  EvalEnv -> 
  Sem r (Either InterpreterError Program)
interpretProgram prog env = 
    runError $ 
    PR.runReader env $ 
    PS.evalState (LamportTime 0) $ 
    mockResourceOps $ 
    mockEffectHandler $ 
    interpretProgramWithStubs prog
  where
    -- | Simplified version that doesn't use all effects
    interpretProgramWithStubs :: 
      Members '[PR.Reader EvalEnv, Error InterpreterError] r => 
      Program -> 
      Sem r Program
    interpretProgramWithStubs p = return p  -- Just return the program as-is for now

-- | Interpret a TEL expression
interpretExpr :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler] r => 
  Expression -> 
  Sem r Value
interpretExpr = \case
  LiteralExpr lit -> interpretLiteral lit
  VariableExpr var -> do
    env <- PR.ask
    case Map.lookup var env of
      Just val -> return val
      Nothing -> throw $ UnboundVariable var
  
  ApplicationExpr func args -> do
    funcVal <- interpretExpr func
    argVals <- mapM interpretExpr args
    applyFunction funcVal argVals
  
  LambdaExpr params body -> do
    env <- PR.ask
    let paramNames = concatMap extractVarNames params
    return $ VClosure paramNames body env
  
  EffectExpr effect -> interpretEffect effect
  
  LetExpr decls body -> do
    env <- PR.ask
    newEnv <- foldM (\e decl -> processDeclaration e decl) env decls
    PR.local (const newEnv) $ interpretExpr body
  
  IfExpr cond thenExpr elseExpr -> do
    condVal <- interpretExpr cond
    case condVal of
      VBool True -> interpretExpr thenExpr
      VBool False -> interpretExpr elseExpr
      _ -> throw $ TypeMismatch "Bool" condVal
  
  CaseExpr scrutinee patterns -> do
    val <- interpretExpr scrutinee
    matchPatterns val patterns
  
  TimeExpr timeExpr -> interpretTimeExpr timeExpr
  
  InfixExpr left op right -> do
    leftVal <- interpretExpr left
    rightVal <- interpretExpr right
    applyOperator op leftVal rightVal
  
  HashRefExpr _hash -> 
    throw $ UnsupportedOperation "Hash references not yet supported"
  
  DoExpr statements -> interpretDo statements

-- | Interpret a literal expression
interpretLiteral :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler] r => 
  LiteralExpr -> 
  Sem r Value
interpretLiteral = \case
  IntLiteral n -> return $ VInt n
  DoubleLiteral d -> return $ VDouble d
  TextLiteral t -> return $ VText t
  BoolLiteral b -> return $ VBool b
  ListLiteral exprs -> do
    vals <- mapM interpretExpr exprs
    return $ VList vals
  TupleLiteral exprs -> do
    vals <- mapM interpretExpr exprs
    return $ VTuple vals

-- | Apply a function to arguments
applyFunction :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler] r => 
  Value -> 
  [Value] -> 
  Sem r Value
applyFunction (VClosure params body closureEnv) args
  | length params /= length args = 
      throw $ UnsupportedOperation $ "Function expected " <> show (length params) <> " arguments but got " <> show (length args)
  | otherwise = do
      env <- PR.ask
      let paramBindings = zip params args
      let newEnv = foldl' (\e (name, val) -> Map.insert name val e) (Map.union closureEnv env) paramBindings
      PR.local (const newEnv) $ interpretExpr body
applyFunction val _ = throw $ TypeMismatch "Function" val

-- | Match value against patterns
matchPatterns :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler] r => 
  Value -> 
  [(Pattern, Expression)] -> 
  Sem r Value
matchPatterns _ [] = throw $ InternalError "No matching pattern"
matchPatterns val ((pat, expr):rest) = do
  env <- PR.ask
  case matchPattern val pat of
    Right bindings -> do
      let newEnv = foldl' (\e (name, v) -> Map.insert name v e) env bindings
      PR.local (const newEnv) $ interpretExpr expr
    Left _ -> matchPatterns val rest

-- | Match a value against a pattern, returning bindings
matchPattern :: Value -> Pattern -> Either InterpreterError [(Identifier, Value)]
matchPattern val = \case
  VarPattern name -> Right [(name, val)]
  WildcardPattern -> Right []
  LiteralPattern (IntLiteral n) -> 
    case val of
      VInt m | n == m -> Right []
      _ -> Left $ PatternMatchFailed (LiteralPattern (IntLiteral n)) val
  LiteralPattern (BoolLiteral b) -> 
    case val of
      VBool c | b == c -> Right []
      _ -> Left $ PatternMatchFailed (LiteralPattern (BoolLiteral b)) val
  LiteralPattern (TextLiteral t) -> 
    case val of
      VText s | t == s -> Right []
      _ -> Left $ PatternMatchFailed (LiteralPattern (TextLiteral t)) val
  -- Add other pattern matching cases as needed
  _ -> Left $ UnsupportedOperation "Pattern not yet supported"

-- | Interpret a time expression
interpretTimeExpr :: 
  Members '[Error InterpreterError] r => 
  TimeExpr -> 
  Sem r Value
interpretTimeExpr _ = 
  throw $ UnsupportedOperation "Time expressions not yet supported"

-- | Apply an operator to values
applyOperator :: 
  Members '[Error InterpreterError] r => 
  Operator -> 
  Value -> 
  Value -> 
  Sem r Value
applyOperator op leftVal rightVal = 
  case (op, leftVal, rightVal) of
    (AddOp, VInt a, VInt b) -> return $ VInt (a + b)
    (SubOp, VInt a, VInt b) -> return $ VInt (a - b)
    (MulOp, VInt a, VInt b) -> return $ VInt (a * b)
    (DivOp, VInt a, VInt b) -> 
      if b == 0 
        then throw $ InternalError "Division by zero" 
        else return $ VInt (a `div` b)
    (EqOp, VInt a, VInt b) -> return $ VBool (a == b)
    (NeqOp, VInt a, VInt b) -> return $ VBool (a /= b)
    (LtOp, VInt a, VInt b) -> return $ VBool (a < b)
    (GtOp, VInt a, VInt b) -> return $ VBool (a > b)
    (LeOp, VInt a, VInt b) -> return $ VBool (a <= b)
    (GeOp, VInt a, VInt b) -> return $ VBool (a >= b)
    (AndOp, VBool a, VBool b) -> return $ VBool (a && b)
    (OrOp, VBool a, VBool b) -> return $ VBool (a || b)
    -- Add support for other operators and value types
    _ -> throw $ UnsupportedOperation $ "Unsupported operator " <> show op <> " for values " <> show leftVal <> " and " <> show rightVal

-- | Process a declaration
processDeclaration :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler] r => 
  EvalEnv -> 
  Declaration -> 
  Sem r EvalEnv
processDeclaration env (Declaration name expr) = do
  val <- PR.local (const env) $ interpretExpr expr
  return $ Map.insert name val env

-- | Interpret a do block
interpretDo :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler] r => 
  [DoStatement] -> 
  Sem r Value
interpretDo [] = return $ VTuple []  -- Empty do block returns unit
interpretDo stmts = do
  env <- PR.ask
  foldM processDoStatement (VTuple [], env) stmts >>= \case
    (val, _) -> return val
  where
    processDoStatement :: 
      Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler] r => 
      (Value, EvalEnv) -> 
      DoStatement -> 
      Sem r (Value, EvalEnv)
    processDoStatement (_, env) = \case
      BindStmt name expr -> do
        val <- PR.local (const env) $ interpretExpr expr
        return (val, Map.insert name val env)
      LetStmt decls -> do
        newEnv <- foldM (\e decl -> PR.local (const e) $ processDeclaration e decl) env decls
        return (VTuple [], newEnv)
      ExprStmt expr -> do
        val <- PR.local (const env) $ interpretExpr expr
        return (val, env)

-- | Interpret an effect expression
interpretEffect :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler] r => 
  EffectExpr -> 
  Sem r Value
interpretEffect = \case
  DepositExpr amount target timeline -> do
    amountVal <- interpretExpr amount
    recipientVal <- interpretExpr target
    timelineVal <- interpretExpr timeline
    
    case (amountVal, recipientVal, timelineVal) of
      (VInt amt, VText recip, VText tl) -> do
        -- In a real implementation, you would create a proper Effect here 
        -- and use the ResourceOps Polysemy effect
        let effect = CE.ProgramEffect "deposit" (TE.encodeUtf8 recip)
        return $ VEffect effect
      _ -> throw $ TypeMismatch "Expected (Int, Text, Text) for deposit effect" (VTuple [amountVal, recipientVal, timelineVal])
  
  WithdrawExpr amount source timeline -> do
    amountVal <- interpretExpr amount
    sourceVal <- interpretExpr source
    timelineVal <- interpretExpr timeline
    
    case (amountVal, sourceVal, timelineVal) of
      (VInt amt, VText src, VText tl) -> do
        -- Similar to deposit, create an effect
        let effect = CE.ProgramEffect "withdraw" (TE.encodeUtf8 src)
        return $ VEffect effect
      _ -> throw $ TypeMismatch "Expected (Int, Text, Text) for withdraw effect" (VTuple [amountVal, sourceVal, timelineVal])
  
  TransferExpr amount source recipient timeline -> do
    amountVal <- interpretExpr amount
    sourceVal <- interpretExpr source
    recipientVal <- interpretExpr recipient
    timelineVal <- interpretExpr timeline
    
    case (amountVal, sourceVal, recipientVal, timelineVal) of
      (VInt amt, VText src, VText recip, VText tl) -> do
        -- Create a transfer effect
        let effect = CE.ProgramEffect "transfer" (TE.encodeUtf8 src)
        return $ VEffect effect
      _ -> throw $ TypeMismatch "Expected (Int, Text, Text, Text) for transfer effect" 
                             (VTuple [amountVal, sourceVal, recipientVal, timelineVal])
  
  ObserveExpr source timeline -> do
    sourceVal <- interpretExpr source
    timelineVal <- interpretExpr timeline
    
    case (sourceVal, timelineVal) of
      (VText src, VText tl) -> do
        -- Create an observe effect
        let effect = CE.ProgramEffect "observe" (TE.encodeUtf8 src)
        return $ VEffect effect
      _ -> throw $ TypeMismatch "Expected (Text, Text) for observe effect" (VTuple [sourceVal, timelineVal])
  
  EmitExpr message -> do
    msgVal <- interpretExpr message
    
    case msgVal of
      VText msg -> do
        -- Create an emit effect
        let effect = CE.ProgramEffect "emit" (TE.encodeUtf8 msg)
        return $ VEffect effect
      _ -> throw $ TypeMismatch "Expected Text for emit effect" msgVal
  
  InvokeExpr function -> do
    funcVal <- interpretExpr function
    
    case funcVal of
      VClosure _ _ _ -> do
        -- Create an invoke effect
        let effect = CE.ProgramEffect "invoke" (TE.encodeUtf8 "function")
        return $ VEffect effect
      _ -> throw $ TypeMismatch "Expected Function for invoke effect" funcVal

-- | Helper for running a test program with mock effects
runTestProgram :: Program -> IO (Either InterpreterError Value)
runTestProgram prog = runM $ 
  runError $ 
  PR.runReader emptyEnv $ 
  PS.evalState (LamportTime 0) $ 
  mockResourceOps $ 
  mockEffectHandler $ 
  evalProgram prog
  where
    evalProgram :: 
      Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler] r => 
      Program -> 
      Sem r Value
    evalProgram p = do
      -- Find the main function or entry point
      case findMainFunction p of
        Just expr -> interpretExpr expr
        Nothing -> throw $ InternalError "No main function found in program"
    
    findMainFunction :: Program -> Maybe Expression
    findMainFunction p = 
      case filter isMainFunction (programDefinitions p) of
        (Definition _ (SimpleFunctionDef _ _ body):_) -> Just body
        _ -> Nothing
    
    isMainFunction :: Definition -> Bool
    isMainFunction (Definition _ (SimpleFunctionDef name _ _)) = name == "main"
    isMainFunction _ = False

-- Mock implementations for testing

-- | Mock implementation of resource operations
mockResourceOps :: Member (Embed IO) r => Sem (ResourceOps ': r) a -> Sem r a
mockResourceOps = P.interpret $ \case
  GetResource _ -> pure $ Right mockResource
  CreateResource _ _ _ -> pure $ Right mockResource
  TransferResource _ _ _ -> pure $ Right mockResource
  ConsumeResource _ -> pure $ Right mockResource
  VerifyResource _ -> pure $ Right True
  GetResourcesByOwner _ -> pure $ Right [mockResource]
  GetResourcesByTimeline _ -> pure $ Right [mockResource]
  where
    mockResource = ResourceInfo 
      { resourceId = "mock-resource-id"
      , resourceOrigin = "mock-timeline"
      , resourceOwner = "mock-owner"
      , resourceCapabilities = [TransferCapability, UpdateCapability]
      , resourceMeta = TE.encodeUtf8 "metadata"
      , resourceSpentBy = Nothing
      , resourceParents = []
      , resourceTimestamp = LamportTime 0
      , resourceProvenanceChain = ["mock-timeline"]
      }

-- | Mock implementation of effect handling
mockEffectHandler :: Member (Embed IO) r => Sem (EffectHandler ': r) a -> Sem r a
mockEffectHandler = P.interpret $ \case
  ApplyEffect effect _ -> 
    pure $ EffectSuccess "success"
  ValidateEffectPreconditions _ _ -> pure True

-- | Mock effect types for testing
data DepositEffect = DepositEffect Integer String String deriving Show
data WithdrawEffect = WithdrawEffect Integer String String deriving Show
data TransferEffect = TransferEffect Integer String String String deriving Show
data ObserveEffect = ObserveEffect String String deriving Show
data EmitEffect = EmitEffect String deriving Show
data InvokeEffect = InvokeEffect String deriving Show

-- | Main interpreter entry point
interpret :: Program -> IO (Either InterpreterError Value)
interpret = runTestProgram

runTestExpr :: Expression -> IO (Either InterpreterError Value)
runTestExpr expr = runM $ 
  runError $ 
  PR.runReader emptyEnv $ 
  PS.evalState (LamportTime 0) $ 
  mockResourceOps $ 
  mockEffectHandler $ 
  interpretExpr expr

-- | Run an interpreter with the given environment
runTestExprWithEnv :: EvalEnv -> Expression -> IO (Either InterpreterError Value)
runTestExprWithEnv env expr = runM $ 
  runError $ 
  PR.runReader env $ 
  PS.evalState (LamportTime 0) $ 
  mockResourceOps $ 
  mockEffectHandler $ 
  interpretExpr expr

-- | Extract variable names from patterns
extractVarNames :: Pattern -> [Identifier]
extractVarNames (VarPattern name) = [name]
extractVarNames (ConstructorPattern _ patterns) = concatMap extractVarNames patterns
extractVarNames _ = []  -- Other patterns don't bind variables 