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
  , InterpreterError(..)  -- Export all constructors including InternalError
  
  -- * Value type
  , Value(..)
  
  -- * Effect types
  , CoreEffect(..)
  , TELDepositEffect(..)
  , TELWithdrawEffect(..)
  , TELTransferEffect(..)
  , TELObserveEffect(..)
  , TELEmitEffect(..)
  , TELInvokeEffect(..)
  
  -- * Effect conversion
  , toEffect
  
  -- * Helpers for testing
  , runTestProgram
  , runTestExpr
  , runTestExprWithEnv
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy hiding (interpret, Effect)
import qualified Polysemy as P
import Polysemy.Error
import qualified Polysemy.Reader as PR
import qualified Polysemy.State as PS
import Control.Monad (foldM)
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import Relude (viaNonEmpty)

import Core.TEL.AST
import Core.Effects hiding (Effect(..))
import Core.Common (Hash(..), LamportTime(..), computeHash, EntityHash(..))
import Core.Types (ActorInfo(..), ResourceInfo(..), ResourceCapability(..))
import Core.Effect (Effect(..), EffectResult(..), EffectMetadata(..), EffectStatus(..))
import Core.ProgramId (ProgramId(..))
import Core.ResourceId (ResourceId(..))
import Core.TimelineId (TimelineId(..))
import qualified Core.Effect as CE

-- | Value type for the interpreter
data Value
  = VInt Integer
  | VBool Bool
  | VText Text
  | VList [Value]
  | VFunction (forall r. Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => Value -> Sem r Value)
  | VEffect CoreEffect
  | VUnit
  | VTuple [Value]
  | VDouble Double
  | VClosure [Identifier] Expression EvalEnv

-- | Interpreter errors
data InterpreterError
  = UndefinedVariable Text
  | TypeMismatch Text Value
  | UnsupportedOperation Text
  | EffectError Text
  | InvalidProgram Text
  | InternalError Text  -- For internal errors like parsing errors

-- | Effect types for the interpreter
data TELDepositEffect = TELDepositEffect ResourceId Integer ProgramId
data TELWithdrawEffect = TELWithdrawEffect ResourceId Integer ProgramId
data TELTransferEffect = TELTransferEffect ResourceId ProgramId ProgramId Integer
data TELObserveEffect = TELObserveEffect TimelineId ByteString
data TELEmitEffect = TELEmitEffect ByteString
data TELInvokeEffect = TELInvokeEffect ByteString

-- | Core effect type
data CoreEffect
  = DepositEff TELDepositEffect
  | WithdrawEff TELWithdrawEffect
  | TransferEff TELTransferEffect
  | ObserveEff TELObserveEffect
  | EmitEff TELEmitEffect
  | InvokeEff TELInvokeEffect
  | SequenceEff CoreEffect CoreEffect
  | ParallelEff CoreEffect CoreEffect
  | ChoiceEff CoreEffect CoreEffect

-- | Evaluation environment mapping variables to values
type EvalEnv = Map Identifier Value

-- | Empty evaluation environment
emptyEnv :: EvalEnv
emptyEnv = Map.empty

-- | Extend evaluation environment with a new binding
extendEnv :: Identifier -> Value -> EvalEnv -> EvalEnv
extendEnv = Map.insert

-- | The interpreter monad stack
-- This type synonym represents the effect stack used throughout the interpreter.
-- It includes environment reading, error handling, state for Lamport time,
-- resource operations, effect handling, and logical clock operations.
type Interpreter r a = 
  Sem (PR.Reader EvalEnv ': Error InterpreterError ': PS.State LamportTime ': ResourceOps ': EffectHandler ': LogicalClock ': r) a

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
    mockLogicalClock $
    interpretProgramWithStubs prog
  where
    -- | Simplified version that doesn't use all effects
    interpretProgramWithStubs :: 
      Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
      Program -> 
      Sem r Program
    interpretProgramWithStubs p = return p  -- Just return the program as-is for now

-- | Interpret a TEL expression
interpretExpr :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  Expression -> 
  Sem r Value
interpretExpr = \case
  LiteralExpr lit -> interpretLiteral lit
  VariableExpr var -> do
    env <- PR.ask
    case Map.lookup var env of
      Just val -> return val
      Nothing -> throw $ UndefinedVariable var
  
  ApplicationExpr func args -> do
    funcVal <- interpretExpr func
    argVals <- mapM interpretExpr args
    applyFunction funcVal argVals
  
  LambdaExpr params body -> do
    env <- PR.ask
    let paramNames = concatMap extractVarNames params
    -- Return a VFunction that properly creates a closure with the environment
    return $ VFunction $ \x -> do
      -- Create an updated environment with the parameter bound to the value
      let newEnv = foldr (\param acc -> Map.insert param x acc) env paramNames
      -- Create a closure that will be evaluated later
      return $ VClosure paramNames body newEnv
  
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
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  LiteralExpr -> 
  Sem r Value
interpretLiteral = \case
  IntLiteral n -> return $ VInt n
  DoubleLiteral d -> throw $ TypeMismatch "Double" (VDouble d)
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
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  Value -> 
  [Value] -> 
  Sem r Value
applyFunction (VFunction f) args = 
  case viaNonEmpty head args of
    Just arg -> f arg
    Nothing -> throw $ TypeMismatch "At least one argument" (VList args)
applyFunction val _ = throw $ TypeMismatch "Function" val

-- | Match value against patterns
matchPatterns :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  Value -> 
  [(Pattern, Expression)] -> 
  Sem r Value
matchPatterns _ [] = throw $ InvalidProgram "No matching pattern"
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
      _ -> Left $ TypeMismatch "Int" val
  LiteralPattern (BoolLiteral b) -> 
    case val of
      VBool c | b == c -> Right []
      _ -> Left $ TypeMismatch "Bool" val
  LiteralPattern (TextLiteral t) -> 
    case val of
      VText s | t == s -> Right []
      _ -> Left $ TypeMismatch "Text" val
  -- Add other pattern matching cases as needed
  _ -> Left $ UnsupportedOperation "Pattern not yet supported"

-- | Interpret a time expression
interpretTimeExpr :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  TimeExpr -> 
  Sem r Value
interpretTimeExpr = \case
  AfterExpr _duration _unit expr -> do
    -- Interpret as a delayed effect
    valueExpr <- interpretExpr expr
    case valueExpr of
      VEffect effect -> do
        -- In a real implementation, this would schedule the effect after the specified time
        -- For now, we just return the effect
        return $ VEffect effect
      _ -> throw $ TypeMismatch "Effect" valueExpr
  
  WithinExpr _duration _unit expr -> do
    -- Interpret as an effect with timeout
    valueExpr <- interpretExpr expr
    case valueExpr of
      VEffect effect -> do
        -- In a real implementation, this would set a timeout for the effect
        -- For now, we just return the effect
        return $ VEffect effect
      _ -> throw $ TypeMismatch "Effect" valueExpr
  
  AtExpr _timestamp expr -> do
    -- Interpret as an effect to be executed at a specific time
    valueExpr <- interpretExpr expr
    case valueExpr of
      VEffect effect -> do
        -- In a real implementation, this would schedule the effect at the specified time
        -- For now, we just return the effect
        return $ VEffect effect
      _ -> throw $ TypeMismatch "Effect" valueExpr

-- | Process a declaration
processDeclaration :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  EvalEnv -> 
  Declaration -> 
  Sem r EvalEnv
processDeclaration env (Declaration name val) = do
  value <- PR.local (const env) $ interpretExpr val
  return $ Map.insert name value env

-- | Interpret an effect expression
interpretEffect :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  EffectExpr -> 
  Sem r Value
interpretEffect = \case
  DepositExpr resourceIdExpr programIdExpr amountExpr -> do
    -- Evaluate the expressions to get the actual values
    resourceIdVal <- interpretExpr resourceIdExpr
    programIdVal <- interpretExpr programIdExpr
    amountVal <- interpretExpr amountExpr
    
    -- Extract the values from the evaluated expressions
    resourceId <- case resourceIdVal of
      VText t -> return $ ResourceId (TE.encodeUtf8 t)
      _ -> throw $ TypeMismatch "Text for ResourceId" resourceIdVal
      
    programId <- case programIdVal of
      VText t -> return $ ProgramId (TE.encodeUtf8 t)
      _ -> throw $ TypeMismatch "Text for ProgramId" programIdVal
      
    amount <- case amountVal of
      VInt n -> return n
      _ -> throw $ TypeMismatch "Integer for amount" amountVal
    
    -- Create a deposit effect
    let effect = DepositEff (TELDepositEffect resourceId amount programId)
    -- Apply the effect
    time <- PS.get
    result <- applyEffect (convertEffect effect) (makeEmptyMetadata effect time)
    case result of
      EffectSuccess _bs -> return $ VEffect effect
      EffectFailure msg -> throw $ EffectError msg
      EffectDeferred _ -> return $ VEffect effect
  
  WithdrawExpr resourceIdExpr programIdExpr amountExpr -> do
    -- Evaluate the expressions to get the actual values
    resourceIdVal <- interpretExpr resourceIdExpr
    programIdVal <- interpretExpr programIdExpr
    amountVal <- interpretExpr amountExpr
    
    -- Extract the values from the evaluated expressions
    resourceId <- case resourceIdVal of
      VText t -> return $ ResourceId (TE.encodeUtf8 t)
      _ -> throw $ TypeMismatch "Text for ResourceId" resourceIdVal
      
    programId <- case programIdVal of
      VText t -> return $ ProgramId (TE.encodeUtf8 t)
      _ -> throw $ TypeMismatch "Text for ProgramId" programIdVal
      
    amount <- case amountVal of
      VInt n -> return n
      _ -> throw $ TypeMismatch "Integer for amount" amountVal
    
    -- Create a withdraw effect
    let effect = WithdrawEff (TELWithdrawEffect resourceId amount programId)
    -- Apply the effect
    time <- PS.get
    result <- applyEffect (convertEffect effect) (makeEmptyMetadata effect time)
    case result of
      EffectSuccess _bs -> return $ VEffect effect
      EffectFailure msg -> throw $ EffectError msg
      EffectDeferred _ -> return $ VEffect effect
  
  TransferExpr resourceIdExpr sourceProgramIdExpr destProgramIdExpr amountExpr -> do
    -- Evaluate the expressions to get the actual values
    resourceIdVal <- interpretExpr resourceIdExpr
    sourceProgramIdVal <- interpretExpr sourceProgramIdExpr
    destProgramIdVal <- interpretExpr destProgramIdExpr
    amountVal <- interpretExpr amountExpr
    
    -- Extract the values from the evaluated expressions
    resourceId <- case resourceIdVal of
      VText t -> return $ ResourceId (TE.encodeUtf8 t)
      _ -> throw $ TypeMismatch "Text for ResourceId" resourceIdVal
      
    sourceProgramId <- case sourceProgramIdVal of
      VText t -> return $ ProgramId (TE.encodeUtf8 t)
      _ -> throw $ TypeMismatch "Text for source ProgramId" sourceProgramIdVal
      
    destProgramId <- case destProgramIdVal of
      VText t -> return $ ProgramId (TE.encodeUtf8 t)
      _ -> throw $ TypeMismatch "Text for destination ProgramId" destProgramIdVal
      
    amount <- case amountVal of
      VInt n -> return n
      _ -> throw $ TypeMismatch "Integer for amount" amountVal
    
    -- Create a transfer effect
    let effect = TransferEff (TELTransferEffect resourceId sourceProgramId destProgramId amount)
    -- Apply the effect
    time <- PS.get
    result <- applyEffect (convertEffect effect) (makeEmptyMetadata effect time)
    case result of
      EffectSuccess _bs -> return $ VEffect effect
      EffectFailure msg -> throw $ EffectError msg
      EffectDeferred _ -> return $ VEffect effect
  
  ObserveExpr factTypeExpr timelineIdExpr -> do
    -- Evaluate the expressions to get the actual values
    factTypeVal <- interpretExpr factTypeExpr
    timelineIdVal <- interpretExpr timelineIdExpr
    
    -- Extract the values from the evaluated expressions
    factType <- case factTypeVal of
      VText t -> return t
      _ -> throw $ TypeMismatch "Text for factType" factTypeVal
      
    timelineId <- case timelineIdVal of
      VText t -> return $ TimelineId (TE.encodeUtf8 t)
      _ -> throw $ TypeMismatch "Text for TimelineId" timelineIdVal
    
    -- Create a timeline effect for observation
    let effect = ObserveEff (TELObserveEffect timelineId (TE.encodeUtf8 factType))
    -- Apply the effect
    time <- PS.get
    result <- applyEffect (convertEffect effect) (makeEmptyMetadata effect time)
    case result of
      EffectSuccess _bs -> return $ VEffect effect
      EffectFailure msg -> throw $ EffectError msg
      EffectDeferred _ -> return $ VEffect effect
  
  EmitExpr eventDataExpr -> do
    -- Evaluate the expression to get the actual value
    eventDataVal <- interpretExpr eventDataExpr
    
    -- Extract the value from the evaluated expression
    eventData <- case eventDataVal of
      VText t -> return t
      _ -> throw $ TypeMismatch "Text for eventData" eventDataVal
    
    -- Create an event effect (could be implemented as a program effect)
    let effect = EmitEff (TELEmitEffect (TE.encodeUtf8 eventData))
    -- Apply the effect
    time <- PS.get
    result <- applyEffect (convertEffect effect) (makeEmptyMetadata effect time)
    case result of
      EffectSuccess _bs -> return $ VEffect effect
      EffectFailure msg -> throw $ EffectError msg
      EffectDeferred _ -> return $ VEffect effect
  
  InvokeExpr programHashExpr -> do
    -- Evaluate the expression to get the actual value
    programHashVal <- interpretExpr programHashExpr
    
    -- Extract the value from the evaluated expression
    programHash <- case programHashVal of
      VText t -> return $ computeHash (TE.encodeUtf8 t)
      _ -> throw $ TypeMismatch "Text for programHash" programHashVal
    
    -- Create a program invocation effect
    let effect = InvokeEff (TELInvokeEffect (getHashBytes programHash))
    -- Apply the effect
    time <- PS.get
    result <- applyEffect (convertEffect effect) (makeEmptyMetadata effect time)
    case result of
      EffectSuccess _bs -> return $ VEffect effect
      EffectFailure msg -> throw $ EffectError msg
      EffectDeferred _ -> return $ VEffect effect

-- | Helper to create empty metadata for an effect
makeEmptyMetadata :: CoreEffect -> LamportTime -> EffectMetadata
makeEmptyMetadata effect _time = EffectMetadata
  { effectId = computeHash (CE.serializeEffect (toEffect effect))
  , effectCreator = ProgramId "TEL-interpreter"
  , effectCreatedAt = error "Not set - mock time" -- Would use real time in production
  , effectStatus = CE.EffectPending
  , effectPreconditions = []
  , effectParentIds = Set.empty
  , effectObserved = []
  }

-- | Convert CoreEffect to Effect for use with applyEffect
convertEffect :: CoreEffect -> Effect
convertEffect = toEffect

-- | Get the bytes from a Hash
getHashBytes :: Hash -> BS.ByteString
getHashBytes (Hash bs) = bs

-- | Interpret a do expression
interpretDo :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  [DoStatement] -> 
  Sem r Value
interpretDo statements = do
  env <- PR.ask
  executeStatements env statements
  where
    executeStatements :: 
      Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
      EvalEnv -> 
      [DoStatement] -> 
      Sem r Value
    executeStatements _env [] = 
      return $ VEffect (EmitEff (TELEmitEffect "unit")) -- Default return value using a CoreEffect
    executeStatements env (ExprStmt expr : rest) = do
      _ <- PR.local (const env) $ interpretExpr expr
      executeStatements env rest
    executeStatements env (BindStmt var expr : rest) = do
      val <- PR.local (const env) $ interpretExpr expr
      let newEnv = Map.insert var val env
      executeStatements newEnv rest
    executeStatements env (LetStmt decls : rest) = do
      newEnv <- foldM (\e (Declaration name val) -> do
                         v <- PR.local (const e) $ interpretExpr val
                         return $ Map.insert name v e) env decls
      executeStatements newEnv rest

-- | Apply an operator to two values
applyOperator :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  Operator -> 
  Value -> 
  Value -> 
  Sem r Value
applyOperator op left right = 
  case op of
    SequenceOp -> sequenceEffects left right
    ParallelOp -> parallelEffects left right
    ChoiceOp -> choiceEffects left right
    AddOp -> numericOp (+) left right
    SubOp -> numericOp (-) left right
    MulOp -> numericOp (*) left right
    DivOp -> numericOp div left right
    EqOp -> compareOp (==) left right
    NeqOp -> compareOp (/=) left right
    LtOp -> compareOp (<) left right
    GtOp -> compareOp (>) left right
    LeOp -> compareOp (<=) left right
    GeOp -> compareOp (>=) left right
    AndOp -> boolOp (&&) left right
    OrOp -> boolOp (||) left right

-- | Sequence two effects (>>) 
sequenceEffects :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  Value -> 
  Value -> 
  Sem r Value
sequenceEffects (VEffect e1) (VEffect e2) = do
  -- Execute the first effect
  time <- PS.get
  result1 <- applyEffect (convertEffect e1) (makeEmptyMetadata e1 time)
  case result1 of
    EffectFailure msg -> throw $ EffectError msg
    _ -> do
      -- Increment the time
      newTime <- incrementLocalTime
      -- Execute the second effect
      result2 <- applyEffect (convertEffect e2) (makeEmptyMetadata e2 newTime)
      case result2 of
        EffectFailure msg -> throw $ EffectError msg
        _ -> return $ VEffect e2
sequenceEffects v1 v2 = throw $ 
  TypeMismatch "Expected two effects for sequence operator (>>)" (VTuple [v1, v2])

-- | Execute effects in parallel (<|>)
parallelEffects :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  Value -> 
  Value -> 
  Sem r Value
parallelEffects (VEffect e1) (VEffect e2) = do
  -- In a real implementation, this would execute both effects in parallel
  -- For now, we just execute them sequentially and return a tuple of both effects
  time <- PS.get
  result1 <- applyEffect (convertEffect e1) (makeEmptyMetadata e1 time)
  result2 <- applyEffect (convertEffect e2) (makeEmptyMetadata e1 time)
  case (result1, result2) of
    (EffectFailure msg1, _) -> throw $ EffectError msg1
    (_, EffectFailure msg2) -> throw $ EffectError msg2
    _ -> return $ VTuple [VEffect e1, VEffect e2]
parallelEffects v1 v2 = throw $ 
  TypeMismatch "Expected two effects for parallel operator (<|>)" (VTuple [v1, v2])

-- | Execute first effect, if it fails, try the second (<|)
choiceEffects :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  Value -> 
  Value -> 
  Sem r Value
choiceEffects (VEffect e1) (VEffect e2) = do
  -- Try the first effect
  time <- PS.get
  result1 <- applyEffect (convertEffect e1) (makeEmptyMetadata e1 time)
  case result1 of
    EffectSuccess _ -> return $ VEffect e1
    _ -> do
      -- First effect failed, try the second
      result2 <- applyEffect (convertEffect e2) (makeEmptyMetadata e2 time)
      case result2 of
        EffectFailure msg -> throw $ EffectError msg
        _ -> return $ VEffect e2
choiceEffects v1 v2 = throw $ 
  TypeMismatch "Expected two effects for choice operator (<|)" (VTuple [v1, v2])

-- | Numeric operation for integer values
numericOp :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  (Integer -> Integer -> Integer) -> 
  Value -> 
  Value -> 
  Sem r Value
numericOp op (VInt a) (VInt b) = return $ VInt (op a b)
numericOp _ a b = throw $ TypeMismatch "Integer" (VTuple [a, b])

-- | Comparison operation for values
compareOp :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  (Integer -> Integer -> Bool) -> 
  Value -> 
  Value -> 
  Sem r Value
compareOp op (VInt a) (VInt b) = return $ VBool (op a b)
compareOp _ a b = throw $ TypeMismatch "Integer" (VTuple [a, b])

-- | Boolean operation for boolean values
boolOp :: 
  Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
  (Bool -> Bool -> Bool) -> 
  Value -> 
  Value -> 
  Sem r Value
boolOp op (VBool a) (VBool b) = return $ VBool (op a b)
boolOp _ a b = throw $ TypeMismatch "Bool" (VTuple [a, b])

-- | Helper for running a test program with mock effects
runTestProgram :: Program -> IO (Either InterpreterError Value)
runTestProgram prog = runM $ 
  runError $ 
  PR.runReader emptyEnv $ 
  PS.evalState (LamportTime 0) $ 
  mockResourceOps $ 
  mockEffectHandler $ 
  mockLogicalClock $
  evalProgram prog
  where
    evalProgram :: 
      Members '[PR.Reader EvalEnv, Error InterpreterError, PS.State LamportTime, ResourceOps, EffectHandler, LogicalClock] r => 
      Program -> 
      Sem r Value
    evalProgram p = do
      -- Find the main function or entry point
      case findMainFunction p of
        Just expr -> interpretExpr expr
        Nothing -> throw $ InvalidProgram "No main function found in program"
    
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
    mockResource :: ResourceInfo
    mockResource = ResourceInfo
      { resourceId = EntityHash $ computeHash $ TE.encodeUtf8 "mock-resource-id" -- ResourceHash
      , resourceOrigin = EntityHash $ computeHash $ TE.encodeUtf8 "mock-timeline" -- TimelineHash
      , resourceOwner = EntityHash $ computeHash $ TE.encodeUtf8 "mock-owner" -- ActorHash
      , resourceCapabilities = [TransferCapability, UpdateCapability] -- List of ResourceCapability
      , resourceMeta = TE.encodeUtf8 "metadata" -- ByteString
      , resourceSpentBy = Nothing -- Maybe Hash
      , resourceParents = [] -- [ResourceHash]
      , resourceTimestamp = LamportTime 0 -- LamportTime
      , resourceProvenanceChain = [EntityHash $ computeHash $ TE.encodeUtf8 "origin-timeline"] -- [TimelineHash]
      }

-- | Mock implementation of effect handling
mockEffectHandler :: Member (Embed IO) r => Sem (EffectHandler ': r) a -> Sem r a
mockEffectHandler = P.interpret $ \case
  ApplyEffect _effect _ -> 
    pure $ EffectSuccess "success"
  ValidateEffectPreconditions _ _ -> pure True

-- | Mock implementation of logical clock
mockLogicalClock :: Member (Embed IO) r => Sem (LogicalClock ': r) a -> Sem r a
mockLogicalClock = P.interpret $ \case
  GetLamportTime -> pure (LamportTime 0)
  IncrementTime -> pure (LamportTime 1)
  UpdateTime t -> pure t

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
  mockLogicalClock $
  interpretExpr expr

-- | Run an interpreter with the given environment
runTestExprWithEnv :: EvalEnv -> Expression -> IO (Either InterpreterError Value)
runTestExprWithEnv env expr = runM $ 
  runError $ 
  PR.runReader env $ 
  PS.evalState (LamportTime 0) $ 
  mockResourceOps $ 
  mockEffectHandler $ 
  mockLogicalClock $
  interpretExpr expr

-- | Extract variable names from patterns
extractVarNames :: Pattern -> [Identifier]
extractVarNames (VarPattern name) = [name]
extractVarNames (ConstructorPattern _ patterns) = concatMap extractVarNames patterns
extractVarNames _ = []  -- Other patterns don't bind variables 

-- | Convert internal CoreEffect to CE.Effect for serialization
toEffect :: CoreEffect -> CE.Effect
toEffect = \case
  DepositEff (TELDepositEffect rid amt pid) -> CE.DepositEffect rid amt pid
  WithdrawEff (TELWithdrawEffect rid amt pid) -> CE.WithdrawEffect rid amt pid
  TransferEff (TELTransferEffect rid spid dpid amt) -> CE.TransferEffect rid amt spid dpid
  ObserveEff (TELObserveEffect tid fact) -> CE.TimelineEffect tid fact
  EmitEff (TELEmitEffect event) -> CE.ProgramEffect (ProgramId "self") event
  InvokeEff (TELInvokeEffect program) -> CE.ProgramEffect (ProgramId "invoke") program
  SequenceEff e1 e2 -> CE.CompositeEffect [toEffect e1, toEffect e2]
  ParallelEff e1 e2 -> CE.CompositeEffect [toEffect e1, toEffect e2]
  ChoiceEff e1 e2 -> CE.CompositeEffect [toEffect e1, toEffect e2]

-- | Simple function to increment time for our purposes
incrementLocalTime :: Member (PS.State LamportTime) r => Sem r LamportTime
incrementLocalTime = do
  time <- PS.get
  let newTime = LamportTime (unLamportTime time + 1)
  PS.put newTime
  return newTime
  where
    unLamportTime (LamportTime t) = t 