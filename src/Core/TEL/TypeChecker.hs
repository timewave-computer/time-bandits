{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Core.TEL.TypeChecker
Description : Type checker for the Temporal Effect Language
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides type checking functionality for the Temporal Effect Language (TEL).
It validates that programs are well-typed and infers types for expressions.
-}
module Core.TEL.TypeChecker
  ( -- * Type Checking Functions
    typeCheck
  , typeCheckExpr
  , typeCheckProgram
  
  -- * Type Error
  , TypeError(..)
  
  -- * Type Environment
  , TypeEnv
  , emptyEnv
  , extendEnv
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')

import Core.TEL.AST
  ( Program(..), Definition(..), TypeSignature(..), FunctionDef(..)
  , Pattern(..), GuardedExpr(..), Declaration(..)
  , TypeExpr(..), Expression(..), LiteralExpr(..), TimeExpr(AfterExpr, WithinExpr, AtExpr)
  , TimeUnit(..), EffectExpr(..), DoStatement(..)
  , Operator(..), Identifier, HashLiteral
  )

-- | Type checking error
data TypeError
  = TypeMismatch TypeExpr TypeExpr
  | UnboundVariable Identifier
  | OccursCheck Identifier TypeExpr
  | InfiniteType
  | UndefinedConstructor Identifier
  | PatternMatchError Pattern TypeExpr
  | NotAFunction TypeExpr
  | WrongNumberOfArguments Int Int
  | EffectError Text
  | UndefinedType Identifier
  | MissingImplementation Text
  deriving (Eq, Show)

-- | Type environment mapping variables to their types
type TypeEnv = Map Identifier TypeExpr

-- | Substitution mapping type variables to types
type Substitution = Map Identifier TypeExpr

-- | Empty type environment
emptyEnv :: TypeEnv
emptyEnv = Map.empty

-- | Extend type environment with a new binding
extendEnv :: Identifier -> TypeExpr -> TypeEnv -> TypeEnv
extendEnv = Map.insert

-- | Type checking monad
type TypeCheck a = ReaderT TypeEnv (S.StateT Int (Except TypeError)) a

-- | Type check a TEL program
typeCheck :: Program -> Either TypeError Program
typeCheck prog = runExcept $ S.evalStateT (runReaderT (typeCheckProgram prog) emptyEnv) 0

-- | Type check a program
typeCheckProgram :: Program -> TypeCheck Program
typeCheckProgram prog@Program{..} = do
  -- Collect function signatures
  let env = foldl' collectSignatures emptyEnv programDefinitions
  
  -- Type check each definition with all signatures in scope
  local (const env) $ do
    mapM_ typeCheckDefinition programDefinitions
    return prog
  where
    collectSignatures env def = case defSignature def of
      Just TypeSignature{..} -> extendEnv typeSigName typeSigType env
      Nothing -> env

-- | Type check a definition
typeCheckDefinition :: Definition -> TypeCheck ()
typeCheckDefinition Definition{..} = do
  case defSignature of
    Just sig@TypeSignature{..} -> do
      -- If there's a signature, validate the implementation against it
      inferredType <- typeInferFunctionDef defFunction
      unify inferredType typeSigType
    
    Nothing -> do
      -- If there's no signature, just infer the type
      _ <- typeInferFunctionDef defFunction
      return ()

-- | Type infer a function definition
typeInferFunctionDef :: FunctionDef -> TypeCheck TypeExpr
typeInferFunctionDef (SimpleFunctionDef name params body) = do
  -- Generate fresh type variables for parameters
  paramTypes <- replicateM' (length params) freshTypeVar
  
  -- Bind parameters to their type variables
  let paramBindings = zip (extractPatternVars params) paramTypes
      paramEnv = foldl' (\env (var, ty) -> extendEnv var ty env) emptyEnv paramBindings
  
  -- Infer the body type in the extended environment
  bodyType <- local (Map.union paramEnv) $ typeInferExpr body
  
  -- Construct the function type
  return $ foldr FunctionType bodyType paramTypes

typeInferFunctionDef (GuardedFunctionDef name params guards) = do
  -- This is similar to SimpleFunctionDef but need to check all guards
  -- For now, just implementing a placeholder
  throwError $ MissingImplementation "GuardedFunctionDef type inference not yet implemented"

-- | Extract variable names from patterns
extractPatternVars :: [Pattern] -> [Identifier]
extractPatternVars = concatMap go
  where
    go :: Pattern -> [Identifier]
    go (VarPattern name) = [name]
    go (ConstructorPattern _ patterns) = concatMap go patterns
    go _ = []

-- | Type check an expression and return its type
typeCheckExpr :: TypeEnv -> Expression -> Either TypeError TypeExpr
typeCheckExpr env expr = runExcept $ S.evalStateT (runReaderT (typeInferExpr expr) env) 0

-- | Type infer an expression
typeInferExpr :: Expression -> TypeCheck TypeExpr
typeInferExpr expr = case expr of
  LiteralExpr lit -> typeInferLiteral lit
  
  VariableExpr name -> do
    env <- ask
    case Map.lookup name env of
      Just ty -> return ty
      Nothing -> throwError $ UnboundVariable name
  
  ApplicationExpr func args -> do
    funcType <- typeInferExpr func
    argTypes <- mapM typeInferExpr args
    resultType <- freshTypeVar
    
    -- Construct the expected function type for all arguments
    let expectedFuncType = foldr FunctionType resultType argTypes
    
    -- Unify with the actual function type
    unify funcType expectedFuncType
    
    return resultType
  
  LambdaExpr params body -> do
    -- Generate fresh type variables for parameters
    paramTypes <- replicateM' (length params) freshTypeVar
    
    -- Bind parameters to their type variables
    let paramBindings = zip (extractPatternVars params) paramTypes
        paramEnv = foldl' (\env (var, ty) -> extendEnv var ty env) emptyEnv paramBindings
    
    -- Infer the body type in the extended environment
    bodyType <- local (Map.union paramEnv) $ typeInferExpr body
    
    -- Construct the function type
    return $ foldr FunctionType bodyType paramTypes
  
  EffectExpr effectExpr -> typeInferEffect effectExpr
  
  LetExpr decls body -> do
    -- Create type variables for each declaration
    declVars <- replicateM' (length decls) freshTypeVar
    let declNames = map declName decls
        declBindings = zip declNames declVars
        declEnv = foldl' (\env (var, ty) -> extendEnv var ty env) emptyEnv declBindings
    
    -- Type check each declaration in the extended environment
    local (Map.union declEnv) $ do
      -- Infer types for each declaration
      declTypes <- mapM (typeInferExpr . declValue) decls
      
      -- Unify inferred types with assigned type variables
      zipWithM_' unify declVars declTypes
      
      -- Infer the body type in the extended environment
      typeInferExpr body
  
  IfExpr cond thenExpr elseExpr -> do
    condType <- typeInferExpr cond
    unify condType (BasicType "Bool")
    
    thenType <- typeInferExpr thenExpr
    elseType <- typeInferExpr elseExpr
    
    unify thenType elseType
    return thenType
  
  CaseExpr scrutinee patterns -> do
    scrutineeType <- typeInferExpr scrutinee
    
    -- Type check each pattern and result expression
    resultTypes <- forM patterns $ \(pattern, resultExpr) -> do
      -- Type check the pattern
      patternEnv <- typeCheckPattern pattern scrutineeType
      
      -- Type check the result expression in extended environment
      local (Map.union patternEnv) $ typeInferExpr resultExpr
    
    -- Ensure all result types are the same
    case resultTypes of
      [] -> freshTypeVar  -- If no patterns, return a fresh type variable
      (firstType:rest) -> do
        -- Unify first type with all other result types
        forM_ rest $ \ty -> unify firstType ty
        return firstType
  
  TimeExpr timeExpr -> typeInferTimeExpr timeExpr
  
  InfixExpr e1 op e2 -> typeInferInfixExpr e1 op e2
  
  HashRefExpr _ -> do
    -- In a real implementation, look up the hash's type in a repository
    -- For now, just return a placeholder type
    freshTypeVar
  
  DoExpr stmts -> do
    -- Type check a do-block
    -- For now, just a placeholder
    throwError $ MissingImplementation "Do-block type inference not yet implemented"

-- | Type check a pattern against a type, returning an environment with new bindings
typeCheckPattern :: Pattern -> TypeExpr -> TypeCheck TypeEnv
typeCheckPattern pattern ty = case pattern of
  WildcardPattern -> return emptyEnv
  
  VarPattern name -> return $ Map.singleton name ty
  
  LiteralPattern lit -> do
    -- Infer the literal's type and unify with expected type
    litType <- typeInferLiteral lit
    unify litType ty
    return emptyEnv
  
  ConstructorPattern name patterns -> do
    -- In a real implementation, look up the constructor's type
    -- For now, just return a placeholder
    throwError $ MissingImplementation "Constructor pattern type checking not yet implemented"

-- | Type infer a literal
typeInferLiteral :: LiteralExpr -> TypeCheck TypeExpr
typeInferLiteral lit = case lit of
  IntLiteral _ -> return $ BasicType "Int"
  DoubleLiteral _ -> return $ BasicType "Double"
  TextLiteral _ -> return $ BasicType "Text"
  BoolLiteral _ -> return $ BasicType "Bool"
  ListLiteral exprs -> do
    if null exprs
      then do
        elemType <- freshTypeVar
        return $ ListType elemType
      else do
        -- Process list elements
        case exprs of
          [] -> do
            elemType <- freshTypeVar
            return $ ListType elemType
          (firstExpr:rest) -> do
            -- Infer type of first element
            elemType <- typeInferExpr firstExpr
            
            -- Check all elements have the same type
            forM_ rest $ \expr -> do
              exprType <- typeInferExpr expr
              unify elemType exprType
            
            return $ ListType elemType
  
  TupleLiteral exprs -> do
    elemTypes <- mapM typeInferExpr exprs
    return $ TupleType elemTypes

-- | Type infer an effect expression
typeInferEffect :: EffectExpr -> TypeCheck TypeExpr
typeInferEffect effect = case effect of
  DepositExpr amount target timeline -> do
    -- Amount should be a numeric type
    amountType <- typeInferExpr amount
    unify amountType (BasicType "Double") `catchError` \_ ->
      unify amountType (BasicType "Int")
    
    -- Target and timeline can be any type for now
    _ <- typeInferExpr target
    _ <- typeInferExpr timeline
    
    -- The result is an Effect with no return value
    return $ EffectType (BasicType "()") 
  
  WithdrawExpr amount source timeline -> do
    -- Amount should be a numeric type
    amountType <- typeInferExpr amount
    unify amountType (BasicType "Double") `catchError` \_ ->
      unify amountType (BasicType "Int")
    
    -- Source and timeline can be any type for now
    _ <- typeInferExpr source
    _ <- typeInferExpr timeline
    
    -- The result is an Effect with no return value
    return $ EffectType (BasicType "()")
  
  TransferExpr amount source target timeline -> do
    -- Amount should be a numeric type
    amountType <- typeInferExpr amount
    unify amountType (BasicType "Double") `catchError` \_ ->
      unify amountType (BasicType "Int")
    
    -- Source, target, and timeline can be any type for now
    _ <- typeInferExpr source
    _ <- typeInferExpr target
    _ <- typeInferExpr timeline
    
    -- The result is an Effect with no return value
    return $ EffectType (BasicType "()")
  
  ObserveExpr what timeline -> do
    -- What and timeline can be any type for now
    whatType <- typeInferExpr what
    _ <- typeInferExpr timeline
    
    -- The result is an Effect with the observed value's type
    return $ EffectType whatType
  
  EmitExpr value -> do
    -- Value can be any type
    valueType <- typeInferExpr value
    
    -- The result is an Effect with no return value
    return $ EffectType (BasicType "()")
  
  InvokeExpr value -> do
    -- Value should be a function
    valueType <- typeInferExpr value
    
    -- The result is an Effect with the function's return type
    -- In a real implementation, we'd extract the return type from the function type
    -- For now, just return a placeholder
    resultType <- freshTypeVar
    return $ EffectType resultType

-- | Type infer a time expression
typeInferTimeExpr :: TimeExpr -> TypeCheck TypeExpr
typeInferTimeExpr timeExpr = case timeExpr of
  AfterExpr amount unit action -> do
    -- Amount should be a numeric type
    amountType <- typeInferExpr amount
    unify amountType (BasicType "Int")
    
    -- Action can be any type
    actionType <- typeInferExpr action
    
    -- The result has the same type as the action
    return actionType
  
  WithinExpr amount unit action -> do
    -- Amount should be a numeric type
    amountType <- typeInferExpr amount
    unify amountType (BasicType "Int")
    
    -- Action can be any type
    actionType <- typeInferExpr action
    
    -- The result has the same type as the action
    return actionType
  
  AtExpr time action -> do
    -- Time should be a time type
    timeType <- typeInferExpr time
    unify timeType (BasicType "Time")
    
    -- Action can be any type
    actionType <- typeInferExpr action
    
    -- The result has the same type as the action
    return actionType

-- | Type infer an infix expression
typeInferInfixExpr :: Expression -> Operator -> Expression -> TypeCheck TypeExpr
typeInferInfixExpr e1 op e2 = do
  t1 <- typeInferExpr e1
  t2 <- typeInferExpr e2
  
  case op of
    -- Arithmetic operators
    AddOp -> do
      unifyNumeric t1
      unifyNumeric t2
      unify t1 t2
      return t1
    
    SubOp -> do
      unifyNumeric t1
      unifyNumeric t2
      unify t1 t2
      return t1
    
    MulOp -> do
      unifyNumeric t1
      unifyNumeric t2
      unify t1 t2
      return t1
    
    DivOp -> do
      unifyNumeric t1
      unifyNumeric t2
      unify t1 t2
      return t1
    
    -- Comparison operators
    EqOp -> do
      unify t1 t2
      return $ BasicType "Bool"
    
    NeqOp -> do
      unify t1 t2
      return $ BasicType "Bool"
    
    LtOp -> do
      unifyNumeric t1
      unifyNumeric t2
      unify t1 t2
      return $ BasicType "Bool"
    
    GtOp -> do
      unifyNumeric t1
      unifyNumeric t2
      unify t1 t2
      return $ BasicType "Bool"
    
    LeOp -> do
      unifyNumeric t1
      unifyNumeric t2
      unify t1 t2
      return $ BasicType "Bool"
    
    GeOp -> do
      unifyNumeric t1
      unifyNumeric t2
      unify t1 t2
      return $ BasicType "Bool"
    
    -- Logical operators
    AndOp -> do
      unify t1 (BasicType "Bool")
      unify t2 (BasicType "Bool")
      return $ BasicType "Bool"
    
    OrOp -> do
      unify t1 (BasicType "Bool")
      unify t2 (BasicType "Bool")
      return $ BasicType "Bool"
    
    -- Combinators
    SequenceOp -> do
      -- First expression should be an effect
      case t1 of
        EffectType _ -> return ()
        _ -> throwError $ TypeMismatch (EffectType (BasicType "a")) t1
      
      -- Second expression should be an effect
      case t2 of
        EffectType resultType -> return $ EffectType resultType
        _ -> throwError $ TypeMismatch (EffectType (BasicType "b")) t2
    
    ParallelOp -> do
      -- Both expressions should be effects
      case (t1, t2) of
        (EffectType _, EffectType _) -> do
          -- Result is an effect with a tuple of both results
          resultType <- freshTypeVar
          return $ EffectType resultType
        _ -> throwError $ EffectError "Parallel combinator requires effects on both sides"
    
    ChoiceOp -> do
      -- Both expressions should be effects of the same type
      case (t1, t2) of
        (EffectType r1, EffectType r2) -> do
          unify r1 r2
          return $ EffectType r1
        _ -> throwError $ EffectError "Choice combinator requires effects on both sides"

-- | Unify a type with a numeric type (Int or Double)
unifyNumeric :: TypeExpr -> TypeCheck ()
unifyNumeric ty = do
  intUnify <- catchError (unify ty (BasicType "Int") >> return True) (const $ return False)
  if intUnify
    then return ()
    else unify ty (BasicType "Double")

-- | Unify two types (make them equal)
unify :: TypeExpr -> TypeExpr -> TypeCheck ()
unify t1 t2 = case (t1, t2) of
  -- Same basic types unify
  (BasicType a, BasicType b) | a == b -> return ()
  
  -- List types unify if their element types unify
  (ListType a, ListType b) -> unify a b
  
  -- Tuple types unify if they have the same length and corresponding elements unify
  (TupleType as, TupleType bs)
    | length as == length bs -> zipWithM_' unify as bs
    | otherwise -> throwError $ TypeMismatch t1 t2
  
  -- Function types unify if their parameter and return types unify
  (FunctionType a1 r1, FunctionType a2 r2) -> do
    unify a1 a2
    unify r1 r2
  
  -- Effect types unify if their result types unify
  (EffectType a, EffectType b) -> unify a b
  
  -- Timeline types unify if their content types unify
  (TimelineType a, TimelineType b) -> unify a b
  
  -- Otherwise, types don't unify
  _ -> throwError $ TypeMismatch t1 t2

-- | Generate a fresh type variable
freshTypeVar :: TypeCheck TypeExpr
freshTypeVar = do
  n <- get
  put (n + 1)
  return $ BasicType $ T.pack $ "a" ++ show n

-- | Helper to run monadic operations multiple times
replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n m
  | n <= 0    = return []
  | otherwise = do
      x <- m
      xs <- replicateM' (n-1) m
      return (x:xs)

-- | Helper for zipping with a monadic function
zipWithM_' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_' f as bs = do
  _ <- zipWithM' f as bs
  return ()

-- | Helper for zipping with a monadic function
zipWithM' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' _ [] _ = return []
zipWithM' _ _ [] = return []
zipWithM' f (a:as) (b:bs) = do
  c <- f a b
  cs <- zipWithM' f as bs
  return (c:cs) 