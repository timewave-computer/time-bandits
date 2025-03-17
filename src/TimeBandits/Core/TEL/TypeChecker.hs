{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : TimeBandits.Core.TEL.TypeChecker
Description : Type checker for the Temporal Effect Language
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides type checking functionality for the Temporal Effect Language (TEL).
It validates that programs are well-typed and infers types for expressions.

@since 0.1.0
-}
module TimeBandits.Core.TEL.TypeChecker
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

import TimeBandits.Core.TEL.AST
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

-- | Extract identifiers from patterns for type environment extension
patternToIdentifier :: Pattern -> Maybe Identifier
patternToIdentifier (VarPattern ident) = Just ident
patternToIdentifier _ = Nothing  -- Other patterns don't bind variables directly

-- | Extend environment with a pattern binding
extendEnvPattern :: Pattern -> TypeExpr -> TypeEnv -> TypeEnv
extendEnvPattern p ty env = case patternToIdentifier p of
  Just ident -> Map.insert ident ty env
  Nothing -> env  -- Skip non-variable patterns

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
  -- Get the declared type if available
  declaredType <- case defSignature of
    Just TypeSignature{..} -> return (Just typeSigType)
    Nothing -> return Nothing
  
  -- Infer the type of the function
  inferredType <- typeCheckFunctionDef defFunction
  
  -- Check that the inferred type matches the declared type if present
  case declaredType of
    Just t -> unify t inferredType
    Nothing -> return ()

-- | Type check a function definition
typeCheckFunctionDef :: FunctionDef -> TypeCheck TypeExpr
typeCheckFunctionDef (SimpleFunctionDef name params body) = do
  -- Generate fresh type variables for parameters
  paramTypes <- mapM (\_ -> freshTypeVar) params
  
  -- Extend environment with parameter bindings
  let paramBindings = zipWith (\p t -> (p, t)) params paramTypes
  env <- ask
  let env' = foldr (\(p, t) e -> extendEnvPattern p t e) env paramBindings
  
  -- Type check the body in the extended environment
  bodyType <- local (const env') (typeCheckExpr body)
  
  -- Construct the function type
  return $ foldr (\paramType resultType -> FunctionType paramType resultType) bodyType paramTypes

typeCheckFunctionDef (GuardedFunctionDef name params guards) = do
  -- Similar to SimpleFunctionDef but handle guards
  -- Implementation simplified for brevity
  -- In a real implementation, each guard would be checked and the result types unified
  error "Not implemented: typeCheckFunctionDef for GuardedFunctionDef"

-- | Type check an expression
typeCheckExpr :: Expression -> TypeCheck TypeExpr
typeCheckExpr (LiteralExpr lit) = typeCheckLiteral lit
typeCheckExpr (VariableExpr name) = do
  env <- ask
  case Map.lookup name env of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable name
typeCheckExpr (ApplicationExpr func args) = do
  funcType <- typeCheckExpr func
  argTypes <- mapM typeCheckExpr args
  
  -- Check that func is a function type and has the right number of arguments
  resultType <- applyFunction funcType argTypes
  return resultType
  where
    applyFunction :: TypeExpr -> [TypeExpr] -> TypeCheck TypeExpr
    applyFunction funcType [] = return funcType
    applyFunction (FunctionType paramType resultType) (argType:restArgTypes) = do
      unify paramType argType
      applyFunction resultType restArgTypes
    applyFunction t _ = throwError $ NotAFunction t

-- For other expression types (simplified for brevity)
typeCheckExpr _ = error "Not implemented: typeCheckExpr for other expression types"

-- | Type check a literal
typeCheckLiteral :: LiteralExpr -> TypeCheck TypeExpr
typeCheckLiteral (IntLiteral _) = return $ BasicType "Int"
typeCheckLiteral (DoubleLiteral _) = return $ BasicType "Double"
typeCheckLiteral (TextLiteral _) = return $ BasicType "Text"
typeCheckLiteral (BoolLiteral _) = return $ BasicType "Bool"

-- | Generate a fresh type variable
freshTypeVar :: TypeCheck TypeExpr
freshTypeVar = do
  n <- S.get
  S.put (n + 1)
  return $ BasicType (T.pack $ "t" ++ show n)

-- | Unify two types
unify :: TypeExpr -> TypeExpr -> TypeCheck ()
unify t1 t2 | t1 == t2 = return ()
unify (BasicType v1) t2@(BasicType v2)
  | isTypeVar v1 && isTypeVar v2 = return ()  -- Both are type variables, any unification is valid
  | isTypeVar v1 = bindTypeVar v1 t2
  | isTypeVar v2 = bindTypeVar v2 (BasicType v1)
  | otherwise = if v1 == v2 then return () else throwError $ TypeMismatch (BasicType v1) (BasicType v2)
unify (FunctionType p1 r1) (FunctionType p2 r2) = do
  unify p1 p2
  unify r1 r2
unify t1 t2 = throwError $ TypeMismatch t1 t2

-- | Check if a name is a type variable
isTypeVar :: Text -> Bool
isTypeVar name = T.length name > 0 && T.head name == 't' && T.all isDigit (T.tail name)
  where
    isDigit c = c >= '0' && c <= '9'

-- | Bind a type variable to a type
bindTypeVar :: Text -> TypeExpr -> TypeCheck ()
bindTypeVar name t
  | occursCheck name t = throwError $ OccursCheck name t
  | otherwise = return ()  -- In a full implementation, this would update the substitution

-- | Check if a type variable occurs in a type
occursCheck :: Text -> TypeExpr -> Bool
occursCheck name (BasicType v) = v == name
occursCheck name (FunctionType p r) = occursCheck name p || occursCheck name r
occursCheck _ _ = False 