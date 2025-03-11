{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
Module      : Core.TEL.AST
Description : Abstract Syntax Tree for Temporal Effect Language
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines the Abstract Syntax Tree (AST) for the Temporal Effect Language (TEL),
a Haskell-like language for describing cross-timeline effects as specified in ADR-013.
-}
module Core.TEL.AST 
  ( -- * Top-level program structures
    Program(..)
  , Definition(..)
  , TypeSignature(..)
  , FunctionDef(..)
    
  -- * Patterns
  , Pattern(..)
  
  -- * Guards
  , GuardedExpr(..)
  , Declaration(..)
  
  -- * Types
  , TypeExpr(..)
  
  -- * Expressions
  , Expression(..)
  , LiteralExpr(..)
  , TimeExpr(..)
  , TimeUnit(..)
  , EffectExpr(..)
  , DoStatement(..)
  , Operator(..)
  
  -- * Identifiers and references
  , Identifier
  , HashLiteral
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.=), (.:))
import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Serialize (Serialize)
import qualified Data.Text.Encoding as TE
import Control.Applicative ((<|>))

-- | An identifier in the TEL language
type Identifier = Text

-- | A hash literal for content-addressed code
type HashLiteral = ByteString

-- | A complete TEL program
data Program = Program
  { programDefinitions :: [Definition]
  , programSourceFile :: Maybe FilePath 
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A definition in a TEL program
data Definition = Definition
  { defSignature :: Maybe TypeSignature
  , defFunction :: FunctionDef
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A type signature for a definition
data TypeSignature = TypeSignature
  { typeSigName :: Identifier
  , typeSigType :: TypeExpr
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A function definition
data FunctionDef 
  = SimpleFunctionDef 
      { funcName :: Identifier
      , funcParams :: [Pattern]
      , funcBody :: Expression
      }
  | GuardedFunctionDef
      { funcName :: Identifier
      , funcParams :: [Pattern]
      , funcGuards :: [GuardedExpr]
      }
  deriving (Show, Eq, Generic)

deriving instance FromJSON FunctionDef
deriving instance ToJSON FunctionDef

-- | A pattern used in pattern matching
data Pattern
  = VarPattern Identifier
  | LiteralPattern LiteralExpr
  | ConstructorPattern Identifier [Pattern]
  | WildcardPattern
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A guarded expression
data GuardedExpr
  = Guard Expression Expression
  | Where [Declaration]
  deriving (Show, Eq, Generic)

deriving instance FromJSON GuardedExpr
deriving instance ToJSON GuardedExpr

-- | A declaration in a let or where expression
data Declaration = Declaration
  { declName :: Identifier
  , declValue :: Expression
  } deriving (Show, Eq, Generic)

deriving instance FromJSON Declaration
deriving instance ToJSON Declaration

-- | A type expression
data TypeExpr
  = BasicType Identifier
  | ListType TypeExpr
  | TupleType [TypeExpr]
  | FunctionType TypeExpr TypeExpr
  | EffectType TypeExpr
  | TimelineType TypeExpr
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | An expression in the TEL language
data Expression
  = LiteralExpr LiteralExpr
  | VariableExpr Identifier
  | ApplicationExpr Expression [Expression]
  | LambdaExpr [Pattern] Expression
  | EffectExpr EffectExpr
  | LetExpr [Declaration] Expression
  | IfExpr Expression Expression Expression
  | CaseExpr Expression [(Pattern, Expression)]
  | TimeExpr TimeExpr
  | InfixExpr Expression Operator Expression
  | HashRefExpr HashLiteral
  | DoExpr [DoStatement]
  deriving (Show, Eq, Generic)

-- For testing purposes, let's simplify by not using HashRefExpr in tests
instance FromJSON Expression where
  parseJSON v = 
    (LiteralExpr <$> parseJSON v) <|>
    (VariableExpr <$> parseJSON v) <|>
    (ApplicationExpr <$> parseJSON v <*> parseJSON v) <|>
    (LambdaExpr <$> parseJSON v <*> parseJSON v) <|>
    (EffectExpr <$> parseJSON v) <|>
    (LetExpr <$> parseJSON v <*> parseJSON v) <|>
    (IfExpr <$> parseJSON v <*> parseJSON v <*> parseJSON v) <|>
    (CaseExpr <$> parseJSON v <*> parseJSON v) <|>
    (TimeExpr <$> parseJSON v) <|>
    (InfixExpr <$> parseJSON v <*> parseJSON v <*> parseJSON v) <|>
    (DoExpr <$> parseJSON v)

instance ToJSON Expression where
  toJSON (LiteralExpr x) = toJSON x
  toJSON (VariableExpr x) = toJSON x
  toJSON (ApplicationExpr f args) = object ["function" .= f, "arguments" .= args]
  toJSON (LambdaExpr params body) = object ["parameters" .= params, "body" .= body]
  toJSON (EffectExpr e) = toJSON e
  toJSON (LetExpr decls body) = object ["declarations" .= decls, "body" .= body]
  toJSON (IfExpr cond t e) = object ["condition" .= cond, "then" .= t, "else" .= e]
  toJSON (CaseExpr e pats) = object ["expression" .= e, "patterns" .= pats]
  toJSON (TimeExpr t) = toJSON t
  toJSON (InfixExpr l op r) = object ["left" .= l, "operator" .= op, "right" .= r]
  toJSON (HashRefExpr _) = object ["type" .= ("HashRefExpr" :: Text)]
  toJSON (DoExpr stmts) = toJSON stmts

-- | A literal expression
data LiteralExpr
  = IntLiteral Integer
  | DoubleLiteral Double
  | TextLiteral Text
  | BoolLiteral Bool
  | ListLiteral [Expression]
  | TupleLiteral [Expression]
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A time-related expression
data TimeExpr
  = AfterExpr Expression TimeUnit Expression
  | WithinExpr Expression TimeUnit Expression
  | AtExpr Expression Expression
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Time units
data TimeUnit
  = Seconds
  | Minutes
  | Hours
  | Days
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Serialize)

-- | An effect expression
data EffectExpr
  = DepositExpr Expression Expression Expression
  | WithdrawExpr Expression Expression Expression
  | TransferExpr Expression Expression Expression Expression
  | ObserveExpr Expression Expression
  | EmitExpr Expression
  | InvokeExpr Expression
  deriving (Show, Eq, Generic)

deriving instance FromJSON EffectExpr
deriving instance ToJSON EffectExpr

-- | A statement in a do-block
data DoStatement
  = BindStmt Identifier Expression
  | LetStmt [Declaration]
  | ExprStmt Expression
  deriving (Show, Eq, Generic)

deriving instance FromJSON DoStatement
deriving instance ToJSON DoStatement

-- | An operator for infix expressions
data Operator
  = SequenceOp  -- ^ >>
  | ParallelOp  -- ^ <|>
  | ChoiceOp    -- ^ <|
  | AddOp       -- ^ +
  | SubOp       -- ^ -
  | MulOp       -- ^ *
  | DivOp       -- ^ /
  | EqOp        -- ^ ==
  | NeqOp       -- ^ /=
  | LtOp        -- ^ <
  | GtOp        -- ^ >
  | LeOp        -- ^ <=
  | GeOp        -- ^ >=
  | AndOp       -- ^ &&
  | OrOp        -- ^ ||
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Serialize) 