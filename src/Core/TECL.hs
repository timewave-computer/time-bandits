{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module: Core.TECL
Description: Temporal Effect Combinator Language (TECL) Parser and Interpreter

This module provides the bridge between the TECL language and the Time Bandits
effect system. It includes:

1. The TECL parser that converts text to an AST
2. Type checking and validation for TECL programs
3. Translation from TECL to ProposedEffect instances
4. Integration with the effect application system

TECL (Temporal Effect Combinator Language) is a declarative language for
writing cross-timeline workflows for time travelers.
-}
module Core.TECL
  ( -- * Parsing
    parseTECL
  , parseFile
  
  -- * AST Types
  , TECLAST(..)
  , TECLStatement(..)
  , TECLExpression(..)
  , TECLEffect(..)
  , ParseError(..)
  
  -- * Type Checking
  , typecheckTECL
  , TypeCheckError(..)
  
  -- * Effect Generation
  , translateToEffects
  ) where

import Control.Exception (Exception)
import Control.Monad (forM, unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import Core.Effect (Effect, EffectId)
import Core.ActorId (ActorId)
import Programs.ProgramTypes (ProgramId)
import Core.Common (Hash, EntityHash, TimelineHash)
import qualified Data.ByteString.Base64 as B64

-- | The TECL Abstract Syntax Tree
data TECLAST = TECLAST
  { astStatements :: [TECLStatement]
  , astSourceFile :: Maybe FilePath
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A TECL statement
data TECLStatement
  = EffectStatement TECLEffect
  | DeclarationStatement Text TECLExpression
  | ConditionalStatement TECLExpression [TECLStatement] [TECLStatement]
  | RepeatStatement TECLExpression [TECLStatement]
  | TimeoutStatement TECLExpression [TECLStatement] [TECLStatement]
  | ObserveStatement Text TECLExpression
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A TECL expression
data TECLExpression
  = LiteralExpr LiteralValue
  | VariableExpr Text
  | BinaryExpr BinaryOp TECLExpression TECLExpression
  | UnaryExpr UnaryOp TECLExpression
  | CallExpr Text [TECLExpression]
  | TimeExpr TimeSpec
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A TECL effect
data TECLEffect
  = DepositEffect
      { depositTimeline :: Text
      , depositAsset :: Text
      , depositAmount :: TECLExpression
      }
  | WithdrawEffect
      { withdrawTimeline :: Text
      , withdrawAsset :: Text
      , withdrawAmount :: TECLExpression
      }
  | TransferEffect
      { transferTimeline :: Text
      , transferAsset :: Text
      , transferAmount :: TECLExpression
      , transferDestination :: Text
      }
  | ObserveEffect
      { observeTimeline :: Text
      , observeFact :: Text
      }
  | EmitFactEffect
      { emitFactName :: Text
      , emitFactValue :: TECLExpression
      }
  | InvokeEffect
      { invokeProgram :: Text
      , invokeCommand :: Text
      , invokeArgs :: [TECLExpression]
      }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Literal values in TECL
data LiteralValue
  = IntLiteral Int
  | DoubleLiteral Double
  | TextLiteral Text
  | BoolLiteral Bool
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Binary operators
data BinaryOp
  = AddOp | SubOp | MulOp | DivOp
  | EqOp | NeOp | LtOp | GtOp | LeOp | GeOp
  | AndOp | OrOp
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Unary operators
data UnaryOp
  = NegOp | NotOp
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Time specification
data TimeSpec
  = AbsoluteTime Text
  | RelativeTime Int TimeUnit
  | TimeWindow Int TimeUnit
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Time units
data TimeUnit
  = Seconds | Minutes | Hours | Days
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | TECL parse error
data ParseError = ParseError
  { errorMessage :: Text
  , errorLocation :: Maybe (Int, Int)  -- ^ Line and column
  , errorContext :: Text
  }
  deriving (Show, Eq, Generic, Exception, FromJSON, ToJSON)

-- | Type checking error
data TypeCheckError = TypeCheckError
  { typeErrorMessage :: Text
  , typeErrorLocation :: Maybe (Int, Int)
  , typeErrorExpected :: Text
  , typeErrorActual :: Text
  }
  deriving (Show, Eq, Generic, Exception, FromJSON, ToJSON)

-- | Parse TECL from text
parseTECL :: Text -> Either ParseError TECLAST
parseTECL input = 
  -- This is a placeholder implementation
  -- In a real implementation, we would use Parsec or Megaparsec
  
  -- Just for the sake of the example, we'll return a simple AST with a deposit effect
  Right TECLAST
    { astStatements = 
        [ EffectStatement
            (DepositEffect
              { depositTimeline = "Ethereum"
              , depositAsset = "ETH"
              , depositAmount = LiteralExpr (DoubleLiteral 1.0)
              })
        ]
    , astSourceFile = Nothing
    }

-- | Parse TECL from a file
parseFile :: FilePath -> IO (Either ParseError TECLAST)
parseFile path = do
  content <- TIO.readFile path
  let result = parseTECL content
  return $ case result of
    Right ast -> Right $ ast { astSourceFile = Just path }
    Left err -> Left err

-- | Type check a TECL AST
typecheckTECL :: TECLAST -> Either TypeCheckError TECLAST
typecheckTECL ast =
  -- This is a placeholder implementation
  -- In a real implementation, we would validate types
  Right ast

-- | Translate TECL to proposed effects
translateToEffects :: TECLAST -> [ProposedEffect]
translateToEffects ast =
  -- Convert each statement to a proposed effect
  concatMap statementToEffect (astStatements ast)
  where
    -- Convert a statement to a proposed effect
    statementToEffect :: TECLStatement -> [ProposedEffect]
    statementToEffect = \case
      EffectStatement effect -> [effectToProposed effect]
      DeclarationStatement name expr -> [] -- Declarations don't generate effects directly
      ConditionalStatement cond thenStmts elseStmts -> 
        -- For conditionals, we create a conditional effect that contains
        -- the effects from both branches
        let thenEffects = concatMap statementToEffect thenStmts
            elseEffects = concatMap statementToEffect elseStmts
        in [ProposedEffect 
              { proposedType = "conditional"
              , proposedPayload = Map.fromList 
                  [ ("condition", T.pack $ show cond)
                  , ("thenEffects", T.pack $ show thenEffects)
                  , ("elseEffects", T.pack $ show elseEffects)
                  ]
              , proposedParents = []
              }]
      RepeatStatement cond stmts ->
        -- For repeats, we create a repeat effect that contains
        -- the effects to be repeated
        let innerEffects = concatMap statementToEffect stmts
        in [ProposedEffect
              { proposedType = "repeat"
              , proposedPayload = Map.fromList
                  [ ("condition", T.pack $ show cond)
                  , ("effects", T.pack $ show innerEffects)
                  ]
              , proposedParents = []
              }]
      TimeoutStatement timeout stmts fallbackStmts ->
        -- For timeouts, we create a timeout effect that contains
        -- the effects to be executed and fallback effects
        let mainEffects = concatMap statementToEffect stmts
            fallbackEffects = concatMap statementToEffect fallbackStmts
        in [ProposedEffect
              { proposedType = "timeout"
              , proposedPayload = Map.fromList
                  [ ("timeout", T.pack $ show timeout)
                  , ("effects", T.pack $ show mainEffects)
                  , ("fallbackEffects", T.pack $ show fallbackEffects)
                  ]
              , proposedParents = []
              }]
      ObserveStatement factName expr ->
        -- For observations, we create an observe effect
        [ProposedEffect
          { proposedType = "observe"
          , proposedPayload = Map.fromList
              [ ("factName", factName)
              , ("expression", T.pack $ show expr)
              ]
          , proposedParents = []
          }]
    
    -- Convert a TECL effect to a proposed effect
    effectToProposed :: TECLEffect -> ProposedEffect
    effectToProposed = \case
      DepositEffect timeline asset amount ->
        ProposedEffect
          { proposedType = "deposit"
          , proposedPayload = Map.fromList
              [ ("timeline", timeline)
              , ("asset", asset)
              , ("amount", T.pack $ show amount)
              ]
          , proposedParents = []
          }
      WithdrawEffect timeline asset amount ->
        ProposedEffect
          { proposedType = "withdraw"
          , proposedPayload = Map.fromList
              [ ("timeline", timeline)
              , ("asset", asset)
              , ("amount", T.pack $ show amount)
              ]
          , proposedParents = []
          }
      TransferEffect timeline asset amount destination ->
        ProposedEffect
          { proposedType = "transfer"
          , proposedPayload = Map.fromList
              [ ("timeline", timeline)
              , ("asset", asset)
              , ("amount", T.pack $ show amount)
              , ("destination", destination)
              ]
          , proposedParents = []
          }
      ObserveEffect timeline fact ->
        ProposedEffect
          { proposedType = "observe"
          , proposedPayload = Map.fromList
              [ ("timeline", timeline)
              , ("fact", fact)
              ]
          , proposedParents = []
          }
      EmitFactEffect factName value ->
        ProposedEffect
          { proposedType = "emitFact"
          , proposedPayload = Map.fromList
              [ ("factName", factName)
              , ("value", T.pack $ show value)
              ]
          , proposedParents = []
          }
      InvokeEffect program command args ->
        ProposedEffect
          { proposedType = "invoke"
          , proposedPayload = Map.fromList
              [ ("program", program)
              , ("command", command)
              , ("args", T.pack $ show args)
              ]
          , proposedParents = []
          }

-- | A proposed effect (to be applied)
data ProposedEffect = ProposedEffect
  { proposedType :: Text
  , proposedPayload :: Map Text Text
  , proposedParents :: [Text]  -- Store parent IDs as Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON) 