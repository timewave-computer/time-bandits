{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : TimeBandits.Core.TEL.PrettyPrinter
Description : Pretty printer for the Temporal Effect Language
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides pretty printing functionality for the Temporal Effect
Language (TEL) AST, converting it back to its textual representation.

@since 0.1.0
-}
module TimeBandits.Core.TEL.PrettyPrinter 
  ( -- * Pretty Printing Functions
    prettyPrint
  , prettyPrintProgram
  , prettyPrintExpression
  
  -- * Customization Options
  , PrettyOptions(..)
  , defaultOptions
  , compactOptions
  , verbose
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16
import Data.List (intersperse)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import TimeBandits.Core.TEL.AST
  ( Program(..), Definition(..), TypeSignature(..), FunctionDef(..)
  , Pattern(..), GuardedExpr(..), Declaration(..)
  , TypeExpr(..), Expression(..), LiteralExpr(..), TimeExpr(AfterExpr, WithinExpr, AtExpr)
  , TimeUnit(..), EffectExpr(..), DoStatement(..)
  , Operator(..), Identifier, HashLiteral
  )

-- | Pretty printer options
data PrettyOptions = PrettyOptions
  { indentWidth :: Int       -- ^ Number of spaces per indentation level
  , lineWidth   :: Int       -- ^ Maximum line width
  , showTypes   :: Bool      -- ^ Whether to include type signatures
  , verbose     :: Bool      -- ^ Whether to include extra information
  }

-- | Default pretty printer options
defaultOptions :: PrettyOptions
defaultOptions = PrettyOptions
  { indentWidth = 2
  , lineWidth   = 80
  , showTypes   = True
  , verbose     = False
  }

-- | Compact pretty printer options
compactOptions :: PrettyOptions
compactOptions = PrettyOptions
  { indentWidth = 2
  , lineWidth   = 120
  , showTypes   = False
  , verbose     = False
  }

-- | Pretty print a TEL program with default options
prettyPrint :: Program -> Text
prettyPrint = prettyPrintProgram defaultOptions

-- | Pretty print a TEL program with custom options
prettyPrintProgram :: PrettyOptions -> Program -> Text
prettyPrintProgram opts prog =
  renderStrict $ layoutPretty layoutOptions $ programDoc opts prog
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine (lineWidth opts) 1.0 }

-- | Pretty print a TEL expression with default options
prettyPrintExpression :: Expression -> Text
prettyPrintExpression = prettyPrintExpr defaultOptions

-- | Pretty print a TEL expression with custom options
prettyPrintExpr :: PrettyOptions -> Expression -> Text
prettyPrintExpr opts expr =
  renderStrict $ layoutPretty layoutOptions $ expressionDoc opts expr
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine (lineWidth opts) 1.0 }

-- Implementation of document generators for each AST type
-- (These functions generate the pretty-printed document representation)

-- | Generate a document for a program
programDoc :: PrettyOptions -> Program -> Doc ann
programDoc opts (Program defs _) =
  vsep $ map (definitionDoc opts) defs

-- | Generate a document for a definition
definitionDoc :: PrettyOptions -> Definition -> Doc ann
definitionDoc opts (Definition maybeSig funcDef) =
  case maybeSig of
    Just sig | showTypes opts -> typeSignatureDoc opts sig <> line
    _ -> telEmptyDoc
  <> functionDefDoc opts funcDef

-- | Generate a document for a type signature
typeSignatureDoc :: PrettyOptions -> TypeSignature -> Doc ann
typeSignatureDoc opts (TypeSignature name ty) =
  pretty name <+> "::" <+> typeExprDoc opts ty

-- | Generate a document for a type expression
typeExprDoc :: PrettyOptions -> TypeExpr -> Doc ann
typeExprDoc opts typeExpr = case typeExpr of
  BasicType name -> pretty name
  FunctionType paramType returnType ->
    let paramDoc = case paramType of
          FunctionType _ _ -> parens (typeExprDoc opts paramType)
          _ -> typeExprDoc opts paramType
    in paramDoc <+> "->" <+> typeExprDoc opts returnType
  ListType elemType -> brackets (typeExprDoc opts elemType)
  TupleType types -> 
    tupled (map (typeExprDoc opts) types)
  EffectType innerType ->
    "Effect" <+> parens (typeExprDoc opts innerType)
  TimelineType innerType ->
    "Timeline" <+> parens (typeExprDoc opts innerType)

-- | Generate a document for a function definition
functionDefDoc :: PrettyOptions -> FunctionDef -> Doc ann
functionDefDoc opts funcDef = case funcDef of
  SimpleFunctionDef name params body ->
    let paramDocs = map (patternDoc opts) params
        bodyDoc = expressionDoc opts body
    in pretty name <+> hsep paramDocs <+> "=" <> line <>
       indent (indentWidth opts) bodyDoc
       
  GuardedFunctionDef name params guards ->
    let paramDocs = map (patternDoc opts) params
        guardDocs = map (guardedExprDoc opts) guards
    in pretty name <+> hsep paramDocs <+> "=" <> line <>
       indent (indentWidth opts) (vsep guardDocs)

-- | Generate a document for a pattern
patternDoc :: PrettyOptions -> Pattern -> Doc ann
patternDoc opts pattern = case pattern of
  VarPattern name -> pretty name
  WildcardPattern -> "_"
  LiteralPattern lit -> literalDoc opts lit
  ConstructorPattern name patterns -> 
    pretty name <+> hsep (map (patternDoc opts) patterns)

-- | Generate a document for a literal expression
literalDoc :: PrettyOptions -> LiteralExpr -> Doc ann
literalDoc _ lit = case lit of
  IntLiteral n -> pretty n
  DoubleLiteral d -> pretty d
  TextLiteral s -> dquotes (pretty s)
  BoolLiteral b -> if b then "True" else "False"
  ListLiteral exprs -> 
    brackets (hsep (punctuate comma (map (\e -> expressionDoc defaultOptions e) exprs)))
  TupleLiteral exprs ->
    tupled (map (\e -> expressionDoc defaultOptions e) exprs)

-- | Generate a document for an expression
expressionDoc :: PrettyOptions -> Expression -> Doc ann
expressionDoc opts expr = case expr of
  LiteralExpr lit -> 
    literalDoc opts lit
    
  VariableExpr name -> 
    pretty name
    
  ApplicationExpr func args ->
    let funcDoc = case func of
          LambdaExpr _ _ -> parens (expressionDoc opts func)
          _ -> expressionDoc opts func
        argDocs = map (\arg -> case arg of
                          LambdaExpr _ _ -> parens (expressionDoc opts arg)
                          ApplicationExpr _ _ -> parens (expressionDoc opts arg)
                          _ -> expressionDoc opts arg) args
    in funcDoc <+> hsep argDocs
    
  LambdaExpr params body ->
    let paramDocs = map (patternDoc opts) params
        bodyDoc = expressionDoc opts body
    in "\\" <> hsep paramDocs <+> "->" <+> bodyDoc
    
  LetExpr decls body ->
    let declDocs = map (declarationDoc opts) decls
        bodyDoc = expressionDoc opts body
    in "let" <+> align (vsep declDocs) <> line <>
       "in" <+> bodyDoc
    
  IfExpr cond thenExpr elseExpr ->
    let condDoc = expressionDoc opts cond
        thenDoc = expressionDoc opts thenExpr
        elseDoc = expressionDoc opts elseExpr
    in "if" <+> condDoc <> line <>
       indent (indentWidth opts) ("then" <+> thenDoc) <> line <>
       indent (indentWidth opts) ("else" <+> elseDoc)
    
  CaseExpr scrutinee patterns ->
    let scrutineeDoc = expressionDoc opts scrutinee
        patternDocs = map (\(pat, expr) -> 
                          patternDoc opts pat <+> "->" <+>
                          indent (indentWidth opts) (expressionDoc opts expr))
                     patterns
    in "case" <+> scrutineeDoc <+> "of" <> line <>
       indent (indentWidth opts) (vsep patternDocs)
    
  EffectExpr effect ->
    effectDoc opts effect
    
  TimeExpr timeExpr ->
    timeExprDoc opts timeExpr
    
  InfixExpr left op right ->
    let leftDoc = expressionDoc opts left
        rightDoc = expressionDoc opts right
        opDoc = case op of
          SequenceOp -> ">>"
          ParallelOp -> "<|>"
          ChoiceOp -> "<|"
          AddOp -> "+"
          SubOp -> "-"
          MulOp -> "*"
          DivOp -> "/"
          EqOp -> "=="
          NeqOp -> "/="
          LtOp -> "<"
          GtOp -> ">"
          LeOp -> "<="
          GeOp -> ">="
          AndOp -> "&&"
          OrOp -> "||"
    in leftDoc <+> opDoc <+> rightDoc
    
  HashRefExpr hash ->
    "#" <> pretty (T.pack $ show hash)
    
  DoExpr stmts ->
    let stmtDocs = map doStatementDoc' stmts
    in "do" <> line <>
       indent (indentWidth opts) (vsep stmtDocs)
    where
      doStatementDoc' :: DoStatement -> Doc ann
      doStatementDoc' stmt = case stmt of
        BindStmt ident expr ->
          pretty ident <+> "<-" <+> expressionDoc opts expr
        
        ExprStmt expr ->
          expressionDoc opts expr
        
        LetStmt decls ->
          "let" <+> align (vsep (map (declarationDoc opts) decls))

-- | Generate a document for a declaration
declarationDoc :: PrettyOptions -> Declaration -> Doc ann
declarationDoc opts decl =
  pretty (declName decl) <+> "=" <+> expressionDoc opts (declValue decl)

-- | Generate a document for an effect expression
effectDoc :: PrettyOptions -> EffectExpr -> Doc ann
effectDoc opts effect = case effect of
  DepositExpr resource amount account ->
    "deposit" <+> expressionDoc opts resource <+> expressionDoc opts amount <+> expressionDoc opts account
    
  WithdrawExpr resource amount account ->
    "withdraw" <+> expressionDoc opts resource <+> expressionDoc opts amount <+> expressionDoc opts account
    
  TransferExpr resource fromAcc toAcc amount ->
    "transfer" <+> expressionDoc opts resource <+> expressionDoc opts fromAcc <+> 
               expressionDoc opts toAcc <+> expressionDoc opts amount
    
  ObserveExpr resource data_ ->
    "observe" <+> expressionDoc opts resource <+> expressionDoc opts data_
    
  EmitExpr event ->
    "emit" <+> expressionDoc opts event
    
  InvokeExpr action ->
    "invoke" <+> expressionDoc opts action

-- | Generate a document for a time expression
timeExprDoc :: PrettyOptions -> TimeExpr -> Doc ann
timeExprDoc opts timeExpr = case timeExpr of
  AfterExpr time timeUnit expr ->
    "after" <+> expressionDoc opts time <+> timeUnitDoc timeUnit <+> expressionDoc opts expr
    
  WithinExpr time timeUnit expr ->
    "within" <+> expressionDoc opts time <+> timeUnitDoc timeUnit <+> expressionDoc opts expr
    
  AtExpr time expr ->
    "at" <+> expressionDoc opts time <+> expressionDoc opts expr

-- | Generate a document for a time unit
timeUnitDoc :: TimeUnit -> Doc ann
timeUnitDoc unit = case unit of
  Seconds -> "seconds"
  Minutes -> "minutes"
  Hours -> "hours"
  Days -> "days"

-- | Empty document (used to avoid overlapping variable name with prettyprinter)
telEmptyDoc :: Doc ann
telEmptyDoc = emptyDoc 

-- | Generate a document for a guarded expression
guardedExprDoc :: PrettyOptions -> GuardedExpr -> Doc ann
guardedExprDoc opts guardedExpr = case guardedExpr of
  Guard cond expr ->
    "|" <+> expressionDoc opts cond <+> "=" <+> expressionDoc opts expr
  Where decls ->
    "where" <> line <>
    indent (indentWidth opts) (vsep (map (declarationDoc opts) decls)) 