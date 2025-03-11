{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Core.TEL.PrettyPrinter
Description : Pretty printer for the Temporal Effect Language
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides pretty printing functionality for the Temporal Effect
Language (TEL) AST, converting it back to its textual representation.
-}
module Core.TEL.PrettyPrinter 
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

import Core.TEL.AST
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
  , compact     :: Bool      -- ^ Whether to use compact formatting
  }

-- | Default pretty printer options
defaultOptions :: PrettyOptions
defaultOptions = PrettyOptions
  { indentWidth = 2
  , lineWidth   = 80
  , showTypes   = True
  , compact     = False
  }

-- | Compact options (minimal whitespace)
compactOptions :: PrettyOptions
compactOptions = defaultOptions
  { indentWidth = 1
  , lineWidth   = 100
  , showTypes   = False
  , compact     = True
  }

-- | Verbose options (maximal information and formatting)
verbose :: PrettyOptions
verbose = defaultOptions
  { indentWidth = 4
  , lineWidth   = 100
  , showTypes   = True
  , compact     = False
  }

-- | Pretty print a TEL program to Text
prettyPrint :: Pretty a => PrettyOptions -> a -> Text
prettyPrint opts doc =
  renderStrict . layoutSmart layoutOpts . pretty $ doc
  where
    layoutOpts = LayoutOptions { layoutPageWidth = AvailablePerLine (lineWidth opts) 1.0 }

-- | Pretty print a TEL program to Text
prettyPrintProgram :: PrettyOptions -> Program -> Text
prettyPrintProgram = prettyPrint

-- | Pretty print a TEL expression to Text
prettyPrintExpression :: PrettyOptions -> Expression -> Text
prettyPrintExpression = prettyPrint

-- | Pretty typeclass with access to options
class PrettyWithOptions a where
  prettyWithOptions :: PrettyOptions -> a -> Doc ann

-- | Context to provide options to pretty instances
withPrettyOptions :: PrettyWithOptions a => PrettyOptions -> a -> Doc ann
withPrettyOptions = prettyWithOptions

-- | Pretty instance for Program
instance Pretty Program where
  pretty = prettyWithOptions defaultOptions

instance PrettyWithOptions Program where
  prettyWithOptions opts Program{..} =
    vsep (map (prettyWithOptions opts) programDefinitions)

-- | Pretty instance for Definition
instance Pretty Definition where
  pretty = prettyWithOptions defaultOptions

instance PrettyWithOptions Definition where
  prettyWithOptions opts Definition{..} =
    case (showTypes opts, defSignature) of
      (True, Just sig) -> 
        prettyWithOptions opts sig <> hardline <>
        prettyWithOptions opts defFunction
      _ -> prettyWithOptions opts defFunction

-- | Pretty instance for TypeSignature
instance Pretty TypeSignature where
  pretty = prettyWithOptions defaultOptions

instance PrettyWithOptions TypeSignature where
  prettyWithOptions _ TypeSignature{..} =
    pretty typeSigName <+> "::" <+> pretty typeSigType

-- | Pretty instance for FunctionDef
instance Pretty FunctionDef where
  pretty = prettyWithOptions defaultOptions

instance PrettyWithOptions FunctionDef where
  prettyWithOptions opts (SimpleFunctionDef name params body) =
    pretty name <+>
    hsep (map pretty params) <+>
    equals <+>
    prettyWithOptions opts body
  
  prettyWithOptions opts (GuardedFunctionDef name params guards) =
    pretty name <+>
    hsep (map pretty params) <>
    nest (indentWidth opts) (hardline <> vsep (map (prettyWithOptions opts) guards))

-- | Pretty instance for Pattern
instance Pretty Pattern where
  pretty WildcardPattern = "_"
  pretty (VarPattern name) = pretty name
  pretty (LiteralPattern lit) = pretty lit
  pretty (ConstructorPattern name patterns) =
    pretty name <+> hsep (map pretty patterns)

-- | Pretty instance for GuardedExpr
instance Pretty GuardedExpr where
  pretty = prettyWithOptions defaultOptions

instance PrettyWithOptions GuardedExpr where
  prettyWithOptions _ (Guard cond expr) =
    "|" <+> pretty cond <+> equals <+> pretty expr
  
  prettyWithOptions opts (Where decls) =
    "where" <+>
    nest (indentWidth opts) (vsep (map pretty decls))

-- | Pretty instance for Declaration
instance Pretty Declaration where
  pretty Declaration{..} =
    pretty declName <+> equals <+> pretty declValue

-- | Pretty instance for TypeExpr
instance Pretty TypeExpr where
  pretty (BasicType name) = pretty name
  pretty (ListType t) = brackets (pretty t)
  pretty (TupleType ts) = parens (hsep (punctuate comma (map pretty ts)))
  
  pretty (FunctionType t1 t2) = case t1 of
    FunctionType _ _ -> parens (pretty t1) <+> "->" <+> pretty t2
    _ -> pretty t1 <+> "->" <+> pretty t2
  
  pretty (EffectType t) = "Effect" <+> pretty t
  pretty (TimelineType t) = "Timeline" <+> pretty t

-- | Pretty instance for Expression
instance Pretty Expression where
  pretty = prettyWithOptions defaultOptions

instance PrettyWithOptions Expression where
  prettyWithOptions opts expr = case expr of
    LiteralExpr lit -> pretty lit
    VariableExpr name -> pretty name
    
    ApplicationExpr func args ->
      let funcDoc = case func of
            InfixExpr _ _ _ -> parens (prettyWithOptions opts func)
            _ -> prettyWithOptions opts func
      in funcDoc <+> hsep (map argDoc args)
      where
        argDoc arg = case arg of
          LiteralExpr _ -> prettyWithOptions opts arg
          VariableExpr _ -> prettyWithOptions opts arg
          _ -> parens (prettyWithOptions opts arg)
    
    LambdaExpr patterns body ->
      "\\" <> hsep (map pretty patterns) <+> "->" <+> 
      prettyWithOptions opts body
    
    EffectExpr effect -> prettyWithOptions opts effect
    
    LetExpr decls body ->
      "let" <+>
      nest (indentWidth opts) (vsep (map pretty decls)) <+>
      "in" <+>
      prettyWithOptions opts body
    
    IfExpr cond thenExpr elseExpr ->
      "if" <+> pretty cond <+>
      "then" <+> prettyWithOptions opts thenExpr <+>
      "else" <+> prettyWithOptions opts elseExpr
    
    CaseExpr scrutinee patterns ->
      "case" <+> pretty scrutinee <+> "of" <>
      nest (indentWidth opts) 
        (hardline <> vsep [pretty pat <+> "->" <+> prettyWithOptions opts expr | (pat, expr) <- patterns])
    
    TimeExpr timeExpr -> prettyWithOptions opts timeExpr
    
    InfixExpr e1 op e2 ->
      let leftDoc = case e1 of
            InfixExpr _ _ _ -> parens (prettyWithOptions opts e1)
            _ -> prettyWithOptions opts e1
          rightDoc = case e2 of
            InfixExpr _ _ _ -> parens (prettyWithOptions opts e2)
            _ -> prettyWithOptions opts e2
      in leftDoc <+> pretty op <+> rightDoc
    
    HashRefExpr hash -> "@" <> pretty (C8.unpack (B16.encode hash))
    
    DoExpr stmts ->
      "do" <>
      nest (indentWidth opts) (hardline <> vsep (map (prettyWithOptions opts) stmts))

-- | Pretty instance for LiteralExpr
instance Pretty LiteralExpr where
  pretty (IntLiteral n) = pretty n
  pretty (DoubleLiteral d) = pretty d
  pretty (TextLiteral t) = dquotes (pretty t)
  pretty (BoolLiteral True) = "True"
  pretty (BoolLiteral False) = "False"
  pretty (ListLiteral exprs) = brackets (hsep (punctuate comma (map pretty exprs)))
  pretty (TupleLiteral exprs) = parens (hsep (punctuate comma (map pretty exprs)))

-- | Pretty instance for TimeExpr
instance Pretty TimeExpr where
  pretty = prettyWithOptions defaultOptions

instance PrettyWithOptions TimeExpr where
  prettyWithOptions opts (AfterExpr amount unit action) =
    "after" <+> prettyWithOptions opts amount <+> pretty unit <+> prettyWithOptions opts action
  
  prettyWithOptions opts (WithinExpr amount unit action) =
    "within" <+> prettyWithOptions opts amount <+> pretty unit <+> prettyWithOptions opts action
  
  prettyWithOptions opts (AtExpr time action) =
    "at" <+> prettyWithOptions opts time <+> prettyWithOptions opts action

-- | Pretty instance for TimeUnit
instance Pretty TimeUnit where
  pretty Seconds = "seconds"
  pretty Minutes = "minutes"
  pretty Hours = "hours"
  pretty Days = "days"

-- | Pretty instance for EffectExpr
instance Pretty EffectExpr where
  pretty = prettyWithOptions defaultOptions

instance PrettyWithOptions EffectExpr where
  prettyWithOptions opts (DepositExpr amount target timeline) =
    "deposit" <+> pretty amount <+> "to" <+> pretty target <+> "on" <+> pretty timeline
  
  prettyWithOptions opts (WithdrawExpr amount source timeline) =
    "withdraw" <+> pretty amount <+> "from" <+> pretty source <+> "on" <+> pretty timeline
  
  prettyWithOptions opts (TransferExpr amount source target timeline) =
    "transfer" <+> pretty amount <+> "from" <+> pretty source <+> "to" <+> pretty target <+> "on" <+> pretty timeline
  
  prettyWithOptions opts (ObserveExpr value timeline) =
    "observe" <+> pretty value <+> "on" <+> pretty timeline
  
  prettyWithOptions opts (EmitExpr value) =
    "emit" <+> pretty value
  
  prettyWithOptions opts (InvokeExpr value) =
    "invoke" <+> pretty value

-- | Pretty instance for DoStatement
instance Pretty DoStatement where
  pretty = prettyWithOptions defaultOptions

instance PrettyWithOptions DoStatement where
  prettyWithOptions opts (BindStmt name expr) =
    pretty name <+> "<-" <+> prettyWithOptions opts expr
  
  prettyWithOptions opts (LetStmt decls) =
    "let" <+> vsep (map pretty decls)
  
  prettyWithOptions opts (ExprStmt expr) =
    prettyWithOptions opts expr

-- | Pretty instance for Operator
instance Pretty Operator where
  pretty SequenceOp = ">>"
  pretty ParallelOp = "<|>"
  pretty ChoiceOp = "<|"
  pretty AddOp = "+"
  pretty SubOp = "-"
  pretty MulOp = "*"
  pretty DivOp = "/"
  pretty EqOp = "=="
  pretty NeqOp = "/="
  pretty LtOp = "<"
  pretty GtOp = ">"
  pretty LeOp = "<="
  pretty GeOp = ">="
  pretty AndOp = "&&"
  pretty OrOp = "||" 