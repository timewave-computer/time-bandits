{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TimeBandits.Core.TEL
Description : Temporal Effect Language (TEL) main module
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides the main entry point for the Temporal Effect Language (TEL),
re-exporting the necessary components from the submodules. TEL is a Haskell-like
language for describing cross-timeline effects as specified in ADR-013.

TEL unifies and replaces the earlier TECL (Temporal Effect Combinator Language) 
implementation, combining its strengths while providing a more comprehensive 
feature set.

@since 0.1.0
-}
module TimeBandits.Core.TEL
  ( -- * Abstract Syntax Tree
    module TimeBandits.Core.TEL.AST
    
    -- * Parsing
  , parseTEL
  , parseFile
  , parseExpr
  
    -- * Pretty Printing
  , prettyPrintProgram
  , prettyPrintExpression
  , PrettyOptions(..)
  , defaultOptions
  
    -- * Type Checking
  , typeCheck
  , typeCheckExpr
  , TypeError(..)
    
    -- * Concurrency
  , module TimeBandits.Core.TEL.Concurrency
    
    -- * Effect Generation
  , translateToEffects
  
    -- * Legacy compatibility (TECL -> TEL)
  , LiteralValue
  ) where

import TimeBandits.Core.TEL.AST
import TimeBandits.Core.TEL.Parser (parseTEL, parseFile, parseExpr)
import TimeBandits.Core.TEL.PrettyPrinter (prettyPrintProgram, prettyPrintExpression, PrettyOptions(..), defaultOptions)
import TimeBandits.Core.TEL.TypeChecker (typeCheck, typeCheckExpr, TypeError(..))
import TimeBandits.Core.TEL.Concurrency
import TimeBandits.Core.Effect (Effect)

-- | Alias for backwards compatibility with TECL
type LiteralValue = LiteralExpr

-- | Translate TEL code to Time Bandits effects
-- This is a placeholder function that will be implemented as part of the full TEL system
translateToEffects :: String -> [Effect]
translateToEffects _ = []  -- Placeholder implementation 