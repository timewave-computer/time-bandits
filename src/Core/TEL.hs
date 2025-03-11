{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Core.TEL
Description : Temporal Effect Language (TEL) main module
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides the main entry point for the Temporal Effect Language (TEL),
re-exporting the necessary components from the submodules. TEL is a Haskell-like
language for describing cross-timeline effects as specified in ADR-013.
-}
module Core.TEL
  ( -- * Abstract Syntax Tree
    module Core.TEL.AST
    
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
  ) where

import Core.TEL.AST
import Core.TEL.Parser (parseTEL, parseFile, parseExpr)
import Core.TEL.PrettyPrinter (prettyPrintProgram, prettyPrintExpression, PrettyOptions(..), defaultOptions)
import Core.TEL.TypeChecker (typeCheck, typeCheckExpr, TypeError(..)) 