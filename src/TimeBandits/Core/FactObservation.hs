{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TimeBandits.Core.FactObservation
Description : Main module for fact observation
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module re-exports functionality from the FactObservation submodules.
-}
module TimeBandits.Core.FactObservation 
  ( -- * Re-export Rules module
    module TimeBandits.Core.FactObservation.Rules
    
    -- * Re-export Engine module
  , EngineConfig(..)
  , RuleEngine
  , EngineError(..)
  , FactResult
  , createEngine
  , loadRules
  , evaluateData
  , evaluateDataWithRule
  , evaluateDataWithRuleSet
  , generateFact
  , generateFactWithProof
  ) where

import TimeBandits.Core.FactObservation.Rules
import TimeBandits.Core.FactObservation.Engine
  ( EngineConfig(..)
  , RuleEngine
  , EngineError(..)
  , FactResult
  , createEngine
  , loadRules
  , evaluateData
  , evaluateDataWithRule
  , evaluateDataWithRuleSet
  , generateFact
  , generateFactWithProof
  )
import TimeBandits.Core.FactObservation.Schema () 