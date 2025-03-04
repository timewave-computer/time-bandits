{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: TimeBandits.Core.Effect
Description: Defines the core effect system that enables time-travel operations

This module defines the fundamental Effect type and related functions that form the
foundation of the Time-Bandits system. Effects represent atomic, verifiable operations
that can be performed on timelines.

Effects have several key properties:
- They are composable, allowing programs to be built from smaller effect primitives
- They are deterministic, ensuring consistent behavior when replayed
- They can be cryptographically verified for security
- They provide an abstraction over different timeline implementations

The Effect type is used throughout the system as the primary building block for
programs that operate across timelines.
-}
module TimeBandits.Core.Effect 
  ( -- * Core Effect Types
    Effect(..)
  , EffectResult
  , EffectId
  , EffectApplied(..)
  
  -- * Effect Creation
  , createEffect
  , effectId
  
  -- * Effect Validation
  , validateEffect
  , effectPreconditions
  , effectPostconditions
  
  -- * Effect Serialization
  , serializeEffect
  , deserializeEffect
  ) where

-- Implementation follows... 