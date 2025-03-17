{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TimeBandits.Core.TimeMap
Description : Timeline tracking and management for the Time Bandits system
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides the main entry point for the TimeMap functionality,
re-exporting the types and operations from submodules. The TimeMap is
responsible for tracking and managing timelines and their relationships.
-}
module TimeBandits.Core.TimeMap
  ( -- * TimeMap Types
    module TimeBandits.Core.TimeMap.Types
    
  -- * TimeMap Operations
  , module TimeBandits.Core.TimeMap.Operations
  ) where

import TimeBandits.Core.TimeMap.Types
import TimeBandits.Core.TimeMap.Operations 