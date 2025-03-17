{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TimeBandits.Core.Resource
Description : Resource management for the Time Bandits system
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides the main entry point for the resource management
functionality, re-exporting the types and operations from submodules.
-}
module TimeBandits.Core.Resource
  ( -- * Resource Types
    module TimeBandits.Core.Resource.Types
    
  -- * Resource Operations
  , module TimeBandits.Core.Resource.Operations
  
  -- * Ledger Operations
  , module TimeBandits.Core.Resource.Ledger
  ) where

import TimeBandits.Core.Resource.Types
import TimeBandits.Core.Resource.Operations
import TimeBandits.Core.Resource.Ledger 