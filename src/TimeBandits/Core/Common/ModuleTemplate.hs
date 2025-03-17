{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : TimeBandits.Core.Common.ModuleTemplate
Description : Template for new modules in the Time Bandits codebase
Copyright   : (c) TimeBandits Authors, 2024
License     : MIT
Maintainer  : TimeBandits Team

This module serves as a template for creating new modules in the Time Bandits codebase.
It defines a standard structure and import style that should be followed.

Usage:
1. Copy this file to your new module location
2. Replace the module name, description, and other header info
3. Follow the import group ordering
4. Implement your module functionality
-}

module TimeBandits.Core.Common.ModuleTemplate 
  ( -- * Template Functions
    exampleFunction
  , TemplateType(..)
    -- * Re-exports
  , module TimeBandits.Core.Common.Types
  ) where

-- Import documentation of standard extensions
import TimeBandits.Core.Common.Extensions


-- External libraries (alphabetized)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Generics (Generic)

-- TimeBandits common modules
import TimeBandits.Core.Common.Types
import TimeBandits.Core.Common.Utils (camelToSnake)

-- TimeBandits domain modules (as needed)
-- import TimeBandits.Core.ContentAddress
-- import TimeBandits.Core.Resource

-- | An example type to demonstrate the structure
data TemplateType = TemplateType
  { field1 :: !Text
  , field2 :: !Int
  , field3 :: !(Map Text Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | An example function to demonstrate documentation style
exampleFunction 
  :: Text       -- ^ Input text to process
  -> Int        -- ^ Maximum number of operations
  -> Maybe Text -- ^ Processed result, if successful
exampleFunction input maxOps = 
  if T.length input > maxOps
    then Nothing
    else Just $ T.toUpper input

{- $usage
Example usage of this module:

@
import TimeBandits.Core.Common.ModuleTemplate (exampleFunction)

main :: IO ()
main = do
  let result = exampleFunction "test" 10
  print result  -- Just "TEST"
@
-}

-- | Internal helper function that isn't exported
internalHelper :: Text -> Text
internalHelper = T.reverse 
