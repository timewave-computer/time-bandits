{- |
Module      : TimeBandits.Core.Common.ModuleLayout
Description : Documentation about the Core.Common module structure
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides documentation about how the Core.Common module is structured
and how it should be used by other modules in the codebase.
-}
module TimeBandits.Core.Common.ModuleLayout where

{- $structure
The Core.Common module is structured as follows:

1. Types.hs - Core types used throughout the system
2. Serialize.hs - Centralized serialization functionality
3. Utils.hs - General purpose utilities

To use these modules, import them explicitly:

@
-- Importing types
import TimeBandits.Core.Common.Types (Hash(..), EntityHash(..), Actor(..))

-- Importing serialization
import TimeBandits.Core.Common.Serialize (Serialize, encode, decode)

-- Importing utilities
import TimeBandits.Core.Common.Utils (camelToSnake, fileExists)
@

Avoid importing the entire module with wildcard imports (@import TimeBandits.Core.Common.Types@).
Instead, explicitly state which types and functions you need.

For qualified imports, use a short alias:

@
import qualified TimeBandits.Core.Common.Utils as CommonUtils
@
-}

{- $dependencies
The Common module has minimal dependencies and should not depend on other Time Bandits modules.
This ensures that it can be used throughout the codebase without creating circular dependencies.

The dependency hierarchy is:

@
Core.Common -> Core.ContentAddress -> Core.Resource, Core.TimeMap ->
Core.Effect, Core.Execution -> Language -> Timeline -> Network -> Tools
@

The Common module should only depend on external libraries and not on other Time Bandits modules.
-}

{- $usage
When implementing new functionality, consider:

1. Is it truly general-purpose? If yes, add it to Utils.hs.
2. Is it a fundamental type used across the codebase? If yes, add it to Types.hs.
3. Is it related to serialization? If yes, add it to Serialize.hs.
4. Otherwise, it probably belongs in a more specific module.

This helps maintain a clean separation of concerns and prevents the Common module from 
becoming a dumping ground for miscellaneous functionality.
-} 