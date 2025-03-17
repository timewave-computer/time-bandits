{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : TimeBandits.Core.Common.Extensions
Description : Standard language extensions for the TimeBandits codebase
Copyright   : (c) TimeBandits Authors, 2024
License     : MIT
Maintainer  : TimeBandits Team

This module documents the standard language extensions used throughout the TimeBandits codebase.
Instead of using Template Haskell to enable extensions, we recommend adding these extensions
directly to your module's LANGUAGE pragmas as needed.

Recommended extensions by category:

Basic extensions:
- OverloadedStrings     -- String literals as Text/ByteString
- ScopedTypeVariables   -- Explicit forall in patterns
- LambdaCase            -- \case syntax
- BlockArguments        -- do notation without $
- RankNTypes            -- Higher-rank types
- ConstraintKinds       -- Constraints as first-class

Deriving extensions:
- DeriveGeneric           -- Enable deriving Generic
- DeriveAnyClass          -- Enable deriving for any class
- DerivingStrategies      -- Control deriving strategy
- GeneralizedNewtypeDeriving -- newtype deriving
- StandaloneDeriving      -- Deriving outside data declaration

Type system extensions:
- DataKinds            -- Type-level data
- TypeFamilies         -- Type families
- TypeOperators        -- Type operators
- TypeApplications     -- Type application
- FlexibleContexts     -- Flexible contexts
- FlexibleInstances    -- Flexible instances 
- MultiParamTypeClasses -- Multiple parameters
- GADTs                -- Generalized algebraic data types
- PolyKinds            -- Polymorphic kinds
- FunctionalDependencies -- Functional dependencies

Syntax extensions:
- ViewPatterns         -- View patterns
- PatternSynonyms      -- Pattern synonyms
- NamedFieldPuns       -- Record field puns
- RecordWildCards      -- Record wildcards
-}

module TimeBandits.Core.Common.Extensions
  ( -- * This module only provides documentation
    -- | See the module documentation for recommended extensions
  ) where

-- This module intentionally does not export any functions or values
-- It serves as documentation for recommended language extensions 