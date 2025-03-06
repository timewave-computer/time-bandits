-- | Core module that re-exports all Core submodules
module Core 
  ( -- * Core Types and Functionality
    -- | Re-export Core.Core module which provides main functionality
    module Core.Core
    
    -- | We explicitly don't re-export other modules to avoid conflicts
    -- Instead, users should import specific modules as needed
  ) where

-- Import the main Core.Core module which contains essential functionality
import Core.Core

-- Note: We're no longer re-exporting all submodules to avoid naming conflicts
-- Users should import specific modules as needed, e.g.:
-- import Core.Types
-- import Core.Effects
-- etc.
