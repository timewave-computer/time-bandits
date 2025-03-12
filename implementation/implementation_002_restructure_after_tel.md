# Implementation 001: Time Bandits Codebase Reorganization Plan

## Current Issues

From what I can see:

1. The modular boundaries are fuzzy - core functionality is scattered across directories
2. There's an inconsistent naming convention (sometimes `camelCase`, sometimes `snake_case`)
3. Import statements are a bit chaotic - lots of wildcard imports
4. The content-addressable components are particularly tangled
5. Test structure doesn't mirror source structure cleanly

## Proposed Directory Structure

Here's what I'm thinking:

```
src/
├── TimeBandits/
│   ├── Core/
│   │   ├── Common/          # Fundamental types and utilities
│   │   ├── ContentAddress/  # Content-addressed code storage
│   │   ├── Execution/       # Execution environment
│   │   ├── Resource/        # Resource management
│   │   ├── TimeMap/         # Time mapping functionality
│   │   └── Effect/          # Effect system
│   ├── Language/
│   │   ├── TEL/             # Temporal Effect Language
│   │   ├── TECL/            # Temporal Effect Combinator Language
│   │   └── Interpreter/     # Language interpreters
│   ├── Timeline/
│   │   ├── Adapter/         # Timeline-specific adapters
│   │   ├── Keeper/          # Time keeper implementation
│   │   └── Fact/            # Fact observation and management
│   ├── Network/
│   │   ├── P2P/             # Peer-to-peer networking
│   │   ├── RPC/             # Remote procedure calls
│   │   └── Protocol/        # Protocol definitions
│   ├── Simulation/
│   │   ├── Controller/      # Simulation control
│   │   ├── Scenario/        # Predefined scenarios
│   │   └── Observer/        # Observation mechanisms
│   └── Tools/
│       ├── CLI/             # Command-line interface
│       ├── Debug/           # Debugging utilities
│       └── Profiling/       # Performance profiling
test/
├── TimeBandits/
│   ├── Core/                # Mirror of src structure
│   ├── Language/
│   ├── Timeline/
│   ├── Network/
│   ├── Simulation/
│   └── Tools/
```

## Implementation Strategy

This is too big for a single refactoring. Let's break it into phases:

### Phase 1: Core Infrastructure (3 weeks)

Start with the foundation:

1. **Create the new directory structure** (1 day)
   ```bash
   # Create all directories
   mkdir -p src/TimeBandits/{Core/{Common,ContentAddress,Execution,Resource,TimeMap,Effect},Language/{TEL,TECL,Interpreter},Timeline/{Adapter,Keeper,Fact},Network/{P2P,RPC,Protocol},Simulation/{Controller,Scenario,Observer},Tools/{CLI,Debug,Profiling}}
   
   # Mirror in test directory
   mkdir -p test/TimeBandits/{Core,Language,Timeline,Network,Simulation,Tools}
   ```

2. **Establish Common Module** (3 days)
   - Move fundamental types and utilities to `Core/Common`
   - Update imports in existing files
   - Focus on fixing one module completely before moving on

3. **Move & Refactor Content-Addressable Code** (1 week)
   - This is one of the most entangled areas
   - Start with core definitions, then dependencies
   - Update imports in all dependent modules
   - Run tests after each component move

4. **Reorganize Resource Management** (1 week)
   - Implement the type class design from our ADR
   - Move existing code while updating to the new patterns
   - Update all consumers of the resource API

### Phase 2: Language & Effect System (2 weeks)

1. **Separate TEL & TECL Implementations** (1 week)
   - Move language definitions to their own modules
   - Clean up circular dependencies
   - Ensure interpreters remain compatible

2. **Refactor Effect System** (1 week)
   - Consolidate effect types and handlers
   - Clean up the effect application pipeline
   - Update tests to use the new structure

### Phase 3: Timeline & Network (2 weeks)

1. **Reorganize Timeline Components** (1 week)
   - Separate adapters by timeline
   - Consolidate keeper implementations
   - Update fact management system

2. **Clean Up Network Components** (1 week)
   - Separate P2P from RPC concerns
   - Clarify protocol definitions
   - Update discovery mechanisms

### Phase 4: Testing & Tool Alignment (2 weeks)

1. **Align Test Structure with Source** (1 week)
   - Reorganize tests to mirror source structure
   - Ensure test coverage remains high
   - Fix any broken imports

2. **Update Tools and CLI** (1 week)
   - Refactor CLI to use the new structure
   - Update debugging and profiling tools
   - Ensure backward compatibility

## Practical Implementation Advice

Here's how I'd approach this if I were doing it myself:

1. **Use a branch-per-module strategy**
   ```bash
   # For each module
   git checkout -b refactor-core-common
   # Make changes, test, commit
   git checkout main
   git merge refactor-core-common
   ```

2. **Establish clear modular boundaries before moving code**
   - Define the interfaces between modules
   - Document dependencies explicitly
   - Only then start moving implementation files

3. **Move in dependency order**
   ```
   Common → ContentAddress → Resource → Execution → Language → Timeline
   ```

4. **Handle imports systematically**
   - Start by making all imports explicit (no wildcards)
   - Use qualified imports where appropriate
   - Fix one file completely before moving to the next

5. **Write a small script to check build status**
   ```bash
   #!/bin/bash
   # Run after each significant change
   cabal build && cabal test
   ```

6. **Use IDE refactoring tools when possible**
   - Most modern IDEs can handle renames and moves
   - But verify all changes manually

## The Trickiest Parts

Based on my examination, these areas will require special attention:

1. **ContentAddress and Execution coupling**
   - There are circular dependencies here
   - Break them by creating a clean interface layer
   - Consider using type classes to abstract relationships

2. **Effect system dependencies**
   - Effect handlers reference many parts of the system
   - Consider using a capability pattern to reduce coupling
   - Move effect definitions and handlers to separate modules

3. **Timeline adapter initialization**
   - Currently very tangled with configuration
   - Extract a clean factory pattern
   - Use dependency injection to simplify

4. **Test fixtures and utilities**
   - Many tests have duplicate setup code
   - Extract common test utilities first
   - Then reorganize the tests themselves

## Code-Level Reorganization Example

Let's look at a concrete example. The `Core/CodeAddress.hs` file should be split like this:

```haskell
-- Core/ContentAddress/Types.hs
module TimeBandits.Core.ContentAddress.Types where

-- Core types moved from original file
data CodeHash = CodeHash { unHash :: ByteString }
  deriving (Eq, Ord, Show)

data DefType = FunctionDef | ModuleDef
  deriving (Eq, Show)

data CodeDefinition = CodeDefinition
  { cdHash :: CodeHash
  , cdSource :: Text
  , cdType :: DefType
  }
  deriving (Eq, Show)

-- Core/ContentAddress/Repository.hs
module TimeBandits.Core.ContentAddress.Repository where

import TimeBandits.Core.ContentAddress.Types

-- Repository operations moved from original file
class CodeRepository r where
  storeDefinition :: r -> CodeDefinition -> IO (Either RepositoryError CodeHash)
  lookupByHash :: r -> CodeHash -> IO (Maybe CodeDefinition)
  -- etc.

-- Core/ContentAddress/Hash.hs
module TimeBandits.Core.ContentAddress.Hash where

import TimeBandits.Core.ContentAddress.Types

-- Hashing functions moved from original file
hashFunction :: Text -> Text -> CodeHash
hashFunction name body = -- implementation

hashModule :: Text -> [CodeHash] -> Text -> CodeHash
hashModule name deps body = -- implementation
```

## Import Management Strategy

Imports are going to be the biggest pain point. Here's my recommendation:

1. **Start by making all imports explicit**
   ```haskell
   -- BEFORE
   import Data.Map
   
   -- AFTER
   import Data.Map (Map, empty, insert, lookup)
   ```

2. **Use qualified imports for clarity**
   ```haskell
   -- BEFORE
   import Data.Map
   
   -- AFTER
   import qualified Data.Map as Map
   ```

3. **Create import groups with standard ordering**
   ```haskell
   -- Standard library imports
   import Control.Monad (when, unless)
   import Data.Maybe (fromMaybe)
   
   -- Third-party imports
   import qualified Data.Text as T
   
   -- Project imports
   import TimeBandits.Core.Common.Types
   ```

4. **Use a script to help manage imports**
   ```bash
   # Find all imports of a module
   grep -r "import.*OldModule" src/
   
   # Then update them systematically
   ```

## Final Thoughts

This is a substantial refactoring effort, but the investment will pay dividends in maintainability. The key is to move methodically, module by module, with thorough testing at each step.

I've seen projects get bogged down in dependency hell during refactorings like this. The branch-per-module approach helps contain the chaos, letting you solve one problem at a time rather than drowning in a sea of broken imports.

One last tip that's saved me more than once: keep a "cheat sheet" document during the refactor that maps old module paths to new ones. It makes updating imports much less cognitively taxing.

Would you like me to elaborate on any particular aspect of this plan? I'm especially concerned about the circular dependencies I spotted in the content-addressable execution system - we might need a more specialized approach there.