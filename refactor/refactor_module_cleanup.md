# Time Bandits Module Cleanup and Refactoring Plan

Status: In-progress

## Overview

This document outlines a step-by-step plan to address the current issues with the Time Bandits codebase after the recent reorganization. The primary goal is to resolve namespace conflicts, module imports, type ambiguities, and duplicated definitions to enable successful compilation and test execution.

## Current Issues Identified

1. **Module namespace inconsistencies**
   - Some modules are defined with `TimeBandits.Core.*` namespace but imported as `Core.*`
   - Conflicting import patterns across the codebase

2. **Type ambiguities**
   - Multiple definitions or imports of the same types (e.g., `Hash`, `Signature`, `Actor`, etc.)
   - Conflicts between `Core.Common` and `Core.Types` definitions

3. **Code duplication**
   - Multiple declarations of functions like `effectId` and `effectPreconditions`

4. **Type mismatches**
   - `EntityHash` vs. `ResourceId` usage inconsistencies
   - Various incompatible type usages across modules

5. **Nix build configuration**
   - Nix build pointing to individual component directories that no longer exist

## Refactoring Plan

### Phase 1: Establish Clear Module Namespace Convention

1. **Decide on consistent module naming**
   - Choose between `TimeBandits.Core.*` or `Core.*` namespacing
   - Use `Core.*` as it is the dominant pattern

2. **Update module declarations**
   - Ensure all module declarations follow the chosen pattern
   - Update imports to match the module declarations

### Phase 2: Resolve Core Type Definitions

1. **Consolidate common types**
   - Move all core types to appropriate modules
   - Focus on eliminating duplication between `Core.Common` and `Core.Types`, moving things or using `Core.Common` where possible

2. **Create type hierarchy**
   - Create a clear type hierarchy to avoid circular dependencies
   - Ensure that foundational types are defined in modules imported by others

3. **Fix specific modules with duplicated types**:
   1. **Hash and EntityHash**
      - Choose one location (recommend `Core.Common`)
      - Update all imports and re-export as needed

   2. **Signature**
      - Define in `Core.Common` only
      - Update all imports and re-export as needed

   3. **Actor and related types**
      - Define in `Core.Types` only
      - Update all imports and re-export as needed

### Phase 3: Fix Import Structures

1. **Qualified imports**
   - Use qualified imports for modules with overlapping type names
   - Example: `import qualified Core.Common as Common`

2. **Explicit imports**
   - Use explicit imports to avoid ambiguity
   - Example: `import Core.Common (Hash(..), PubKey(..))`

3. **Update Core/Utils.hs imports**
   - Continue the pattern established in the fix for this file
   - Use qualified imports and explicit imports

4. **Fix import ambiguities in**:
   - `Core/Effect.hs`
   - `Core/Timeline.hs`
   - `Core/TimelineDescriptor.hs`
   - `Core/Resource.hs`
   - `Core/Effects.hs`

### Phase 4: Fix Duplicate Function Definitions

1. **Core/Effect.hs duplicates**
   - Remove duplicate definitions of `effectId` and `effectPreconditions`
   - Ensure functions are defined only once

2. **Resolve any other duplicate functions**
   - Check for and fix any other duplicate definitions throughout the codebase

### Phase 5: Fix Type Mismatches

1. **EntityHash vs ResourceId**
   - Fix type inconsistencies in `Core/Resource.hs`
   - Ensure consistent type usage across function signatures

2. **TimelineHash conflicts**
   - Resolve ambiguous references in `Core/Timeline.hs`
   - Use qualified imports where needed

3. **LamportTime usage**
   - Fix illegal term-level uses in `Core/Resource.hs`
   - Ensure proper type application

4. **ActorHash and similar types**
   - Fix inconsistent usage patterns in `Core/Effects.hs`

### Phase 6: Update Nix Configuration

1. **Update flake.nix**
   - Point to the correct source directories
   - Build from the consolidated structure

2. **Update cabal configuration**
   - Ensure cabal file correctly lists all modules
   - Remove any references to standalone `Types` module that was removed

### Phase 7: Update Tests

1. **Fix test imports**
   - Update test imports to match the new module structure
   - Resolve any ambiguities in test code

2. **Create minimal passing test**
   - Get a simple subset of tests running first
   - Expand to full test suite as refactoring progresses

### Phase 8: Implementation Steps

Below is a detailed checklist of tasks to complete. Use this to track progress:

#### Module Namespace Standardization

- [x] ✅ Audit all module declarations in `/src/Core/`
- [x] ✅ Standardize on `Core.*` namespace
- [x] ✅ Update `Execution.ResourceLedger` module declaration to match its file path

#### Core Type Hierarchy Fix

- [ ] Ensure `Core.Common` contains base cryptographic types (Hash, Signature, etc.)
- [ ] Migrate everything from `Core.Types` into `Core.Common` and update all imports that now need to point to `Core.Common`
- [ ] Move all actor-related types to `Core.Common`
- [ ] Move all timeline-related types to appropriate modules
- [ ] Ensure resource types are consistent

#### Import Structure Updates

- [x] ✅ **Core/Utils.hs**: Consolidate hash functionality into this module
- [x] ✅ **Core/Effect.hs**: Fix duplicate definitions by renaming functions
- [x] ✅ **Core/Timeline.hs**: Fix ambiguous TimelineHash references
- [x] ✅ **Core/TimelineDescriptor.hs**: Fix Serialize instances by adding manual implementations
- [x] ✅ **Core/Resource.hs**: Fix ResourceId type inconsistencies
- [x] ✅ **Core/Effects.hs**: Fix ambiguous imports and duplicated types
- [x] ✅ **Core/TimeMap.hs**: Fix timelineHash references to use timelineId
- [ ] Review all other files for similar patterns

#### Fix Serialization Issues

- [x] ✅ Fix deriving strategies in Core/Effect.hs to use explicit `deriving stock` and add manual Serialize instances
- [x] ✅ Fix deriving strategies in Core/TimelineDescriptor.hs
- [x] ✅ Fix deriving strategies in Core/TimeMap.hs
- [x] ✅ Fix deriving strategies in Core/ResourceLedger.hs
- [x] ✅ Fix deriving strategies in Core/Effects.hs
- [ ] Review other files for deriving strategy issues

#### Fix Core.hs Module Exports

- [x] ✅ Fix Core.hs to only re-export Core.Core module, avoiding namespace conflicts

#### Fix Type Mismatches

- [x] ✅ Fix Core.Core.hs to use correct Hash types from Core.Types
- [ ] Fix all EntityHash/ResourceId mismatches
- [x] ✅ Fix all TimelineHash conflicts
- [ ] Fix all Signature type ambiguities
- [ ] Review other type mismatches

#### Build System Updates

- [x] ✅ Successfully build with existing Nix configuration
- [ ] Update flake.nix if needed
- [ ] Fix any build issues

#### Testing

- [ ] Update test imports
- [ ] Fix test ambiguities
- [ ] Create minimal passing test
- [ ] Expand test coverage

## Execution Strategy

To implement this plan effectively:

1. **Focus on one phase at a time**
   - Complete each phase before moving to the next
   - Document changes as they are made

2. **Use linters and compiler warnings**
   - Use GHC warnings to identify issues
   - Address warnings systematically

3. **Regular build testing**
   - Test builds frequently
   - Address new issues as they arise

4. **Maintain test coverage**
   - Update tests as code changes
   - Ensure tests remain valid

## Expected Outcomes

By following this refactoring plan, we aim to:

1. Eliminate all namespace and import ambiguities
2. Resolve type conflicts and duplications
3. Establish a clear, consistent module hierarchy
4. Enable successful builds via Nix
5. Maintain or improve test coverage 