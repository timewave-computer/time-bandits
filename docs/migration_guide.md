# Time Bandits Migration Guide

Version: 1.0
Date: 2025-03-07

## Overview

This migration guide is intended for users who have built applications on previous versions of the Time Bandits system. It explains the major architectural changes in the latest version and provides step-by-step guidance for migrating your code to use the new abstractions.

## Key Architectural Changes

The Time Bandits system has been refactored with several key improvements:

1. **Clear Type Separation**: Core types (Timeline, Resource, Program, Effect) are now first-class entities with dedicated modules.
2. **Explicit Effect Execution**: All state changes now occur via explicit effect execution, rather than implicit state updates.
3. **Program Invocation Model**: Programs now use a formal invocation model with explicit resource ownership and transfer.
4. **Causal Ordering**: Time Maps and Transition Messages ensure proper causal ordering across timelines.
5. **Actor-Based Design**: Three distinct actor roles (Time Travelers, Time Keepers, Time Bandits) with clear responsibilities.
6. **Multi-Mode Simulation**: Support for in-memory, local multi-process, and geo-distributed deployment modes.
7. **Enhanced Security**: Formal verification of key security properties like double-spend prevention and reentrancy protection.
8. **ZK Proof Integration**: Zero-knowledge proofs for validating transitions and resource ownership.

## Migration Steps

### Step 1: Update Imports

Replace imports from deprecated modules with their new counterparts:

```haskell
-- Old imports
import TimeBandits.Core
import TimeBandits.Effects

-- New imports
import TimeBandits.Timeline
import TimeBandits.Resource
import TimeBandits.Program
import TimeBandits.Effect
import TimeBandits.EffectExecutor
```

### Step 2: Replace Direct State Modifications

Replace direct state modifications with explicit effect execution:

```haskell
-- Old approach
updateTimelineState timeline newEvents = 
  timeline { events = events timeline ++ newEvents }

-- New approach
import TimeBandits.EffectExecutor (applyEffect)
import TimeBandits.Effect (Effect(..))

updateTimelineState timeline newEvents = do
  let effect = AppendEvents newEvents
  applyEffect timeline effect
```

### Step 3: Update Resource Handling

Use the new resource ownership model:

```haskell
-- Old approach
transferToken from to amount = modifyBalances from to amount

-- New approach
import TimeBandits.Resource (escrowResource, claimResource)

transferToken from to amount = do
  resource <- getResource amount from
  escrowedResource <- escrowResource resource to
  claimResource escrowedResource to
```

### Step 4: Implement Transition Messages

Replace direct function calls with transition messages:

```haskell
-- Old approach
callProgram programId functionName args = 
  executeProgram programId functionName args

-- New approach
import TimeBandits.TransitionMessage (createTransitionMessage)
import TimeBandits.Controller (submitTransition)

callProgram programId functionName args = do
  message <- createTransitionMessage programId functionName args
  submitTransition message
```

### Step 5: Update Timeline Interactions

Use the new Timeline and TimeMap abstractions:

```haskell
-- Old approach
getBlockHeight timelineId = lookupTimeline timelineId >>= getHeight

-- New approach
import TimeBandits.TimeMap (getTimelineHead)

getBlockHeight timelineId timeMap = getTimelineHead timelineId timeMap
```

### Step 6: Adapt to Actor Roles

Structure your code around the actor role abstractions:

```haskell
-- For client applications (Time Travelers)
import TimeBandits.TimeTraveler (deployProgram, submitTransition)

-- For validators (Time Keepers)
import TimeBandits.TimeKeeper (validateMessage, applyToTimeline)

-- For network nodes (Time Bandits)
import TimeBandits.TimeBandit (executeProgram, propagateMessage)
```

## Testing Your Migration

1. Run unit tests with the new abstractions
2. Verify that all security properties are maintained
3. Test in all deployment modes (in-memory, local, distributed)

## Common Issues and Solutions

### Issue: Compatibility with Old Code

**Solution**: Use the adapter functions in the new modules which provide backward compatibility.

### Issue: Managing Program State

**Solution**: Use the new `ProgramMemory` abstraction for storing program state.

### Issue: Cross-Timeline Operations

**Solution**: Use TimeMap to maintain a consistent view across timelines.

## Getting Help

If you encounter issues during migration, please:

1. Check the API documentation for the new modules
2. Review the example applications in the `/examples` directory
3. Open an issue on the GitHub repository with details about your problem

## Timeline for Deprecated Code Removal

The deprecated modules and functions will be fully removed in version 2.0, scheduled for release in Q3 2025. Please complete your migration before then to ensure compatibility with future releases. 