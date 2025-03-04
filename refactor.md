# Time Bandits Refactor Plan

Including:

- Program-based execution with explicit resource ownership
- Causal ordering via Lamport clocks and time maps
- Invocation model with safe resource passing
- Multi-mode simulation (in-memory, local, geo-distributed)
- Nix flakes for per-actor builds & simulation controller

## Phase 1: Core Type Abstractions & Separation of Concerns

**Goal**: Establish clear Timeline, Resource, Program, Effect separation.

### Step 1.1 Introduce Timeline, Resource, Program, and Effect as First-Class Types

- Move timeline interactions into `Timeline.hs`
- Define resources as first-class entities in `Resource.hs`
- Define program state and memory in `Program.hs`
- Define effects as explicit operations in `Effect.hs`

Example Type Structure:

```haskell
data Timeline = Timeline 
    { timelineId :: TimelineId 
    , eventLog :: [Event] 
    , clock :: TimelineClock 
    }

data Resource 
    = TokenBalanceResource TokenId Address Amount 
    | EscrowReceiptResource EscrowId 
    | ContractWitnessResource ContractId Value 
    | SyntheticInternalMarker Text

data Program = Program 
    { programId :: ProgramId 
    , executionState :: ProgramState 
    , memory :: ProgramMemory 
    }

data Effect 
    = EscrowToProgram Resource ProgramId MemorySlot 
    | InvokeProgram ProgramId FunctionName [Resource] 
    | ClaimFromProgram ProgramId MemorySlot Resource 
    | DelegateCapability Capability ProgramId Expiry 
    | WatchResource ResourceKey Condition Trigger 
    | AtomicBatch [Effect]
```

### Step 1.2 Replace Implicit State Updates with Explicit Effect Execution

- All timeline changes should now happen via Effect application.
- No more direct state modification—everything must go through an effect handler.
- Implement initial effect execution pipeline in `EffectExecutor.hs`:

```haskell
applyEffect :: ProgramState -> Effect -> Either ExecutionError ProgramState
applyEffect state (EscrowToProgram res pid slot) = modifyMemory pid slot (Just res) state
applyEffect state (InvokeProgram pid fn args) = executeProgram pid fn args state
applyEffect state (ClaimFromProgram pid slot res) = claimResource pid slot res state
```

### Step 1.3 Cleanup After Phase 1

After implementing the new modules, clean up existing code to avoid duplication:

**Resource Module Cleanup**:
- Identify `ResourceOps` effect in `TimeBandits.Effects` and related operations
- Mark as deprecated with comments directing to `TimeBandits.Resource`
- Create adapter functions in `Resource.hs` for backward compatibility

**Program Module Cleanup**:
- Identify program-related types and functions in `TimeBandits.Core` and `TimeBandits.Effects`
- Mark as deprecated with comments directing to `TimeBandits.Program` and `TimeBandits.ProgramEffect`
- Create adapter functions in the new modules for backward compatibility

**Timeline Module Cleanup**:
- Identify timeline-related types and functions in `TimeBandits.Core` and `TimeBandits.Effects`
- Mark as deprecated with comments directing to `TimeBandits.Timeline`
- Create adapter functions in `Timeline.hs` for backward compatibility

**General Approach**:
- Do not remove any code yet, only mark as deprecated
- Update imports gradually as other modules are refactored
- Ensure tests pass with the new implementations

## Phase 2: Implement Program Invocation & Resource Ownership Model

**Goal**: Enable programs to own and transfer resources explicitly.

### Step 2.1 Implement EscrowToProgram & ClaimFromProgram

- Introduce escrow memory slots inside `ProgramMemory.hs`.
- Programs lose ownership of resources when escrowing them.
- Only the target program can claim escrowed resources.

Example in `ProgramMemory.hs`:

```haskell
data MemorySlot = MemorySlot Text

data ProgramMemory = ProgramMemory 
    { slots :: Map MemorySlot Resource }

escrowToProgram :: ProgramMemory -> MemorySlot -> Resource -> ProgramMemory
escrowToProgram mem slot res = mem { slots = insert slot res (slots mem) }

claimFromProgram :: ProgramMemory -> MemorySlot -> Maybe (Resource, ProgramMemory)
claimFromProgram mem slot = do
    res <- lookup slot (slots mem)
    let newMem = mem { slots = delete slot (slots mem) }
    return (res, newMem)
```

### Step 2.2 Implement InvokeProgram for Cross-Program Calls

- Implement function call execution between programs.
- Programs should declare entry points and allow safe invocation.

Example in `InvokeProgram.hs`:

```haskell
invokeProgram :: ProgramId -> FunctionName -> [Resource] -> Either ExecutionError ProgramState
invokeProgram pid fn args = do
    program <- lookupProgram pid
    case lookupFunction fn program of
        Just f -> f args
        Nothing -> Left FunctionNotFound
```

### Step 2.3 Cleanup After Phase 2

After implementing the program invocation and resource ownership model:

**Program Module Finalization**:
- Update any remaining code using the old program invocation patterns
- Convert direct program state updates to use explicit effect execution
- Complete adapter functions in `Program.hs` and `ProgramEffect.hs`

**Resource Ownership Cleanup**:
- Identify and update code that manipulates resources directly
- Ensure all resource transfers go through the escrow mechanism
- Update tests to use the new escrow/claim pattern

## Phase 3: Implement Time Maps & Lamport Clocks

**Goal**: Ensure programs execute in causal order across timelines.

### Step 3.1 Introduce LamportClock.hs

- Track logical time per timeline and per program.
- Enforce strict happens-before ordering for all invocations.

```haskell
data LamportClock = LamportClock { counter :: Int }

incrementLamport :: LamportClock -> LamportClock
incrementLamport (LamportClock c) = LamportClock (c + 1)
```

### Step 3.2 Implement TimeMap.hs to Track Cross-Timeline State

- Maintain a snapshot of multiple timelines.
- Programs execute against a consistent time map.

```haskell
data TimeMap = TimeMap 
    { timelines :: Map TimelineId LamportClock
    , observedHeads :: Map TimelineId BlockHeader 
    }
```

### Step 3.3 Integrate Time Map Checks in EffectExecutor.hs

- Before executing an effect, verify that time constraints hold.

```haskell
applyEffect state (InvokeProgram pid fn args) = do
    ensureCausalOrder state.pid fn args
    executeProgram pid fn args state
```

### Step 3.4 Cleanup After Phase 3

After implementing time maps and Lamport clocks:

**Timeline Consistency Cleanup**:
- Migrate existing timeline logic to use the new Lamport clock implementation
- Update any code that relies on timeline-specific time ordering
- Ensure all cross-timeline operations respect causal ordering

**TimeMap Integration**:
- Update effect executor to use TimeMap for all executions
- Migrate any code that directly accesses timeline state
- Update tests to verify causal ordering guarantees

## Phase 4: Implement Multi-Mode Simulation

**Goal**: Support in-memory, local multi-process, and geo-distributed execution.

### Step 4.1 Implement Controller (Controller.hs)

- Decides execution mode (InMemory | LocalProcesses | GeoDistributed).
- Starts actors in the correct way (function call, process spawn, SSH).

```haskell
data SimulationMode = InMemory | LocalProcesses | GeoDistributed

runSimulation :: SimulationMode -> Scenario -> IO ()
runSimulation mode scenario = case mode of
    InMemory -> runInMemory scenario
    LocalProcesses -> runLocally scenario
    GeoDistributed -> runGeoDistributed scenario
```

### Step 4.2 Implement Actor Abstraction

- All actors implement the same interface, whether in-memory or spawned as a process.

```haskell
class Actor a where
    runActor :: a -> IO ()
    actorId :: a -> ActorID
```

### Step 4.3 Implement Scenario.hs for Defining Simulations

- Scenarios define actors, their roles, and execution mode (TOML-based).

Example scenario:

```TOML
mode = "LocalProcesses"
actors = [
  { id = "trader1", type = "Trader" },
  { id = "pricefeed1", type = "PriceFeed" }
]
```

### Step 4.4 Implement Nix Flakes for Actors & Controller

- Each actor is a standalone flake.
- The controller aggregates and launches them.

```nix
{
  description = "Time Bandits Simulation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    trader.url = "./simulation/actors/trader";
    pricefeed.url = "./simulation/actors/pricefeed";
    controller.url = "./simulation/controller";
  };

  outputs = { self, nixpkgs, trader, pricefeed, controller }:
  let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
  in {
    packages = {
      inherit (trader.packages) default;
      inherit (pricefeed.packages) default;
      inherit (controller.packages) default;
    };
  };
}
```

### Step 4.5 Implement Geo-Distributed Deployment (Deploy.hs)

- Allows spawning remote actors via SSH.

```haskell
startActorRemote :: ActorSpec -> IO ()
startActorRemote spec = do
    let host = lookupHostForActor (actorId spec)
    _ <- spawnProcess "ssh" [host, "nix run", flakeForActorType (actorType spec)]
    pure ()
```

### Step 4.6 Cleanup After Phase 4

After implementing multi-mode simulation:

**Actor Abstraction Cleanup**:
- Migrate any actor-specific code to use the new abstraction
- Update any hard-coded actor implementations to use the common interface
- Ensure all simulation modes work with the same codebase

**Deployment Cleanup**:
- Remove any old deployment scripts that are replaced by the Nix flake system
- Update documentation to reflect the new deployment approach

## Phase 5: Implement Final Security & Invariant Checks

**Goal**: Enforce system-level safety guarantees.

### Step 5.1 Enforce Single-Owner Rule in ProgramMemory.hs

- Only one program can hold a resource at a time.

```haskell
validateOwnership :: ProgramMemory -> Resource -> Either ExecutionError ()
validateOwnership mem res = 
    if res `elem` (elems (slots mem))
        then Left DoubleSpend
        else Right ()
```

### Step 5.2 Implement Replayable Execution Checks in Verifier.hs

```haskell
verifyCausalOrder :: ExecutionLog -> Bool
verifyCausalOrder log = all isValidCausalRelation (generatePairs log)
```

### Step 5.3 Implement Security Property Tests

**Property: No Double Spend**
- Verify validateOwnership prevents reuse.

**Property: No Implicit Invocation**
- Ensure all program calls happen via InvokeProgram.

**Property: No Causal Violations**
- Ensure all execution logs respect LamportClock order.

### Step 5.4 Final Cleanup

After implementing all security and invariant checks:

**Complete Deprecated Code Removal**:
- Remove all code previously marked as deprecated
- Ensure all modules use the new abstractions and patterns
- Verify that no implicit state updates remain in the codebase

**Documentation Update**:
- Update all documentation to reflect the new architecture
- Create migration guides for any external users of the codebase
- Document the security guarantees and how they're enforced

## Final Deliverables

- Timeline.hs
- Resource.hs
- Program.hs
- Effect.hs
- EffectExecutor.hs
- InvokeProgram.hs
- LamportClock.hs
- TimeMap.hs
- Controller.hs
- Scenario.hs
- Deploy.hs
- Nix flakes for per-actor builds
- TOML-based scenario files

## Continuous Cleanup Strategy

To maintain code quality throughout the refactor process:

1. **After Each Implementation Step**:
   - Create adapter functions for backward compatibility
   - Mark old code as deprecated with directions to new modules
   - Run tests to ensure functionality is preserved

2. **During Migration**:
   - Update imports one module at a time
   - Keep adapter functions until all dependencies are updated
   - Maintain test coverage for both old and new implementations

3. **Final Cleanup**:
   - Remove all deprecated code
   - Remove adapter functions
   - Ensure all tests pass with the new implementation only

## Detailed Migration Guides

This section provides detailed migration guides for each component that's being refactored. These guides outline the steps to migrate from the existing implementation to the new architecture.

### Resource Module Migration Guide

#### Overview
This guide outlines the steps to migrate the `ResourceOps` effect from `TimeBandits.Effects` to our new `TimeBandits.Resource` module. This is part of our larger refactoring effort to create more focused, single-responsibility modules.

#### Current State

**In TimeBandits.Effects:**
- `ResourceOps` typeclass with methods for resource management
- `ResourceOperationEffect` data type implementing the effect
- Instance connecting the typeclass to the effect

**In TimeBandits.Resource:**
- Basic resource type definitions
- Simplified resource operations with placeholder implementations
- No effect system integration yet

#### Migration Steps

##### Step 1: Enhance Resource.hs
1. Add the full `ResourceOps` effect to `Resource.hs`
2. Implement all the operations with proper implementations
3. Keep the same interface for compatibility

```haskell
-- | Resource operations effect
data ResourceEffect m a where
  CreateResource :: ByteString -> ActorHash -> TimelineHash -> ResourceEffect m (Either AppError Resource)
  TransferResource :: Resource -> ActorHash -> TimelineHash -> ResourceEffect m (Either AppError Resource)
  ConsumeResource :: Resource -> ResourceEffect m (Either AppError Resource)
  VerifyResource :: Resource -> ResourceEffect m (Either AppError Bool)
  GetResource :: ResourceHash -> ResourceEffect m (Either AppError Resource)
  GetResourcesByOwner :: ActorHash -> ResourceEffect m (Either AppError [Resource])
  GetResourcesByTimeline :: TimelineHash -> ResourceEffect m (Either AppError [Resource])
  CreateTransaction :: [Resource] -> [Resource] -> ActorHash -> TimelineHash -> ResourceEffect m (Either AppError UnifiedResourceTransaction)
  ValidateTransaction :: UnifiedResourceTransaction -> ResourceEffect m (Either AppError TransactionValidationResult)
  ExecuteTransaction :: UnifiedResourceTransaction -> ResourceEffect m (Either AppError [Resource])
  TransactionHistory :: ResourceHash -> ResourceEffect m (Either AppError [UnifiedResourceTransaction])

makeSem ''ResourceEffect
```

##### Step 2: Create Adapter Functions
1. Create adapter functions in `Resource.hs` that map to the old interface
2. This allows gradual migration without breaking existing code

```haskell
-- | Adapter function to maintain compatibility with old ResourceOps interface
adaptResourceOps :: Member ResourceEffect r => ResourceOperationEffect m a -> Sem r a
adaptResourceOps = \case
  OpCreateResource meta owner timeline -> createResource meta owner timeline
  OpTransferResource res actor timeline -> transferResource res actor timeline
  -- ... and so on for all operations
```

##### Step 3: Update Imports
1. Update imports in files that use `ResourceOps` to use `TimeBandits.Resource` instead
2. Replace `ResourceOperationEffect` with `ResourceEffect` in effect stacks

##### Step 4: Deprecate Old Code
1. Mark the old `ResourceOps` and `ResourceOperationEffect` as deprecated
2. Add comments directing users to the new module

```haskell
-- | DEPRECATED: Use TimeBandits.Resource instead
-- This will be removed in a future version
class ResourceOps r where
  ...
```

##### Step 5: Remove Old Code
1. After all code has been migrated, remove the old implementations
2. This should be done in a separate commit after thorough testing

### Program Module Migration Guide

#### Overview
This guide outlines the steps to migrate Program-related functionality from `TimeBandits.Core` and `TimeBandits.Effects` to our new `TimeBandits.Program` and `TimeBandits.ProgramEffect` modules. This is part of our larger refactoring effort to create more focused, single-responsibility modules.

#### Current State

**In TimeBandits.Core and TimeBandits.Effects:**
- Program-related types and functions are scattered across modules
- No clear separation between program state and program effects
- Implicit state updates rather than explicit effect execution

**In our new modules:**
- `TimeBandits.Program`: Defines program state and memory
- `TimeBandits.ProgramEffect`: Defines effects as explicit operations
- `TimeBandits.EffectExecutor`: Implements effect execution pipeline

#### Migration Steps

##### Step 1: Identify Program-Related Code in Core and Effects
1. Identify all program-related types, functions, and effects in the old modules
2. Map them to their new locations in our refactored modules

| Old Location | Type/Function | New Location |
|--------------|--------------|--------------|
| TimeBandits.Core | Program-related types | TimeBandits.Program |
| TimeBandits.Effects | Program execution effects | TimeBandits.ProgramEffect |
| TimeBandits.Effects | Effect handlers | TimeBandits.EffectExecutor |

##### Step 2: Create Adapter Functions
1. Create adapter functions in the new modules that map to the old interface
2. This allows gradual migration without breaking existing code

```haskell
-- | Adapter function to maintain compatibility with old Program interface
adaptProgramOps :: Member ProgramEffect r => OldProgramEffect m a -> Sem r a
adaptProgramOps = \case
  OldExecuteProgram pid fn args -> executeProgram pid fn args
  -- ... and so on for all operations
```

##### Step 3: Update Imports
1. Update imports in files that use Program functionality to use the new modules
2. Replace old effect types with new effect types in effect stacks

##### Step 4: Deprecate Old Code
1. Mark the old Program-related code as deprecated
2. Add comments directing users to the new modules

```haskell
-- | DEPRECATED: Use TimeBandits.Program and TimeBandits.ProgramEffect instead
-- This will be removed in a future version
data OldProgramEffect m a where
  ...
```

##### Step 5: Remove Old Code
1. After all code has been migrated, remove the old implementations
2. This should be done in a separate commit after thorough testing

#### Special Considerations
1. **Memory Model**: The new Program module uses an explicit memory model with slots, which may require adapting code that assumed implicit state.
2. **Effect Execution**: The new EffectExecutor provides a more structured approach to effect execution, which may require changes to how programs are invoked.
3. **Cross-Timeline Effects**: The new TimeMap module handles cross-timeline state, which may require adapting code that assumed direct timeline access.

### Timeline Module Migration Guide

#### Overview
This guide outlines the steps to migrate Timeline-related functionality from `TimeBandits.Core` and `TimeBandits.Effects` to our new `TimeBandits.Timeline` and `TimeBandits.TimeMap` modules. This is part of our larger refactoring effort to create more focused, single-responsibility modules.

#### Current State

**In TimeBandits.Core and TimeBandits.Effects:**
- Timeline-related types and functions are scattered across modules
- No clear separation between timeline state and cross-timeline operations
- Limited support for causal ordering across timelines

**In our new modules:**
- `TimeBandits.Timeline`: Defines timeline state and operations
- `TimeBandits.TimeMap`: Implements cross-timeline state tracking and causal ordering

#### Migration Steps

##### Step 1: Identify Timeline-Related Code in Core and Effects
1. Identify all timeline-related types, functions, and effects in the old modules
2. Map them to their new locations in our refactored modules

| Old Location | Type/Function | New Location |
|--------------|--------------|--------------|
| TimeBandits.Core | Timeline, TimelineHash | TimeBandits.Timeline |
| TimeBandits.Core | MapOfTime, SyncPoint | TimeBandits.TimeMap |
| TimeBandits.Effects | Timeline operations | TimeBandits.Timeline |
| TimeBandits.Effects | Cross-timeline operations | TimeBandits.TimeMap |

##### Step 2: Create Adapter Functions
1. Create adapter functions in the new modules that map to the old interface
2. This allows gradual migration without breaking existing code

```haskell
-- | Adapter function to maintain compatibility with old Timeline interface
adaptTimelineOps :: Member TimelineEffect r => OldTimelineEffect m a -> Sem r a
adaptTimelineOps = \case
  OldCreateTimeline name owner -> createTimeline name owner
  -- ... and so on for all operations
```

##### Step 3: Update Imports
1. Update imports in files that use Timeline functionality to use the new modules
2. Replace old effect types with new effect types in effect stacks

##### Step 4: Deprecate Old Code
1. Mark the old Timeline-related code as deprecated
2. Add comments directing users to the new modules

```haskell
-- | DEPRECATED: Use TimeBandits.Timeline and TimeBandits.TimeMap instead
-- This will be removed in a future version
data OldTimelineEffect m a where
  ...
```

##### Step 5: Remove Old Code
1. After all code has been migrated, remove the old implementations
2. This should be done in a separate commit after thorough testing

#### Special Considerations
1. **Lamport Clocks**: The new TimeMap module uses Lamport clocks for causal ordering, which may require adapting code that assumed simple timeline ordering.
2. **Cross-Timeline Consistency**: The new architecture provides stronger guarantees for cross-timeline consistency, which may change some assumptions in existing code.
3. **Timeline Events**: The new Timeline module has a more structured approach to timeline events, which may require changes to how events are created and processed.

### Master Cleanup Plan

#### Overview
This section provides a high-level overview of the cleanup process after implementing the new architecture according to the refactor plan.

#### Overall Strategy

##### Phase 1: Preparation
1. Complete implementation of all new modules
2. Create adapter functions for backward compatibility
3. Document all changes and migration paths

##### Phase 2: Gradual Migration
1. Update imports in one module at a time
2. Mark old code as deprecated but keep it functional
3. Run tests after each module migration

##### Phase 3: Cleanup
1. Remove deprecated code after all dependencies have been updated
2. Run final tests to ensure everything works correctly
3. Update documentation to reflect the new architecture

#### Testing Strategy
1. Create unit tests for all new modules
2. Ensure all existing tests pass with the new implementation
3. Test adapter functions to verify backward compatibility
4. Run integration tests to verify system-wide functionality

#### Commit Strategy
After each step in the refactor plan, we should:
1. Commit the new implementation
2. Commit the adapter functions
3. Commit the deprecation notices
4. Commit the removal of deprecated code (only after all dependencies are updated)

This ensures that we have a clean git history and can easily revert changes if needed.
