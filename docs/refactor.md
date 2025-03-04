# Time Bandits Refactor Plan

Including:

- Program-based execution with explicit resource ownership
- Causal ordering via Lamport clocks and time maps
- Invocation model with safe resource passing
- Multi-mode simulation (in-memory, local, geo-distributed)
- Nix flakes for per-actor builds & simulation controller

## ✅ Phase 1: Core Type Abstractions & Separation of Concerns

**Goal**: Establish clear Timeline, Resource, Program, Effect separation.

### ✅ Step 1.1 Introduce Timeline, Resource, Program, and Effect as First-Class Types

- ✅ Move timeline interactions into `Timeline.hs`
- ✅ Define resources as first-class entities in `Resource.hs`
- ✅ Define program state and memory in `Program.hs`
- ✅ Define effects as explicit operations in `Effect.hs`

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

### ✅ Step 1.2 Replace Implicit State Updates with Explicit Effect Execution

- ✅ All timeline changes should now happen via Effect application.
- ✅ No more direct state modification—everything must go through an effect handler.
- ✅ Implement initial effect execution pipeline in `EffectExecutor.hs`:

```haskell
applyEffect :: ProgramState -> Effect -> Either ExecutionError ProgramState
applyEffect state (EscrowToProgram res pid slot) = modifyMemory pid slot (Just res) state
applyEffect state (InvokeProgram pid fn args) = executeProgram pid fn args state
applyEffect state (ClaimFromProgram pid slot res) = claimResource pid slot res state
```

### ✅ Step 1.3 Cleanup After Phase 1

After implementing the new modules, clean up existing code to avoid duplication:

**Resource Module Cleanup**:
- ✅ Identify `ResourceOps` effect in `TimeBandits.Effects` and related operations
- ✅ Mark as deprecated with comments directing to `TimeBandits.Resource`
- ✅ Create adapter functions in `Resource.hs` for backward compatibility

**Program Module Cleanup**:
- ✅ Identify program-related types and functions in `TimeBandits.Core` and `TimeBandits.Effects`
- ✅ Mark as deprecated with comments directing to `TimeBandits.Program` and `TimeBandits.ProgramEffect`
- ✅ Create adapter functions in the new modules for backward compatibility

**Timeline Module Cleanup**:
- ✅ Identify timeline-related types and functions in `TimeBandits.Core` and `TimeBandits.Effects`
- ✅ Mark as deprecated with comments directing to `TimeBandits.Timeline`
- ✅ Create adapter functions in `Timeline.hs` for backward compatibility

**General Approach**:
- ✅ Do not remove any code yet, only mark as deprecated
- ✅ Update imports gradually as other modules are refactored
- ✅ Ensure tests pass with the new implementations

## ✅ Phase 2: Implement Program Invocation & Resource Ownership Model

**Goal**: Enable programs to own and transfer resources explicitly.

### ✅ Step 2.1 Implement EscrowToProgram & ClaimFromProgram

- ✅ Introduce escrow memory slots inside `ProgramMemory.hs`.
- ✅ Programs lose ownership of resources when escrowing them.
- ✅ Only the target program can claim escrowed resources.

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

### ✅ Step 2.2 Implement InvokeProgram for Cross-Program Calls

- ✅ Implement function call execution between programs.
- ✅ Programs should declare entry points and allow safe invocation.

Example in `InvokeProgram.hs`:

```haskell
invokeProgram :: ProgramId -> FunctionName -> [Resource] -> Either ExecutionError ProgramState
invokeProgram pid fn args = do
    program <- lookupProgram pid
    case lookupFunction fn program of
        Just f -> f args
        Nothing -> Left FunctionNotFound
```

### ✅ Step 2.3 Cleanup After Phase 2

After implementing the program invocation and resource ownership model:

**Program Module Finalization**:
- ✅ Update any remaining code using the old program invocation patterns
- ✅ Convert direct program state updates to use explicit effect execution
- ✅ Complete adapter functions in `Program.hs` and `ProgramEffect.hs`

**Resource Ownership Cleanup**:
- ✅ Identify and update code that manipulates resources directly
- ✅ Ensure all resource transfers go through the escrow mechanism
- ✅ Update tests to use the new escrow/claim pattern

## ✅ Phase 3: Implement Time Maps & Transition Messages

**Goal**: Ensure programs execute in causal order across timelines with proper authorization and proof.

### ✅ Step 3.1 Implement Time Maps for Cross-Timeline Consistency

- ✅ Maintain a snapshot of multiple timelines with their latest state
- ✅ Track Lamport clocks for logical time ordering
- ✅ Ensure programs always observe strictly advancing time maps

```haskell
data TimeMap = TimeMap 
    { timelines :: Map TimelineId LamportClock
    , observedHeads :: Map TimelineId BlockHeader 
    , observedTimestamps :: Map TimelineId UTCTime
    }
```

### ✅ Step 3.2 Implement TransitionMessage for Program Execution

- ✅ All program transitions must be triggered by a TransitionMessage
- ✅ Messages include proof of resource control and guard validation
- ✅ Each message links to its causal parent (previous effect)

```haskell
data TransitionMessage = TransitionMessage 
    { programId :: ProgramId 
    , stepIndex :: Int 
    , parentEffectHash :: Hash 
    , proof :: ZKProof  -- For validating guard conditions
    , resources :: [Resource]  -- Resources being used
    }
```

### ✅ Step 3.3 Implement Execution Log for Applied Effects

- ✅ Every applied effect produces a log entry for auditability
- ✅ Log entries are content-addressed and causally linked
- ✅ The execution log is fully replayable

```haskell
data LogEntry = LogEntry 
    { effect :: Effect 
    , appliedAt :: UTCTime 
    , causalParent :: Hash 
    , resultingStateHash :: Hash 
    }
```

### ⏳ Step 3.4 Cleanup After Phase 3

After implementing time maps and transition messages:

**Timeline Consistency Cleanup**:
- ⏳ Migrate existing timeline logic to use the new TimeMap implementation
- ⏳ Update any code that relies on timeline-specific time ordering
- ⏳ Ensure all cross-timeline operations respect causal ordering

**Transition Message Integration**:
- ⏳ Replace direct effect execution with TransitionMessage validation
- ⏳ Update the EffectExecutor to verify proofs and resource ownership
- ⏳ Update tests to use transition messages for program advancement

## ⏳ Phase 4: Implement Controller & Multi-Mode Simulation

**Goal**: Support different deployment modes with consistent controller behavior and clearly defined actor roles.

### ⏳ Step 4.1 Implement Controller (Controller.hs)

- ⏳ Enforces system contract regardless of deployment mode
- ⏳ Handles transition message validation, effect application, and time map updates
- ⏳ Maintains the append-only execution log
- ⏳ Coordinates between Time Travelers, Time Keepers, and Time Bandits

```haskell
data Controller = Controller 
    { mode :: SimulationMode
    , timeMap :: TimeMap
    , executionLog :: [LogEntry]
    }

data SimulationMode = InMemory | LocalProcesses | GeoDistributed

runController :: Controller -> TransitionMessage -> IO (Either ExecutionError (Controller, ProgramState))
runController controller msg = do
    -- Validate message (signature, proof, resources)
    -- Coordinate with Time Keepers to validate against timeline rules
    -- Utilize Time Bandits to apply effect and generate proofs
    -- Update program state
    -- Update time map and append to execution log
    pure $ Right (updatedController, newProgramState)
```

### ✅ Step 4.2 Implement Actor Abstraction for Different Modes

- ✅ Define the three key actor roles: Time Travelers, Time Keepers, and Time Bandits
- ✅ All actors implement the same interface, regardless of deployment mode
- ✅ In-memory: Actors are Haskell functions in the same process
- ✅ Local multi-process: Actors run in separate processes with Unix socket messaging
- ✅ Geo-distributed: Actors run on remote machines with TCP/RPC messaging

```haskell
-- Base actor with common properties
class Actor a where
    runActor :: a -> IO ()
    actorId :: a -> ActorID
    sendMessage :: a -> TransitionMessage -> IO ()
    receiveMessage :: a -> IO (Maybe TransitionMessage)

-- Specialized actor types
data TimeTraveler = TimeTraveler
    { travelerId :: ActorID
    , capabilities :: [Capability]
    , ownedResources :: [ResourceID]
    , programAccess :: [ProgramID]
    }

data TimeKeeper = TimeKeeper
    { keeperId :: ActorID
    , managedTimelines :: [TimelineID]
    , validationRules :: [ValidationRule]
    , timelineStates :: Map TimelineID TimelineState
    }

data TimeBandit = TimeBandit
    { banditId :: ActorID
    , networkRole :: NetworkRole
    , connectedPeers :: [PeerID]
    , proofGenerators :: [ProofGenerator]
    , executedPrograms :: Map ProgramID ExecutionState
    }
```

### ⏳ Step 4.3 Implement Deployment for Different Modes

- ⏳ In-memory: Direct function calls with in-memory queues
- ⏳ Local multi-process: Spawn processes with Nix and communicate via Unix sockets
- ⏳ Geo-distributed: Remote execution via SSH with TCP or external RPC
- ⏳ Each deployment mode must support all three actor roles

```haskell
deployActor :: SimulationMode -> ActorSpec -> IO ActorHandle
deployActor InMemory spec = deployInMemoryActor spec
deployActor LocalProcesses spec = deployLocalActor spec
deployActor GeoDistributed spec = deployRemoteActor spec

-- Deploy specific actor types
deployTimeTraveler :: SimulationMode -> TimeTravelerSpec -> IO TimeTravelerHandle
deployTimeKeeper :: SimulationMode -> TimeKeeperSpec -> IO TimeKeeperHandle
deployTimeBandit :: SimulationMode -> TimeBanditSpec -> IO TimeBanditHandle
```

### 🔜 Step 4.4 Implement Scenario Definition with TOML

- 🔜 Define actors, their roles, and initial program deployments
- 🔜 Specify the simulation mode
- 🔜 Configure communication channels and deployment targets
- 🔜 Support all three actor roles in scenario definitions

```TOML
-- Example TOML structure for scenario definition
-- [scenario]
-- name = "Cross-Timeline Resource Transfer"
-- mode = "LocalProcesses"
--
-- [[time_travelers]]
-- id = "alice"
-- capabilities = ["ResourceCreation", "ResourceTransfer"]
--
-- [[time_keepers]]
-- id = "ethereum_keeper"
-- timelines = ["ethereum_main"]
--
-- [[time_bandits]]
-- id = "network_node_1"
-- roles = ["ProofGeneration", "NetworkCoordination"]
```
```haskell
loadScenario :: FilePath -> IO Scenario
loadScenario path = do
    toml <- readFile path
    parseScenario toml
```

### 🔜 Step 4.5 Cleanup After Phase 4

After implementing the controller and multi-mode simulation:

**Controller Integration**:
- 🔜 Update any code that directly applies effects to use the controller
- 🔜 Ensure all program execution happens through TransitionMessages
- 🔜 Update tests to verify controller behavior in different modes

**Actor Implementation Cleanup**:
- 🔜 Migrate any actor-specific code to use the common abstraction
- 🔜 Ensure proper separation of concerns between Time Travelers, Time Keepers, and Time Bandits
- 🔜 Update deployment scripts to work with the new modes
- 🔜 Ensure backwards compatibility during the transition

## 🔜 Phase 5: Implement Security & Invariant Checks

**Goal**: Enforce the system contract's security properties and invariants.

### 🔜 Step 5.1 Implement Ownership Verification

- 🔜 Ensure each resource has exactly one owner at any time
- 🔜 Enforce that only authorized programs can access resources
- 🔜 Verify ownership transfer through proper escrow and claim operations

```haskell
verifyOwnership :: Resource -> Address -> Either SecurityError ()
verifyOwnership res addr =
    if resourceOwner res == addr
        then Right ()
        else Left $ OwnershipVerificationFailed res addr
```

### 🔜 Step 5.2 Implement Causal Order Verification

- 🔜 Ensure time maps always advance monotonically
- 🔜 Verify that cross-timeline events respect logical clock ordering
- 🔜 Prevent backdated transitions with stale time maps

```haskell
verifyCausalOrder :: TimeMap -> TimeMap -> Either SecurityError ()
verifyCausalOrder oldMap newMap =
    if allTimelinesCausallyAdvanced oldMap newMap
        then Right ()
        else Left CausalOrderViolation
```

### 🔜 Step 5.3 Implement ZK Proof Generation and Verification

- 🔜 Generate zero-knowledge proofs for guard conditions
- 🔜 Verify proofs before applying effects
- 🔜 Ensure all transitions carry valid proofs

```haskell
generateProof :: Guard -> Resource -> IO ZKProof
generateProof guard res = -- Implementation depends on specific ZK system

verifyProof :: ZKProof -> Guard -> Resource -> IO Bool
verifyProof proof guard res = -- Implementation depends on specific ZK system
```

### 🔜 Step 5.4 Implement System-Level Security Properties

- 🔜 Double-spend prevention through single-owner rule
- 🔜 Reentrancy prevention through Lamport clocks
- 🔜 Traceability through complete audit trail
- 🔜 Prevention of backdated transitions through time map enforcement

```haskell
data SecurityProperty
    = NoDoubleSpend
    | NoReentrancy
    | FullTraceability
    | NoBackdating

verifySecurityProperty :: SecurityProperty -> ExecutionLog -> Either SecurityError ()
verifySecurityProperty NoDoubleSpend log = -- Verify no resource is used twice
verifySecurityProperty NoReentrancy log = -- Verify no cycles in the execution graph
-- etc.
```

### 🔜 Step 5.5 Final Cleanup

After implementing all security and invariant checks:

**Complete Deprecated Code Removal**:
- ⏳ Remove all code previously marked as deprecated
- ⏳ Ensure all modules use the new abstractions and patterns
- ⏳ Verify that no implicit state updates remain in the codebase

**Documentation Update**:
- 🔜 Update all documentation to reflect the new architecture
- 🔜 Create migration guides for any external users of the codebase
- 🔜 Document the security guarantees and how they're enforced

## Final Deliverables

- ✅ Timeline.hs - Causally ordered event streams with own consistency models
- ✅ TimeMap.hs - Cross-timeline state tracking with Lamport clocks
- ✅ Resource.hs - Program-owned state with ownership tracking
- ✅ Program.hs - Program state and memory with resource contracts
- ✅ ProgramEffect.hs - Explicit effects with guards
- ✅ EffectExecutor.hs - Effect application with invariant checking
- ⏳ Controller.hs - System contract enforcement across simulation modes
- ⏳ TransitionMessage.hs - Proof-carrying program transitions
- ⏳ ExecutionLog.hs - Append-only, causally linked effect log
- ✅ Actor.hs - Common actor interface with shared functionality for all roles
- ✅ ActorRole.hs - Base definitions for specialized actor roles

### Actor Role Implementations
- ✅ TimeTraveler.hs - Implementation of Time Travelers who create and use programs:
  - ✅ Program creation and deployment
  - ✅ TransitionMessage submission
  - ✅ Program state and timeline queries
  - ✅ Resource ownership management
  
- ✅ TimeKeeper.hs - Implementation of Time Keepers who maintain timeline integrity:
  - ✅ Timeline validation rules
  - ✅ Message acceptance/rejection logic
  - ✅ Timeline state queries
  - ✅ Consistency enforcement across timelines
  
- ✅ TimeBandit.hs - Implementation of Time Bandits who run the P2P network:
  - ✅ P2P network operations
  - ✅ Program execution engine
  - ✅ Proof generation and verification
  - ✅ Execution log maintenance
  
- ✅ ActorCoordination.hs - Protocols for interaction between actor roles:
  - ✅ Time Traveler → Time Keeper communication
  - ✅ Time Keeper → Time Bandit coordination
  - ✅ Time Bandit → Time Keeper synchronization
  - ✅ Role-based access control mechanisms

### Actor Role Implementation Details

#### Time Traveler Implementation

The Time Traveler role implementation includes:

```haskell
data TimeTraveler = TimeTraveler
    { travelerId :: ActorID
    , capabilities :: [Capability]
    , ownedResources :: [ResourceID]
    , programAccess :: [ProgramID]
    }
```

Key functions include:

- ✅ `deployProgram` - Create and deploy new programs with initial state
- ✅ `submitTransition` - Generate transition messages with proper signatures
- ✅ `queryProgramState` - Retrieve the current state of programs
- ✅ `queryTimeline` - Access timeline state filtered by capabilities
- ✅ `transferResource` - Transfer ownership of resources to other actors

Time Travelers are responsible for initiating all program transitions by creating signed transition messages that include:
- The program ID being invoked
- The effect to be applied
- Resources required for the operation
- Cryptographic proofs when needed

#### Time Keeper Implementation

The Time Keeper role implementation includes:

```haskell
data TimeKeeper = TimeKeeper
    { keeperId :: ActorID
    , managedTimelines :: [TimelineID]
    , validationRules :: [ValidationRule]
    , timelineStates :: Map TimelineID TimelineState
    }
```

Key functions include:

- ✅ `validateMessage` - Verify transition message validity against rules
- ✅ `applyToTimeline` - Apply validated messages to timeline state
- ✅ `serveTimelineQuery` - Provide timeline state information to authorized actors
- ✅ `registerProgram` - Register new programs with timelines they interact with

Time Keepers maintain the authoritative state of timelines and ensure that:
- Programs only see timelines they're authorized to access
- All applied transitions follow timeline-specific rules
- Timeline states advance monotonically 
- Queries are only served to authorized actors

#### Time Bandit Implementation

The Time Bandit role implementation includes:

```haskell
data TimeBandit = TimeBandit
    { banditId :: ActorID
    , networkRole :: NetworkRole
    , connectedPeers :: [PeerID]
    , proofGenerators :: [ProofGenerator]
    , executedPrograms :: Map ProgramID ExecutionState
    }
```

Key functions include:

- ✅ `executeProgram` - Execute program steps and generate proofs
- ✅ `propagateMessage` - Disseminate messages through the P2P network
- ✅ `generateProof` - Create cryptographic proofs for transitions
- ✅ `verifyProof` - Validate proofs for transition message verification
- ✅ `appendToExecutionLog` - Maintain the append-only execution log

Time Bandits form the distributed infrastructure of the system, ensuring that:
- Program execution is consistent across all nodes
- Cryptographic proofs are generated for all transitions
- The P2P network maintains a consistent view of the system
- Execution logs are properly synchronized across the network

#### Role Interactions

The three actor roles interact through well-defined protocols:

1. **Time Traveler → Time Keeper**:
   - ✅ Time Travelers submit TransitionMessages to Time Keepers
   - ✅ Time Keepers validate and process these messages
   - ✅ Time Travelers query timeline state through Time Keepers

2. **Time Keeper → Time Bandit**:
   - ✅ Time Keepers validate timeline consistency
   - ✅ Time Bandits execute programs and generate proofs
   - ✅ Time Keepers confirm final program states

3. **Time Bandit → Time Keeper**:
   - ✅ Time Bandits maintain the distributed execution log
   - ✅ Time Keepers provide timeline state updates
   - ✅ Both synchronize to maintain system consistency

An example of the complete flow:

```haskell
runProgramTransition :: Controller -> TimeTraveler -> TimeKeeper -> TimeBandit -> ProgramID -> Effect -> IO (Either Error TransitionResult)
runProgramTransition controller traveler keeper bandit pid effect = do
    -- 1. Time Traveler creates transition
    Right transMsg <- submitTransition traveler pid effect Nothing
    
    -- 2. Time Keeper validates transition
    Right valResult <- validateMessage keeper transMsg
    
    -- 3. Time Bandit executes program and generates proof
    Right (execResult, proof) <- executeProgram bandit pid effect
    
    -- 4. Time Traveler submits finalized transition with proof
    Right transId <- submitTransition traveler pid effect (Just proof)
    
    -- 5. Controller applies effect and updates time map
    Right (newController, newState) <- runController controller transMsg
    
    -- 6. Time Keeper updates timeline state
    Right newTimelineState <- applyToTimeline keeper (timelineOf pid) transMsg
    
    -- 7. Time Bandit updates execution log
    Right () <- appendToExecutionLog bandit (logEntryFrom transMsg execResult)
    
    pure $ Right TransitionResult { ... }
```

Each actor role must function consistently across all three simulation modes (In-Memory, Local Multi-Process, and Geo-Distributed) while maintaining the same behavior and security guarantees.

### Deployment and Execution
- ⏳ Deploy.hs - Multi-mode deployment system
- 🔜 Scenario.hs - Scenario definition and parsing
- 🔜 ZKProof.hs - Zero-knowledge proof generation and verification
- 🔜 SecurityVerifier.hs - System-level security property verification 
- ⏳ Nix flakes for per-actor builds
- 🔜 TOML-based scenario files with role assignments

## Continuous Cleanup Strategy

To maintain code quality throughout the refactor process:

1. **After Each Implementation Step**:
   - ✅ Create adapter functions for backward compatibility
   - ✅ Mark old code as deprecated with directions to new modules
   - ✅ Run tests to ensure functionality is preserved

2. **During Migration**:
   - ⏳ Update imports one module at a time
   - ⏳ Keep adapter functions until all dependencies are updated
   - ⏳ Maintain test coverage for both old and new implementations

3. **Final Cleanup**:
   - ⏳ Remove all deprecated code
   - ⏳ Remove adapter functions
   - ⏳ Ensure all tests pass with the new implementation only

## Detailed Migration Guides

This section provides detailed migration guides for each component that's being refactored. These guides outline the steps to migrate from the existing implementation to the new architecture.

### Resource Module Migration Guide

#### Overview
This guide outlines the steps to migrate the `ResourceOps` effect from `TimeBandits.Effects` to our new `TimeBandits.Resource` module. This is part of our larger refactoring effort to create more focused, single-responsibility modules.

#### Current State

**In TimeBandits.Effects:**
- ✅ `ResourceOps` typeclass with methods for resource management
- ✅ `ResourceOperationEffect` data type implementing the effect
- ✅ Instance connecting the typeclass to the effect

**In TimeBandits.Resource:**
- ✅ Basic resource type definitions
- ✅ Simplified resource operations with placeholder implementations
- ✅ No effect system integration yet

#### Migration Steps

##### Step 1: Enhance Resource.hs
1. ✅ Add the full `ResourceOps` effect to `Resource.hs`
2. ✅ Implement all the operations with proper implementations
3. ✅ Keep the same interface for compatibility

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
1. ✅ Create adapter functions in `Resource.hs` that map to the old interface
2. ✅ This allows gradual migration without breaking existing code

```haskell
-- | Adapter function to maintain compatibility with old ResourceOps interface
adaptResourceOps :: Member ResourceEffect r => ResourceOperationEffect m a -> Sem r a
adaptResourceOps = \case
  OpCreateResource meta owner timeline -> createResource meta owner timeline
  OpTransferResource res actor timeline -> transferResource res actor timeline
  -- ... and so on for all operations
```

##### Step 3: Update Imports
1. ✅ Update imports in files that use `ResourceOps` to use `TimeBandits.Resource` instead
2. ✅ Replace `ResourceOperationEffect` with `ResourceEffect` in effect stacks

##### Step 4: Deprecate Old Code
1. ✅ Mark the old `ResourceOps` and `ResourceOperationEffect` as deprecated
2. ✅ Add comments directing users to the new module

```haskell
-- | DEPRECATED: Use TimeBandits.Resource instead
-- This will be removed in a future version
class ResourceOps r where
  ...
```

##### Step 5: Remove Old Code
1. ⏳ After all code has been migrated, remove the old implementations
2. ⏳ This should be done in a separate commit after thorough testing

### Program Module Migration Guide

#### Overview
This guide outlines the steps to migrate Program-related functionality from `TimeBandits.Core` and `TimeBandits.Effects` to our new `TimeBandits.Program` and `TimeBandits.ProgramEffect` modules. This is part of our larger refactoring effort to create more focused, single-responsibility modules.

#### Current State

**In TimeBandits.Core and TimeBandits.Effects:**
- ✅ Program-related types and functions are scattered across modules
- ✅ No clear separation between program state and program effects
- ✅ Implicit state updates rather than explicit effect execution

**In our new modules:**
- ✅ `TimeBandits.Program`: Defines program state and memory
- ✅ `TimeBandits.ProgramEffect`: Defines effects as explicit operations
- ✅ `TimeBandits.EffectExecutor`: Implements effect execution pipeline

#### Migration Steps

##### Step 1: Identify Program-Related Code in Core and Effects
1. ✅ Identify all program-related types, functions, and effects in the old modules
2. ✅ Map them to their new locations in our refactored modules

| Old Location | Type/Function | New Location |
|--------------|--------------|--------------|
| TimeBandits.Core | Program-related types | TimeBandits.Program |
| TimeBandits.Effects | Program execution effects | TimeBandits.ProgramEffect |
| TimeBandits.Effects | Effect handlers | TimeBandits.EffectExecutor |

##### Step 2: Create Adapter Functions
1. ✅ Create adapter functions in the new modules that map to the old interface
2. ✅ This allows gradual migration without breaking existing code

```haskell
-- | Adapter function to maintain compatibility with old Program interface
adaptProgramOps :: Member ProgramEffect r => OldProgramEffect m a -> Sem r a
adaptProgramOps = \case
  OldExecuteProgram pid fn args -> executeProgram pid fn args
  -- ... and so on for all operations
```

##### Step 3: Update Imports
1. ✅ Update imports in files that use Program functionality to use the new modules
2. ✅ Replace old effect types with new effect types in effect stacks

##### Step 4: Deprecate Old Code
1. ✅ Mark the old Program-related code as deprecated
2. ✅ Add comments directing users to the new modules

```haskell
-- | DEPRECATED: Use TimeBandits.Program and TimeBandits.ProgramEffect instead
-- This will be removed in a future version
data OldProgramEffect m a where
  ...
```

##### Step 5: Remove Old Code
1. ⏳ After all code has been migrated, remove the old implementations
2. ⏳ This should be done in a separate commit after thorough testing

#### Special Considerations
1. **Memory Model**: The new Program module uses an explicit memory model with slots, which may require adapting code that assumed implicit state.
2. **Effect Execution**: The new EffectExecutor provides a more structured approach to effect execution, which may require changes to how programs are invoked.
3. **Cross-Timeline Effects**: The new TimeMap module handles cross-timeline state, which may require adapting code that assumed direct timeline access.

### Timeline Module Migration Guide

#### Overview
This guide outlines the steps to migrate Timeline-related functionality from `TimeBandits.Core` and `TimeBandits.Effects` to our new `TimeBandits.Timeline` and `TimeBandits.TimeMap` modules. This is part of our larger refactoring effort to create more focused, single-responsibility modules.

#### Current State

**In TimeBandits.Core and TimeBandits.Effects:**
- ✅ Timeline-related types and functions are scattered across modules
- ✅ No clear separation between timeline state and cross-timeline operations
- ✅ Limited support for causal ordering across timelines

**In our new modules:**
- ✅ `TimeBandits.Timeline`: Defines timeline state and operations
- ✅ `TimeBandits.TimeMap`: Implements cross-timeline state tracking and causal ordering

#### Migration Steps

##### Step 1: Identify Timeline-Related Code in Core and Effects
1. ✅ Identify all timeline-related types, functions, and effects in the old modules
2. ✅ Map them to their new locations in our refactored modules

| Old Location | Type/Function | New Location |
|--------------|--------------|--------------|
| TimeBandits.Core | Timeline, TimelineHash | TimeBandits.Timeline |
| TimeBandits.Core | MapOfTime, SyncPoint | TimeBandits.TimeMap |
| TimeBandits.Effects | Timeline operations | TimeBandits.Timeline |
| TimeBandits.Effects | Cross-timeline operations | TimeBandits.TimeMap |

##### Step 2: Create Adapter Functions
1. ✅ Create adapter functions in the new modules that map to the old interface
2. ✅ This allows gradual migration without breaking existing code

```haskell
-- | Adapter function to maintain compatibility with old Timeline interface
adaptTimelineOps :: Member TimelineEffect r => OldTimelineEffect m a -> Sem r a
adaptTimelineOps = \case
  OldCreateTimeline name owner -> createTimeline name owner
  -- ... and so on for all operations
```

##### Step 3: Update Imports
1. ✅ Update imports in files that use Timeline functionality to use the new modules
2. ✅ Replace old effect types with new effect types in effect stacks

##### Step 4: Deprecate Old Code
1. ✅ Mark the old Timeline-related code as deprecated
2. ✅ Add comments directing users to the new modules

```haskell
-- | DEPRECATED: Use TimeBandits.Timeline and TimeBandits.TimeMap instead
-- This will be removed in a future version
data OldTimelineEffect m a where
  ...
```

##### Step 5: Remove Old Code
1. ⏳ After all code has been migrated, remove the old implementations
2. ⏳ This should be done in a separate commit after thorough testing

#### Special Considerations
1. **Lamport Clocks**: The new TimeMap module uses Lamport clocks for causal ordering, which may require adapting code that assumed simple timeline ordering.
2. **Cross-Timeline Consistency**: The new architecture provides stronger guarantees for cross-timeline consistency, which may change some assumptions in existing code.
3. **Timeline Events**: The new Timeline module has a more structured approach to timeline events, which may require changes to how events are created and processed.

### Master Cleanup Plan

#### Overview
This section provides a high-level overview of the cleanup process after implementing the new architecture according to the refactor plan.

#### Overall Strategy

##### Phase 1: Preparation
1. ✅ Complete implementation of all new modules
2. ✅ Create adapter functions for backward compatibility
3. ✅ Document all changes and migration paths

##### Phase 2: Gradual Migration
1. ⏳ Update imports in one module at a time
2. ⏳ Mark old code as deprecated but keep it functional
3. ⏳ Run tests after each module migration

##### Phase 3: Cleanup
1. ⏳ Remove deprecated code after all dependencies have been updated
2. ⏳ Run final tests to ensure everything works correctly
3. ⏳ Update documentation to reflect the new architecture

#### Testing Strategy
1. ✅ Create unit tests for all new modules
2. ⏳ Ensure all existing tests pass with the new implementation
3. ⏳ Test adapter functions to verify backward compatibility
4. 🔜 Run integration tests to verify system-wide functionality

#### Commit Strategy
After each step in the refactor plan, we should:
1. ✅ Commit the new implementation
2. ✅ Commit the adapter functions
3. ⏳ Commit the deprecation notices
4. 🔜 Commit the removal of deprecated code (only after all dependencies are updated)
