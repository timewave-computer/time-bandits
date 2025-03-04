# Time Bandits Refactor Plan

Includes:

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

## ✅ Phase 4: Implement Controller & Multi-Mode Simulation

**Goal**: Support different deployment modes with consistent controller behavior and clearly defined actor roles.

### ✅ Step 4.1 Implement Controller (Controller.hs)

- ✅ Enforces system contract regardless of deployment mode
- ✅ Handles transition message validation, effect application, and time map updates
- ✅ Maintains the append-only execution log
- ✅ Coordinates between Time Travelers, Time Keepers, and Time Bandits

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

### ✅ Step 4.3 Implement Deployment for Different Modes

- ✅ In-memory: Direct function calls with in-memory queues
- ✅ Local multi-process: Spawn processes with Nix and communicate via Unix sockets
- ✅ Geo-distributed: Remote execution via SSH with TCP or external RPC
- ✅ Each deployment mode must support all three actor roles

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

### ✅ Step 4.4 Implement Scenario Definition with TOML

- 🔜 Define actors, their roles, and initial program deployments
- 🔜 Specify the simulation mode
- 🔜 Configure communication channels and deployment targets
- 🔜 Support all three actor roles in scenario definitions
- ✅ Define actors, their roles, and initial program deployments
- ✅ Specify the simulation mode
- ✅ Configure communication channels and deployment targets
- ✅ Support all three actor roles in scenario definitions

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

### ✅ Step 4.5 Cleanup After Phase 4

After implementing the controller and multi-mode simulation:

**Controller Integration**:
- 🔜 Update any code that directly applies effects to use the controller
- 🔜 Ensure all program execution happens through TransitionMessages
- 🔜 Update tests to verify controller behavior in different modes
- ✅ Update any code that directly applies effects to use the controller
- ✅ Ensure all program execution happens through TransitionMessages
- ✅ Update tests to verify controller behavior in different modes

**Actor Implementation Cleanup**:
- 🔜 Migrate any actor-specific code to use the common abstraction
- 🔜 Ensure proper separation of concerns between Time Travelers, Time Keepers, and Time Bandits
- 🔜 Update deployment scripts to work with the new modes
- 🔜 Ensure backwards compatibility during the transition
- ✅ Migrate any actor-specific code to use the common abstraction
- ✅ Ensure proper separation of concerns between Time Travelers, Time Keepers, and Time Bandits
- ✅ Update deployment scripts to work with the new modes
- ✅ Ensure backwards compatibility during the transition

## ✅ Phase 5: Implement Security & Invariant Checks

**Goal**: Enforce the system contract's security properties and invariants.

### ✅ Step 5.1 Implement Ownership Verification

- ✅ Ensure each resource has exactly one owner at any time
- ✅ Enforce that only authorized programs can access resources
- ✅ Verify ownership transfer through proper escrow and claim operations

```haskell
verifyOwnership :: Resource -> Address -> Either SecurityError ()
verifyOwnership res addr =
    if resourceOwner res == addr
        then Right ()
        else Left $ OwnershipVerificationFailed res addr
```

### ✅ Step 5.2 Implement Causal Order Verification

- ✅ Ensure time maps always advance monotonically
- ✅ Verify that cross-timeline events respect logical clock ordering
- ✅ Prevent backdated transitions with stale time maps

```haskell
verifyCausalOrder :: TimeMap -> TimeMap -> Either SecurityError ()
verifyCausalOrder oldMap newMap =
    if allTimelinesCausallyAdvanced oldMap newMap
        then Right ()
        else Left CausalOrderViolation
```

### ✅ Step 5.3 Implement ZK Proof Generation and Verification

- 🔜 Generate zero-knowledge proofs for guard conditions
- 🔜 Verify proofs before applying effects
- 🔜 Ensure all transitions carry valid proofs

```haskell
generateProof :: Guard -> Resource -> IO ZKProof
generateProof guard res = -- Implementation depends on specific ZK system

verifyProof :: ZKProof -> Guard -> Resource -> IO Bool
verifyProof proof guard res = -- Implementation depends on specific ZK system
```

### ✅ Step 5.4 Implement System-Level Security Properties

- ✅ Double-spend prevention through single-owner rule
- ✅ Reentrancy prevention through Lamport clocks
- ✅ Traceability through complete audit trail
- ✅ Prevention of backdated transitions

```haskell
data SecurityProperty
    = NoDoubleSpend
    | NoReentrancy
    | FullTraceability
    | NoBackdating

verifySecurityProperty :: SecurityProperty -> ExecutionLog -> ResourceLedger -> TimeMap -> Either SecurityError ()
verifySecurityProperty NoDoubleSpend log ledger _ = verifyNoDoubleSpend log ledger
verifySecurityProperty NoReentrancy log _ _ = verifyNoReentrancy log
verifySecurityProperty FullTraceability log _ _ = verifyFullTraceability log
verifySecurityProperty NoBackdating log _ timeMap = verifyNoBackdating log timeMap
```

### ✅ Step 5.5 Final Cleanup

After implementing all security and invariant checks:

**Complete Deprecated Code Removal**:
- ✅ Remove all code previously marked as deprecated
- ✅ Ensure all modules use the new abstractions and patterns
- ✅ Verify that no implicit state updates remain in the codebase

**Documentation Update**:
- ✅ Update all documentation to reflect the new architecture
- ✅ Create migration guides for any external users of the codebase
- ✅ Document the security guarantees and how they're enforced

## ⏭️ Phase 6: Implement Distributed Execution

This phase focuses on implementing the distributed execution capabilities of the Time Bandits system, allowing it to run across multiple processes and machines.

### ✅ Step 6.1 Implement Actor Communication Protocol

Define the protocol for actor communication, including message formats, serialization, and addressing.

```haskell
-- Actor communication protocol
data ActorMessage
  = DeployProgram ProgramDefinition TimeMap
  | ExecuteTransition TransitionMessage
  | QueryState ProgramId
  | SystemControl SystemCommand
  | ActorDiscovery ActorQuery
  deriving (Show, Eq, Generic, Serialize)

-- Actor communication channel
data ActorChannel = ActorChannel
  { channelId :: ChannelId
  , channelType :: ChannelType
  , channelState :: ChannelState
  , remoteActor :: ActorAddress
  }

-- Actor addressing scheme
data ActorAddress
  = LocalAddress ProcessId
  | NetworkAddress HostName Port
  | UnixSocketAddress FilePath
  deriving (Show, Eq, Generic, Serialize)
```

### ✅ Step 6.2 Implement Local Multi-Process Mode

Implement the local multi-process mode, where actors run in separate processes on the same machine.

```haskell
-- Local multi-process configuration
data ProcessConfig = ProcessConfig
  { pcCommand :: Text
  , pcArgs :: [Text]
  , pcWorkingDir :: FilePath
  , pcEnvironment :: [(String, String)]
  , pcSocketPath :: FilePath
  }

-- Process management
startProcess :: ProcessId -> ProcessConfig -> IO ProcessState
stopProcess :: ProcessId -> ProcessState -> IO ()
monitorProcess :: ProcessId -> ProcessConfig -> ProcessState -> IO ProcessState

-- Inter-process communication
createSocket :: FilePath -> IO (Either Text Socket)
sendToProcess :: Serialize a => ProcessState -> a -> IO ()
receiveFromProcess :: Serialize a => ProcessState -> IO a
```

### 🔜 Step 6.3 Implement Geo-Distributed Mode

- 🔜 Use TCP/IP for network communication
- 🔜 Implement encryption and authentication for secure channels
- 🔜 Handle network partitions and reconnection
- 🔜 Support discovery and peer exchange

```haskell
-- | Network mode configuration
data NetworkConfig = NetworkConfig
  { networkRole :: ActorRole
  , networkBindAddress :: SocketAddr
  , networkBootstrapPeers :: [SocketAddr]
  , networkKeyPath :: FilePath
  }

-- | Connect to the distributed network
connectToNetwork :: NetworkConfig -> IO NetworkHandle
```

### 🔜 Step 6.4 Implement Distributed Execution Log

- 🔜 Create persistent storage for execution logs
- 🔜 Replicate logs across network nodes
- 🔜 Implement consensus for log ordering
- 🔜 Support verification of remote logs

```haskell
-- | Distributed log operations
class DistributedLog m where
  appendLogEntry :: LogEntry -> m (Either Error Hash)
  getLogEntry :: Hash -> m (Either Error LogEntry)
  verifyLogConsistency :: m (Either Error Bool)
  replicateLog :: [NodeId] -> m (Either Error ())
```

### 🔜 Step 6.5 Implement Network Resilience

- 🔜 Handle node failures and network interruptions
- 🔜 Implement leader election for coordination tasks
- 🔜 Support state recovery after downtime
- 🔜 Ensure execution continuity across disruptions

```haskell
-- | Network resilience features
recoverFromFailure :: NodeId -> m (Either Error ())
electCoordinator :: [NodeId] -> m (Either Error NodeId)
synchronizeState :: NodeId -> NodeId -> m (Either Error ())
```

## ⏳ Phase 7: Implement Timeline Descriptors & Adapters

This phase focuses on formalizing timeline interactions through descriptors and adapters, making the system more flexible and adaptable to different blockchains.

### ✅ Step 7.1 Implement Timeline Descriptors

Create a formal system for describing different timeline types (blockchains, rollups, event logs) with their specific properties and interfaces.

```haskell
-- | Timeline descriptor defining a timeline's properties
data TimelineDescriptor = TimelineDescriptor
  { tdId :: TimelineHash              -- ^ Unique timeline identifier
  , tdName :: ByteString              -- ^ Human-readable name
  , tdVmType :: VMType                -- ^ Virtual machine type
  , tdClockType :: ClockType          -- ^ Clock type
  , tdEndpoint :: EndpointConfig      -- ^ RPC endpoint configuration
  , tdEffectMappings :: Map EffectType EffectHandlerSpec  -- ^ Effect to handler mappings
  , tdMetadata :: Map ByteString ByteString  -- ^ Additional metadata
  }

-- | Virtual Machine types supported by timelines
data VMType
  = EVM        -- ^ Ethereum Virtual Machine
  | CosmWasm   -- ^ CosmWasm (Cosmos ecosystem)
  | MoveVM     -- ^ Move VM (Sui, Aptos)
  | Solana     -- ^ Solana Programs
  | Native     -- ^ Native Haskell implementation (off-chain)
  | MockVM     -- ^ Mock VM for testing

-- | Clock types for different timelines
data ClockType
  = BlockHeight   -- ^ Block number/height based
  | SlotNumber    -- ^ Slot number based
  | Timestamp     -- ^ Timestamp based
  | LamportClock  -- ^ Logical Lamport clock
```

### ✅ Step 7.2 Implement Timeline Adapters

Create adapters for different timeline types that provide a uniform interface while handling blockchain-specific implementation details.

```haskell
-- | Timeline adapter for interacting with a specific timeline
data TimelineAdapter = TimelineAdapter
  { taConfig :: AdapterConfig              -- ^ Adapter configuration
  , taState :: AdapterState                -- ^ Adapter state
  , taExecuteEffect :: Effect -> IO (Either AdapterError ByteString)  -- ^ Function to execute effects
  , taQueryState :: ByteString -> IO (Either AdapterError ByteString)  -- ^ Function to query state
  , taGetLatestHead :: IO (Either AdapterError BlockHeader)  -- ^ Function to get latest head
  }

-- | Create a timeline adapter from a descriptor
createAdapter :: 
  TimelineDescriptor -> 
  Maybe Manager -> 
  IO TimelineAdapter

-- | Execute an effect using the appropriate adapter
executeEffect :: 
  TimelineAdapter -> 
  Effect -> 
  IO ByteString
```

### 🔜 Step 7.3 Implement TOML-based Configuration

- 🔜 Create a TOML parser for timeline descriptors
- 🔜 Support loading and validating descriptor files
- 🔜 Generate adapters from descriptor files

```haskell
-- | Load a timeline descriptor from a TOML file
loadDescriptor :: FilePath -> IO (Either Error TimelineDescriptor)

-- | Parse a timeline descriptor from ByteString (TOML content)
parseDescriptor :: ByteString -> Either Error TimelineDescriptor

-- | Validate a timeline descriptor
validateDescriptor :: TimelineDescriptor -> Either Error Bool
```

### 🔜 Step 7.4 Integrate Adapters with Effect Interpreter

- 🔜 Modify the effect interpreter to use timeline adapters
- 🔜 Route effects to appropriate adapters based on timeline
- 🔜 Handle adapter errors and retries

```haskell
-- | Apply an effect using the appropriate timeline adapter
applyEffectWithAdapter ::
  Effect ->
  Map TimelineHash TimelineAdapter ->
  TimeMap ->
  IO (Either Error EffectResult)
```

### 🔜 Step 7.5 Implement Example Timeline Descriptors

- 🔜 Create example descriptors for common blockchains:
  - Ethereum Mainnet
  - Ethereum Sepolia Testnet
  - Solana Mainnet
  - Cosmos Hub
  - Local mock timelines

```toml
# Example Ethereum timeline descriptor
[timeline]
id = "ethereum-mainnet"
name = "Ethereum Mainnet"
vm_type = "EVM"
clock_type = "BlockHeight"

[endpoint]
primary = "https://mainnet.infura.io/v3/YOUR_API_KEY"
backups = ["https://eth-mainnet.alchemyapi.io/v2/YOUR_API_KEY"]
```

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
- ✅ SecurityVerifier.hs - System-level security property verification