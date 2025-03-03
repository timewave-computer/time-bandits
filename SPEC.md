Time Bandits — Full System Specification
Version: Draft 1.1 (Comprehensive)
Date: 2025-03-02

## 1. Core Entities

### 1.1 Timeline

- A causally ordered event log.
- May be a blockchain, rollup, off-chain log, or data availability layer.
- Defines its own consistency guarantees (e.g., no double spends, ordered blocks).
- Owns its own clock (which could be block height, slot number, or timestamp).

Timeline Responsibilities

- Define how time advances.
- Define how resources are created, transferred, destroyed.
- Define how external programs authenticate against the timeline.
Provide access to its own causal log.

### 1.2 Resource

An internal program-owned representation of an external asset or a synthetic program-specific value that lives in program memory.

- Created, moved, consumed within a program's memory
- Assigned to a memory slot inside a program
- Scoped to the program's lifecycle
- Pointed to directly in program memory

Examples:

- Token balance resources
- Escrow receipt resources
- Contract witness resources
- Synthetic internal markers

```haskell
data Resource
    = TokenBalanceResource TokenId Address Amount
    | EscrowReceiptResource EscrowId
    | ContractWitnessResource ContractId Value
    | SyntheticInternalMarker Text
```

### 1.3 Asset

An external entity that exists independently on a timeline.

- Exists independently on a timeline
- Owned by an address on the timeline
- Persistent across all programs
- Resolved via queries into the timeline

Examples:

- Tokens (e.g., ETH, USDC)
- Escrow records
- Smart contracts
- Files on a data availability chain
- Reputation credentials

```haskell
data AssetRef
    = TokenAsset TimelineId TokenId
    | EscrowAsset TimelineId EscrowId
    | ContractAsset TimelineId ContractId
```

### 1.4 Time Map

- A sharded view across multiple timelines.
- Declared at program deployment.
- Immutable once declared.
- Provides a read-only window into each timeline's causal events and resource states.

### 1.5 Actor

- External entities (humans, DAOs, programs) that:
  - Deploy programs.
  - Trigger program state transitions by submitting `TransitionMessage`.

Each actor has:

- An Address.
- A CapabilitySet that defines what they can do.
- A cryptographic key for signing messages.

Actors may represent:

- Off-chain users (signing with wallets).
- On-chain contracts (acting as proxy actors).
- Other programs (acting as higher-level automation).

## 2. Program Model

### 2.1 Program

- A declarative state machine that operates within a time map.
- Consists of:
  - Program Definition: Immutable sequence of `GuardedEffect`.
  - Memory Contract: Immutable declaration of per-step resource expectations.
  - Program State: Current execution counter and memory.
- Immutable after deployment.
- Fully replayable.
- Execution is provable with a ZK proof.

### 2.2 Guarded Effect

- An effect (state transition) paired with a guard (precondition).

```haskell
data GuardedEffect = GuardedEffect Guard Effect
```

### 2.3 Effect

- A primitive state transition.
- Operates on resources in program memory, not directly on assets.
- May result in timeline instructions for asset operations.
- Always emits a trace record.

```haskell
data Effect
    = DepositToMemorySlot MemorySlot Resource
    | WithdrawFromMemorySlot MemorySlot Resource
    | TransferBetweenSlots MemorySlot MemorySlot Resource
    | CreateToken TokenId Amount  // Results in timeline instruction
    | DestroyToken TokenId Amount  // Results in timeline instruction
    | TransferToken TokenId Amount Address Address  // Results in timeline instruction
    | CrossTimelineCall TimelineId Effect
    | LogDiagnostic Text
```

### 2.4 Guard

- Preconditions that must hold before an effect is allowed to apply.

```haskell
data Guard
    = BalanceAtLeast TokenId Address Amount
    | EscrowExists EscrowId
    | ContractInState ContractId Value
    | ActorAuthorized Address Capability
    | TimeAfter Timestamp
    | Always
```

## 3. Program Lifecycle

### 3.1 Deployment

- Actor submits:
  - Program Definition
  - Time Map
- System computes:
  - Memory Contract
  - Program is registered immutably.

### 3.2 Execution

- Actor submits:
  - `TransitionMessage` for current step.

System verifies:
  - Proof (correct ownership of required resources).
  - Guard conditions (using the time map).
- System applies effect, advances program counter, updates memory.
- System emits `EventTrace`.

### 3.3 Finalization

- Last effect completes.
- Program is marked complete and sealed.

## 4. Memory Model

### 4.1 Memory Slots

- Named containers for resources within program memory.
- Each slot can hold a specific type of resource.
- Slots are referenced by program effects for resource operations.

```haskell
data MemorySlot = MemorySlot Text
```

### 4.2 Program Memory

- Tracks actual runtime state of program-owned resources.
- Provides a consistent interface for all resource operations.
- Maps memory slots to resources.

```haskell
data ProgramMemory = ProgramMemory
    { slots :: Map MemorySlot Resource
    }
```

Example memory layout:
```
Slot 0: TokenBalanceResource "GoldCoin" "Alice" 100
Slot 1: EscrowReceiptResource "TradeEscrow1"
Slot 2: SyntheticInternalMarker "Program entered Phase 2"
```

### 4.3 Memory Contract

- Immutable, declared at deployment.
- Derived from program definition.
- Specifies required resources for each program step.

## 5. Messages

### 5.1 Transition Message
- Actor submits this to advance a program.

```haskell
data TransitionMessage = TransitionMessage
    { programId :: ProgramId
    , stepIndex :: Int
    , proof :: ZKProof
    , resources :: [Resource]
    }
```

## 6. Proof System

### 6.1 ZK Proof

- Every transition produces a proof that:
  - Guards were satisfied.
  - Resources were correctly moved.
  - No unauthorized state was touched.

## 7. Observability

### 7.1 Event Trace

Every applied effect emits:

```haskell
data EventTrace = EventTrace
    { programId :: ProgramId
    , stepIndex :: Int
    , effect :: Effect
    , appliedBy :: Address
    , consumed :: [Resource]
    , produced :: [Resource]
    , zkProofHash :: Hash
    }
```

### 7.2 Explanation

- Given the program definition, memory contract, time map, and message log, the system can produce:
  - Full causal replay.
  - Audit trail for each transition.
  - Static linting for program validity.

## 8. Governance

### 8.1 Program Registry

- All deployed programs are immutable entries in the registry.
- Registry is indexed by:
  - `ProgramId`
  - `Actor`
  - `Time Map`

## 8.2 Time Map Registry

- Each timeline is explicitly registered, with:
  - Consistency rules.
  - Resource types it supports.
  - Block explorers, watchers, oracles (for external visibility).

## 9. Cross-Timeline Coordination

### 9.1 Time Map Imports
- Programs only see and touch declared timelines.
- Cross-timeline effects require explicit `CrossTimelineCall`.

## 9.2 Oracles

- External agents that bring cross-timeline receipts into a program's memory.
- Oracles must be authorized in the memory contract.

## 10.  System Invariants

| Invariant | Scope | Enforced When |
| --- | --- | --- |
| Timeline Consistency Inheritance | All programs | On timeline read/write |
| Cross-Timeline Isolation | All programs | On import & execution |
| Causal Execution Order | All programs | On every effect |
| Resource Conservation | Tokens & escrows | On every effect |
| Memory Contract Honesty | All programs | On every transition |
| No Backwards Time | All programs | On every timeline access |
| ZK Proof for All Steps | All programs | After every step |
| Program Immutability | All programs | After deployment |
| Operator Neutrality | Whole system | Always |
| Replay Determinism | Whole system | Always |
| Asset Memory Encapsulation | All programs | On all asset operations |

## 11. Error Handling

### 11.1 Static Rejection

- Invalid program definitions are rejected at deploy time.
- Memory contract must match program effects.

### 11.2 Execution Halts

- Failed transitions leave programs halted but retriable.
- Invalid messages are rejected with trace.

## 12. Static Analysis & Linting

### 12.1 Property Checks

- Each program must statically pass:
  - No double spend within a timeline.
  - No touching undeclared resources.
  - No backwards time travel.
  - Completeness: Each step fully defines guards/resources/effect.

## 13. Component Summary Table

| Component | Description | Owned By |
| --- | --- | --- |
| Timeline | External event log | External system |
| Asset | External token, escrow, contract | Timeline |
| Resource | Internal program-owned representation | Program memory |
| Time Map | Sharded view into timelines | Time Bandits |
| Program | Immutable state machine | Time Bandits |
| Effect | Atomic transition | Time Bandits |
| Guard | Preconditions | Time Bandits |
| Memory | Per-program resource space | Time Bandits |
| Message | External trigger | Actor |
| Proof | Execution attestation | Time Bandits |
| Trace | Execution audit | Time Bandits |

## 14. P2P System

### 14.1 Purpose
The P2P layer provides:

- Distributed synchronization of timeline heads.
- Discovery and distribution of program definitions and transition messages.
- Propagation of event traces.
- Peer gossip about detected anomalies (clock drift, double spends, forks).

### 14.2 Peer Identity
Every peer has a cryptographic identity:

```haskell
data PeerId = PeerId PublicKey
```

### 14.3 Peer Capabilities
Each peer declares its capabilities, e.g.:

```haskell
data PeerCapabilities = PeerCapabilities
    { canObserveTimelines :: Set TimelineId
    , canExecutePrograms :: Bool
    , hasArchive :: Bool
    }
```

### 14.4 P2P Messages

```haskell
data PeerMessage
    = TimelineHeadUpdate TimelineId BlockHeader
    | ProgramAnnouncement ProgramId ProgramMetadata
    | TransitionRelay TransitionMessage
    | EventTraceRelay EventTrace
    | AnomalyReport TimelineId Anomaly
```

### 14.5 Anomaly Detection
Peers actively check for:

- Clock drift across observers of the same timeline.
- Fork detection (multiple competing heads).
- Missing causal events (gaps in event logs).

```haskell
data Anomaly
    = ClockSkewDetected TimelineId NominalDiffTime
    | ForkDetected TimelineId BlockHash BlockHash
    | EventGap TimelineId EventId EventId
```

### 14.6 Peer Synchronization
Each peer maintains a peer map of known peers, with:
- Their PeerId
- Their declared capabilities
- Their recent activity (liveness tracking)

```haskell
data PeerInfo = PeerInfo
    { peerId :: PeerId
    , capabilities :: PeerCapabilities
    , lastSeen :: Timestamp
    }
```

## 15. Transient Data Store (Cache Layer)

### 15.1 Purpose
The transient store is:

- A local, append-only content-addressed store.
- Used to cache:
  - Recently seen blocks.
  - Program definitions.
  - Transition messages.
  - Event traces.
  - Partial time maps.

### 15.2 Addressing
All transient data is content-addressed by hash:

```haskell
newtype ContentHash = ContentHash ByteString
```

### 15.3 Data Types Stored

```haskell
data TransientObject
    = CachedBlock TimelineId BlockHeader
    | CachedProgram ProgramId ProgramDefinition MemoryContract
    | CachedTransition TransitionMessage
    | CachedEventTrace EventTrace
    | CachedPartialTimeMap TimeMap
```

### 15.4 Transient Store Operations

```haskell
putTransient :: TransientObject -> IO ContentHash
getTransient :: ContentHash -> IO (Maybe TransientObject)
pruneTransient :: IO ()
```

Pruning is time + relevance-based — e.g., after a program finalizes, its transitions can be pruned after some grace period.

## 16. Content-Addressed Hash-Linked Logs

### 16.1 Purpose
Immutable, append-only logs that capture:
- Observed timeline events (ingress log).
- Program execution traces (execution log).
- Anomaly reports (diagnostic log).
- Each log entry is linked to its parent via hash.

### 16.2 Log Entry

```haskell
data LogEntry
    = TimelineEvent TimelineId BlockHeader Event
    | ProgramExecutionTrace EventTrace
    | PeerAnomalyDetected Anomaly
    deriving (Generic)

data HashLinkedLog = HashLinkedLog
    { entry :: LogEntry
    , parent :: Maybe ContentHash
    }
```

### 16.3 Log Integrity
Each log forms a merkle chain.
Peers can exchange:

```haskell
getLogHead :: TimelineId -> IO ContentHash
```

Peers can verify:

```haskell
verifyLogConsistency :: ContentHash -> ContentHash -> IO Bool
```

### 16.4 Log Types

| Log | Scope | Notes |
| --- | --- | --- |
| Timeline Ingress Log | Per timeline | Raw event feed |
| Execution Log | Per program | Traces every applied effect |
| Diagnostic Log | Per peer | Anomalies, drift, forks |

## 17. Clocks

### 17.1 Purpose
- Tracks time perception per peer and per timeline.
- Peers report their local wall-clock and observed timeline timestamps to each other.
- This powers distributed clock drift detection.

### 17.2 Local Clock

```haskell
data LocalClock = LocalClock
    { wallClock :: UTCTime
    , monotonicClock :: NominalDiffTime
    }
```

### 17.3 Timeline Clock Perception

```haskell
data TimelineClock = TimelineClock
    { timelineId :: TimelineId
    , observedHead :: BlockHeader
    , observedTimestamp :: UTCTime
    , perceivedDrift :: NominalDiffTime
    }
```

### 17.4 Clock Drift Detection
When receiving new timeline heads from peers, each peer compares:

- The announced block timestamp.
- The local peer's own perception of that block's timestamp.
- If drift exceeds threshold:

```haskell
reportAnomaly :: TimelineId -> Anomaly
```

### 17.5 Time Sync Protocol
Peers exchange:

```haskell
data PeerClockReport = PeerClockReport
    { peerId :: PeerId
    , reportedAt :: UTCTime
    , observedTimelineClocks :: [TimelineClock]
    }
```

This powers:
- Detection of peers with faulty clocks.
- Detection of upstream oracles with inconsistent timestamps.

## 18. Extended Data Flows

### 18.1 Timeline Event Ingress
1. Peer observes new block.
2. Peer logs TimelineEvent in ingress log.
3. Peer updates transient store with CachedBlock.
4. Peer updates timeline head in time map.
5. Peer announces new head to neighbors.

### 18.2 Program Execution
1. Peer receives TransitionMessage.
2. Peer verifies message, updates memory.
3. Peer applies effect.
4. Peer emits ProgramExecutionTrace.
5. Peer updates program state in transient store.
6. Peer gossips trace to neighbors.

### 18.3 Anomaly Detection
1. Peer notices clock skew (timeline clock vs wall clock).
2. Peer emits PeerAnomalyDetected to diagnostic log.
3. Peer relays anomaly to neighbors.

## 19. Cross-Component Relationships

| Component | Writes To | Reads From | Notes |
| --- | --- | --- | --- |
| Timeline Watcher | Transient Store, Ingress Log | Peer Network | Observes blocks |
| Executor | Transient Store, Execution Log | Transition Messages | Runs programs |
| Peer Network | All Logs, Transient Store | Peer Messages | Synchronization |
| Anomaly Detector | Diagnostic Log | Peer Clocks | Monitors drift |

## 20. Extended Invariants

| Invariant | Applies To | Notes |
| --- | --- | --- |
| No Drift Beyond Tolerance | Peers | Max drift per timeline |
| Causal Log Consistency | All Logs | Every entry links correctly |
| No Fork Ambiguity | Per Timeline | Each timeline has exactly one head |
| Content Address Integrity | All Transient Data | Hash matches payload |
| Program Determinism | Execution Log | Same program replays identically |

## 21. System Diagram (Expanded)

```
                      [ Timeline A ]
                           |
            +--------------+--------------+
            | Peer A       Peer B       Peer C
            | (Log, Cache)  (Log, Cache)  (Log, Cache)
            |              |              |
   [P2P Network]----> Peer Messages (heads, transitions, traces)

                 [Execution Engine]
                         |
               Applies Effects -> [Execution Log]
                         |
                     [Programs]
                    (Definition + Memory)

           [Diagnostics] --> [Anomaly Log]
```