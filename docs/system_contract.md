System Contract for Time Bandits
Version: 1.1
Date: 2025-03-07

## Purpose

This document defines the formal system contract for Time Bandits, a distributed system that allows time travelers to deploy and execute programs across multiple timelines (blockchains, ledgers, external event logs). It defines the invariants that every correct deployment and execution must respect, regardless of deployment mode (in-memory, local multi-process, or geo-distributed).

## Core Entities

### 2.1 Timeline

- A causally ordered event stream governed by its own rules.
- Defines its own consistency model (e.g., blockchains guarantee no double spends).
- Owns its own clock (height, slot, or timestamp).

## 2.2 Time Map

- A sharded view of the state of all timelines a program can see.
- Includes:
  - Timeline heads (block heights or slots).
  - Observed timestamps (external clock).
  - Logical time (Lamport clock) per timeline.

2.3 Resource

- A unit of state that programs can create, transfer, destroy, or hold.
- Exists inside program memory.
- Has a unique owner program at any time.

2.4 Asset

- An external object (token, escrow, contract state) that exists directly on a timeline.
- Programs do not directly own assets — they own claims on assets (resources).

## Program Model

### 3.1 Programs

- Immutable after deployment.
- Execute a declared sequence of effects, each guarded by explicit preconditions.
- Have private memory to store resources.
- Interact with other programs only via explicit transfers, escrows, and invocation effects.

### 3.2 Effects

- Atomic state transitions.
- All effects are applied via an effect interpreter.
- All effects reference the current time map, and are invalid if causal order is violated.

## Program Lifecycle

### 4.1 Deployment

- Time Traveler submits:
  - Program definition.
  - Initial time map.
- Time Keeper validates:
  - Program is well-formed.
  - Time map is fresh and consistent with timeline state.
- Controller (operated by Time Bandits) computes:
  - Memory contract (expected resource flow per step).
  - Initial program state.
- Program is registered on the network and with relevant timelines.

### 4.2 Execution

- Time Traveler submits a TransitionMessage that:
  - Proves control over required resources.
  - Provides a ZK proof that validates the previous effect's guard conditions.
- Time Keeper:
  - Validates message against timeline rules.
  - Accepts or rejects the transition.
- Time Bandit:
  - Executes the program step.
  - Generates required proofs.
  - Maintains P2P network state.
- Controller:
  - Verifies the message.
  - Applies the effect.
  - Updates program state and memory.
  - Updates time map (advancing Lamport clocks and observed heads).
  - Logs the transition to an append-only execution log.

### 4.3 Finalization

- Programs complete when all effects are applied.
- Final memory state is sealed.
- Time Bandits generate final proof.
- Time Keepers validate and confirm the final state.
- Final execution log and proofs are published to all relevant timelines.

## Actors

The Time Bandits system includes three specialized actor roles:

### 5.1 Time Travelers

- Create and use programs by submitting messages to timelines
- Have the ability to:
  - Deploy new programs with initial time maps
  - Submit TransitionMessage objects to advance program state
  - Query the current state of programs and resources
- Each Time Traveler has:
  - An ActorID (derived from public key)
  - A capability set (permissions for specific effects)
  - Access to one or more timelines

### 5.2 Time Keepers

- Maintain the integrity of Timelines
- Responsible for:
  - Accepting messages based on validity requirements
  - Rejecting invalid or malicious requests
  - Providing timeline state queries
  - Ensuring timeline consistency and immutability
- Control access to timelines and enforce their specific rules
- Act as the trusted authority for timeline state

### 5.3 Time Bandits

- Operate the P2P network infrastructure
- Responsible for:
  - Program execution in the specified simulation mode
  - Generating cryptographic proofs required for transitions
  - Maintaining the distributed execution logs
  - Facilitating communication between system components
- Can represent:
  - Nodes in the P2P network
  - Service providers for specific simulation modes
  - Infrastructure operators

### 5.4 General Actor Properties

- Every actor has:
  - An ActorID (public key derived).
  - A role (e.g., Trader, PriceFeed, Oracle).
- Actors may represent:
  - Off-chain users.
  - On-chain contracts.
  - Autonomous agents (pre-registered programs).

## Messaging

### 6.1 TransitionMessage Every program transition is triggered by a message from an actor:

```haskell
data TransitionMessage = TransitionMessage { programId :: ProgramId , stepIndex :: Int , parentEffectHash :: Hash , proof :: ZKProof , resources :: [Resource] }
```

- `parentEffectHash` links every message to the causal parent (previous applied effect).
- proof proves:
  - All guards held at time of application.
  - Resources were correctly transferred.
- Controller or actor framework must validate this message.

### 6.2 Messaging in Simulation Modes

- Mode: In-Memory
  - Transport: Direct Haskell function calls

- Mode: Local Multi-Process
  - Transport: stdin/stdout or Unix sockets

- Mode: Geo-Distributed
  - Transport: TCP, SSH, or external RPC

## Ownership and Resource Flow

- Each resource has exactly one owner program at any time.
- Resources move between programs only via:
  - EscrowToProgram
  - ClaimFromProgram
  - InvokeProgram (with arguments)
- No program can modify another program's memory directly.
- All cross-program transfers leave a trace record in the execution log.

## Time and Causality

- Every timeline has:
  - Native clock (height/slot/timestamp)
  - Logical clock (Lamport counter)
- Every program has:
  - Per-step program counter (internal Lamport clock)
- Every effect can only apply if:
  - Its guards are satisfied in the current time map.
  - The time map shows each timeline advancing monotonically.
  - Cross-timeline events respect the time map's logical clock order.
- The time map is always:
```haskell
data TimeMap = TimeMap { timelines :: Map TimelineId LamportClock , observedHeads :: Map TimelineId BlockHeader , observedTimestamps :: Map TimelineId UTCTime }
```

- Time travelers cannot backdate effects by presenting stale time maps.

## Execution Log
- Every applied effect produces a log entry:
```haskell
data LogEntry = LogEntry { effect :: Effect , appliedAt :: UTCTime , causalParent :: Hash , resultingStateHash :: Hash }
```

- Every log entry:
  - Is content-addressed.
  - Links to its parent.
  - Logs are fully replayable.

## Controller Responsibilities
- Regardless of simulation mode, the controller enforces:
  - Initial deployment validation.
  - Time map tracking (observed heads, Lamport clocks, timestamps).
  - Transition message validation (proof, signature, resources).
  - Effect application.
  - Memory updates.
  - Causal log appending.
  - Final state sealing.
  - The controller is the only trusted component for enforcing the system contract — actors can be untrusted, as all their messages are verified.

## Simulation Modes and Controller Behavior

- In-Memory Mode
  - All actors are Haskell functions in the same process.
  - Controller invokes actors directly.
  - Messaging is via in-memory queues.
- Local Multi-Process Mode
  - Each actor runs in its own nix run process.
  - Controller spawns actors, then sends/receives messages via Unix sockets.
  - Execution log lives on disk (per process).
- Geo-Distributed Mode
  - Each actor runs on a remote machine (via nix run over SSH).
  - Controller sends/receives messages via TCP or external RPC.
  - Execution log is replicated to a distributed log store (optional).

## Invariants

Invariant	Scope
Timeline Consistency	Each timeline obeys its own rules (imported directly).
Cross-Timeline Isolation	Programs only see declared timelines.
Monotonic Time	Programs observe strictly advancing time maps.
Single Owner	Each resource has exactly one owner at a time.
No Implicit Access	Programs only modify memory via declared effects.
Replay Determinism	Replaying a program's effects with the same inputs gives the same output.
Causal Linking	Every effect is causally linked to its predecessor.
Complete Audit Trail	Every applied effect has a logged proof.
Proof-Carrying Transitions	Every transition carries a ZK proof of correctness.

## Security Properties

- Double spend prevention: Single-owner rule and causal linking prevent double claims.
- No reentrancy: Lamport clocks prevent cycles.
- Full traceability: Every resource movement is logged and provable.
- No backdated transitions: Time map enforcement prevents stale writes.

## Appendices: Example Flow

- Example Cross-Program Trade
  - Program A escrows 100 GoldCoin into Program B.
  - Program B detects escrow, watches for SilverCoin.
  - Program B invokes Program C to finalize.
  - Program C claims GoldCoin from B, completes trade.
  - All 5 transitions leave trace records with proofs.
  - Controller updates time map at each step.