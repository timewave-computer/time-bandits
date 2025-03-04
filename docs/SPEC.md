# Time Bandits System Specification
Version: 1.1
Date: 2025-03-07

## 1. Overview

Time Bandits is a system for deploying and executing cross-timeline programs that enforce strict causal and resource ownership guarantees. Programs execute over a sharded time map (a composite view across multiple timelines) and produce content-addressed, fully auditable logs of their execution.

The system is built around three key actor roles:

- **Time Travelers**: Create and use programs by submitting messages to timelines
- **Time Keepers**: Maintain the integrity of timelines, validate messages, and provide state queries
- **Time Bandits**: Operate the P2P network, execute programs, and generate cryptographic proofs

This document defines the technical specification for programs, timelines, resources, effects, messaging, simulation modes, and the responsibilities of each actor role.

## 2. Core Entities

### 2.1 Timeline

- An external, ordered event stream (blockchain, rollup, DA log).
- Defines its own consistency and validity rules (e.g., no double spends).
- Owns:
  - Native time (height/slot/timestamp).
  - Native assets (tokens, contracts, escrows).

### 2.2 Time Map

- A sharded snapshot of multiple timelines at a specific moment.
- Contains:
  - Timeline heads (block heights/slots).
  - Observed timestamps (real time).
  - Lamport clocks (logical causal order).
- Programs only interact with the time map they were deployed with.

### 2.3 Asset (External Object)

- Exists directly on a timeline (ETH, USDC, on-chain contract state).
- Time Bandits does not custody assets — programs own claims on assets via internal resources.

### 2.4 Resource (Internal Object)

- A unit of state inside program memory.
- Always has exactly one owner program at a time.
- Can represent:
  - A claim on an external asset.
  - An escrow receipt.
  - A synthetic internal marker.

### 2.5 Program

- An immutable, replayable sequence of guarded effects.
- Owns its own memory (resource slots).
- Operates within a time map.

### 2.6 Effect

- A single, atomic state transition.
- May consume or produce resources.
- May transfer resources across programs.
- May invoke other programs.
- Is causally ordered via a log.

## 3. Program Lifecycle

### 3.1 Deployment

- Time Traveler submits:
  - Program definition (effects and guards)
  - Initial time map (declared timelines)
- Time Keeper validates:
  - Program is well-formed
  - Time map is fresh and consistent with timeline state
- Controller (operated by Time Bandits) computes:
  - Memory contract (expected resource flow per step)
  - Initial program state
- Program is registered on the network and with relevant timelines

### 3.2 Execution

- Time Traveler submits:
  - TransitionMessage with:
    - Effect proof
    - Resources
    - Parent effect hash
- Time Keeper:
  - Validates message against timeline rules
  - Accepts or rejects the transition
- Time Bandit:
  - Executes the program step
  - Generates required proofs
  - Maintains P2P network state
- Controller:
  - Validates message
  - Applies effect
  - Advances time map
  - Appends to causal log

### 3.3 Finalization

- Programs complete after the last effect
- Final memory state is sealed
- Time Bandits generate final proof
- Time Keepers validate and confirm the final state
- Final proof and log are published to all relevant timelines

## 4. Actors and Roles

The Time Bandits system includes three specialized actor roles:

### 4.1 Time Travelers

- Create and use programs by submitting messages to timelines
- Have the ability to:
  - Deploy new programs with initial time maps
  - Submit TransitionMessages to advance program state
  - Query the current state of programs and resources
- Each Time Traveler has:
  - ActorID (derived from public key)
  - Capability set (permissions for specific effects)
  - Access to one or more timelines

### 4.2 Time Keepers

- Maintain the integrity of Timelines
- Responsible for:
  - Accepting messages based on validity requirements
  - Rejecting invalid or malicious requests
  - Providing timeline state queries
  - Ensuring timeline consistency and immutability
- Control access to timelines and enforce their specific rules
- Act as the trusted authority for timeline state

### 4.3 Time Bandits

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

### 4.4 General Actor Properties

- All actors have:
  - ActorID (public key derived)
  - Capability set (what effects they can trigger)
- Actors might be:
  - Off-chain users (wallets)
  - On-chain contracts (proxy actors)
  - Oracles (time map feeds)

## 5. Effects

5.1 Effect Types

- EscrowToProgram (transfer resource to another program).
- ClaimFromProgram (recover escrowed resource from another program).
- InvokeProgram (call another program's entry point with arguments).
- DelegateCapability (grant temporary rights to another program).
- WatchResource (react to external event or resource condition).
- AtomicBatch (group multiple effects into a single atomic unit).

- All effects:
  - Reference a time map snapshot.
  - Leave a trace log entry.

### 5.2 Effect Precondition (Guard)

- Every effect has an explicit guard:
  - BalanceAtLeast (check resource balance).
  - EscrowExists (check escrow presence).
  - ContractInState (check external contract state).
  - ActorAuthorized (check actor permissions).
  - TimeAfter (check timeline time).
  - Always (no guard).

## 6. Program Memory and Ownership

- Each program has:
  - A private ProgramMemory.
  - Each step's memory contract (expected resource layout).
  - Every resource has exactly one owner program at any time.
- All inter-program transfers must use:
  - EscrowToProgram
  - ClaimFromProgram
  - InvokeProgram

## 7. Time and Causality

### 7.1 Per-Timeline Time

- Native time (height/slot/timestamp).
- Logical time (Lamport clock).

### 7.2 Per-Program Time

- Program Counter (internal Lamport clock).

### 7.3 Time Map

- Every effect applies against a time map:
  - Contains per-timeline clocks.
  - Advances as new transitions apply.
  - Is always causally consistent.

## 8. Messaging

### 8.1 Transition Message
- All program advancement comes from signed messages: 
```haskell
TransitionMessage
programId
stepIndex
parentEffectHash
proof
resources
```

- parentEffectHash ensures causal linking.
- proof ensures:
  - Guard satisfaction.
  - Correct resource flow.
  - Message transport depends on simulation mode.

## 9. Execution Log

- Every effect produces a log entry:
```haskell
LogEntry
effect
appliedAt
causalParent
resultingStateHash
```

- Log is:
  - Content-addressed.
  - Append-only.
  - Causally linked.

## 10. Controller

The Controller is a core system component operated by Time Bandits and is responsible for:

- Deploying programs based on Time Traveler submissions
- Tracking time maps across all relevant timelines
- Validating transitions submitted by Time Travelers
- Applying effects after validation by Time Keepers
- Maintaining causal logs for replay and audit
- Ensuring system invariants across all operations
- Coordinating with Time Keepers for timeline state updates

The Controller acts as the central coordination point between Time Travelers who submit programs and messages, Time Keepers who maintain timeline integrity, and Time Bandits who execute programs and generate proofs. It ensures that all system operations maintain the required security and consistency properties regardless of the deployment mode.

## 11. Simulation Modes

### 11.1 In-Memory Mode

- All actors run in-process.
- Messaging is direct function calls.

### 11.2 Local Multi-Process Mode

- Each actor runs in its own process.
- Messaging is via Unix sockets or stdin/stdout.
- Controller spawns actors via nix run.

### 11.3 Geo-Distributed Mode

- Each actor runs remotely (e.g., nix run over SSH).
- Messaging uses TCP or external RPC.
- Controller manages distributed coordination.

## 12. Invariants

- Timeline Consistency: Each timeline obeys its own rules.
- Cross-Timeline Isolation: Programs only see declared timelines.
- Monotonic Time: Programs observe strictly advancing time maps.
- Single Owner: Each resource has exactly one owner at any time.
- No Implicit Access: Programs only modify memory via declared effects.
- Replay Determinism: Replay with the same inputs yields the same outputs.
- Causal Linking: Every effect links to its parent.
- Complete Audit Trail: Every applied effect is logged.
- Proof-Carrying Transitions: Every transition carries a ZK proof.

## 13. Security Properties

- Double Spend Prevention: Enforced by single-owner rule.
- No Reentrancy: Enforced by Lamport clocks.
- Full Traceability: Enforced by content-addressed logs.
- No Backdating: Enforced by time map checking.

## 14. Example Flow

- Cross-Program Escrow Trade:
  1. Program A escrows 100 GoldCoin into Program B.
  2. Program B watches for SilverCoin deposit.
  3. Program B invokes Program C to finalize.
  4. Program C claims GoldCoin from B and completes trade.
  5. Each step emits a trace log entry with a proof.
  6. Time map advances at each step.

## 15. System Diagram

```
+-----------------------------------------------------------+
|                    Time Bandits System                     |
+-----------------------------------------------------------+
           |                |                 |
    +------+------+  +------+------+  +-------+-------+
    | Time Travelers|  | Time Keepers |  |  Time Bandits  |
    +------+------+  +------+------+  +-------+-------+
           |                |                 |
           v                v                 v
    +-------------+  +-------------+  +----------------+
    |   Programs  |  |  Timelines  |  |   Controller   |
    +------+------+  +------+------+  +-------+-------+
           |                |                 |
           v                v                 v
    +--------------+ +---------------+ +----------------+
    |Program Memory| |Timeline State | | Execution Log  |
    +--------------+ +---------------+ +----------------+
                                       |
                                       v
                                 +-------------+
                                 |  Time Map   |
                                 +-------------+
```

- Time Travelers submit program definitions and transition messages to advance program state
- Time Keepers maintain timeline integrity, validate messages, and ensure timeline consistency
- Time Bandits operate the P2P network, execute programs, and generate required cryptographic proofs
- Controller validates messages, applies effects, and maintains the execution log
- Programs own memory slots and resources with explicit ownership tracking
- Resources move explicitly between programs through controlled effects
- All operations maintain a consistent time map across timelines
- All changes are logged and proof-checked in the execution log