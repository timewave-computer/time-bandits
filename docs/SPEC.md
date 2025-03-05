# Time Bandits System Specification
Version: 1.2
Date: 2025-03-05
# Time Bandits Specification

## Overview

Time Bandits is a system for defining, executing, and verifying cross-timeline programs — programs that interact with multiple independent blockchains, rollups, and distributed ledgers, while maintaining causal consistency, auditability, and strong security guarantees.

This specification defines:
- Core data structures
- Actor and program boundaries
- Effect lifecycle
- Resource management
- Causal consistency rules
- Messaging model
- Proof requirements
- Simulation environments

## Core Abstractions

### Time Travelers

Time Travelers are external actors that create programs, initiate deposits, and invoke program functions.  
Travelers do not directly own resources. Instead, each traveler has exactly one Account Program, which acts as their gateway into the system.

### Programs

Programs are verifiable off-chain state machines, defined by:
- A static code (list of effects it supports).
- A mutable memory (key-value store).
- A set of owned resources (assets held on behalf of the program).
- A set of causal logs recording every applied effect.

All changes to program state — internal memory updates, resource transfers, or cross-program messaging — must occur via proven effects.

### Account Programs

Each traveler has a dedicated account program, which:
- Holds all the traveler’s resources across timelines.
- Acts as the only entry/exit point for the traveler to interact with other programs.
- Provides a unified inbox/outbox for messages between the traveler and programs.
- Can be composed into other programs (multi-sig management, delegation, etc.).

Programs only communicate with account programs — not directly with off-chain actors.

### Time Keepers

Each supported timeline (e.g., Ethereum, Celestia) has a Time Keeper, responsible for:
- Observing new timeline events.
- Maintaining a time map — a snapshot of each timeline’s latest state.
- Providing proofs of inclusion and balances on request.
- Ensuring external facts referenced by programs are correct.

### Time Bandits

Time Bandits are the distributed peer-to-peer network responsible for:
- Receiving proposed effects from travelers.
- Applying effects to programs.
- Checking causal consistency.
- Updating the resource ledger.
- Recording applied effects into per-resource logs.
- Generating and verifying proofs.
- Gossiping applied effects to other Bandits.

## Data Structures

### Effect

Effects are the atomic unit of state change. Each effect:
- Declares its preconditions.
- Declares the resources it reads/writes.
- References a specific time map snapshot.
- Produces a proof after application.

```haskell
data Effect
    = Deposit { resource :: ResourceId, amount :: Integer, toProgram :: ProgramId }
    | Withdraw { resource :: ResourceId, amount :: Integer, fromProgram :: ProgramId }
    | Transfer { resource :: ResourceId, amount :: Integer, toProgram :: ProgramId }
    | InternalStateUpdate { key :: Text, newValue :: Value }
    | Invoke { targetProgram :: ProgramId, entrypoint :: String, arguments :: [Value] }
    | ReceiveCallback { fromProgram :: ProgramId, payload :: Value }
```

### ProposedEffect

Each traveler submits effects for approval, bundling:

- The effect itself.
- The time map snapshot observed when the effect was proposed.
ResourceLedger

This tracks who owns each resource at all times.

```haskell
type ResourceLedger = Map ResourceId (ProgramId, ResourceState)
```

### Time Map

The Time Map tracks:

- The latest observed block height for each timeline.
- The corresponding block hashes and timestamps.
- A Lamport clock tying together all timeline updates.

### Execution Log

Each resource has its own causal log — a content-addressed append-only log recording every applied effect that touched that resource.

### Program Memory

Each program has a simple key-value store for internal state:

```haskell
type ProgramMemory = Map Text Value
```

## Concurrency Model

### Resource-Centric Concurrency

Effects are the unit of concurrency, not programs.
Effects apply in parallel if they touch disjoint resources.
- Each effect locks the resources it touches.
- If locks are free, the effect applies immediately.
- If locks are held, the effect waits.

### Per-Resource Logs

Each resource tracks its own causal log.
A program’s full history is assembled from its per-resource logs plus internal memory updates.

### Time Map Consistency

Every effect records the exact time map snapshot it observed when proposed.
Before applying, the interpreter compares this with the current time map:

- If the time map advanced, preconditions are rechecked.

## Messaging Model

All actor-initiated messages flow through Account Programs.

- Deposit: Move assets into a target program.
- Withdraw: Retrieve assets back into the account.
- Invoke: Call another program.
- ReceiveCallback: Receive async response from a program.

This gives each traveler:
- A unified inbox/outbox.
- A consistent causal record of all their interactions.
- A single point of governance for future upgrades (delegation, recovery, etc.).

Programs only send messages to other programs (including account programs).
No direct program-to-actor communication is allowed.

## Effect Lifecycle

1. Traveler submits a `ProposedEffect` (to their account program).
2. Account program applies the effect to its own state.
3. If the effect is a `Deposit`, `Invoke`, or similar, it emits a cross-program message.
4. Target program applies the effect (if preconditions hold).
5. Applied effect is recorded in the per-resource log.
6. Applied effect is gossiped to other Bandits.
7. ZK proof is generated and attached to the log entry.

## Invariants

| Invariant                | Description                                               |
|--------------------------|-----------------------------------------------------------|
| Resource Ownership       | Every resource has exactly one program owner at all times. |
| Actor Separation         | Actors interact only through account programs.            |
| Effect-Only Changes      | All state changes flow through applied effects.           |
| Per-Resource Causal Logs | Each resource maintains its own log.                      |
| Causal Consistency       | Every effect observes a specific time map.                |
| Proof Completeness       | Every applied effect has a proof.                         |
| Replayability            | Replaying logs reconstructs exact state.                  |
| Simulation Parity        | Same rules hold across all simulation modes.              |

## Simulation Modes

The system runs in three environments:

| Mode            | Description                                           |
|-----------------|-------------------------------------------------------|
| In-Memory       | All actors run in a single process (fast development).|
| Multi-Process   | Each actor is a process (realistic local test).       |
| Geo-Distributed | Actors run on separate machines (full deployment test). |

All invariants and causal consistency rules must hold in all modes.

## Proof Model

Every applied effect must produce a proof that covers:
- Preconditions held.
- Time map was correct.
- Resource ledger changes were valid.
- Internal state changes followed declared logic.

The proof is:
- Attached to the effect log entry.
- Verifiable by anyone from first principles (program code, time map, previous state).

## Summary Flow

1. Traveler submits effect via account program.
2. Account program applies effect.
3. Account program emits cross-program message.
4. Target program receives message.
5. Target program applies effect (if valid).
6. Effect is logged and proven.
7. Effect is gossiped.

---

This specification defines the complete lifecycle of effects, resource ownership, and causal consistency, ensuring that Time Bandits programs are:

- Verifiable
- Replayable
- Causally consistent across multiple blockchains
- Safe from reentrancy, race conditions, and direct actor interference
