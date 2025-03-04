# Time Bandits

A framework for causally consistent cross-timeline operations, resource management, and provable effects.

## Architecture Overview

The Time Bandits system implements a causally consistent execution environment across multiple timelines (blockchains, rollups, event logs, etc.) with clear actor role separation and verifiable state transitions.

### Key Components

#### 1. Actor Roles

The system explicitly separates three actor roles:

- Time Travelers: Deploy programs and submit transition messages to timelines
- Time Keepers: Maintain individual timelines, validate messages against timeline rules
- Time Bandits: Operate P2P execution network, enforce causal order, maintain time maps

#### 2. TimeMap

The `TimeMap` type is a first-class concept that:
- Tracks observed timeline heads and timestamps
- Maintains per-timeline Lamport clocks
- Is explicitly passed to every effect application
- Is updated after every successful transition

#### 3. Execution Log

All operations are recorded in a structured, content-addressed execution log with:
- Applied effect
- Timestamp
- Parent effect hash (causal link)
- Resulting program state hash
- Attached proofs

#### 4. Timeline Descriptors

Each supported timeline has a `timeline.toml` descriptor that defines:
- Clock mechanism (block height, slot number, timestamp)
- Resource mappings
- Effect adapters
- RPC endpoints
- State query methods

#### 5. Centralized Effect Interpreter

The `EffectInterpreter` enforces the system contract by:
- Validating effect preconditions using the current `TimeMap`
- Enforcing the single-owner invariant for all resources
- Ensuring causal ordering of all applied effects
- Applying effects through appropriate timeline adapters
- Updating program memory
- Updating the `TimeMap`
- Appending to the `ExecutionLog` with causal links

Effect application is a formally controlled process, not just handler logic. Every effect must pass through the interpreter, which enforces all system invariants and maintains the causal enforcement pipeline end-to-end.

#### 6. Cross-Program Resource Flow

The system enforces strict resource ownership rules:
- Every resource has exactly one owner at any time
- Cross-program transfers use explicit escrow/claim operations
- All transfers leave a traceable log entry proving custody transfer

## Running Scenarios

The system supports three simulation modes:
- InMemory: All actors operate in a single process
- LocalProcesses: Actors run in separate processes with Unix socket messaging
- GeoDistributed: Actors run on remote machines with TCP/RPC messaging

Scenarios can be defined in TOML files:

```bash
# Run an example scenario
cabal run time-bandits-scenario -- examples/simple_scenario.toml
```

## Development

The project uses a Nix-based development environment:

```bash
# Enter development environment
nix develop

# Build the project
cabal build

# Run tests
cabal test
```

## Documentation

- [Refactor Plan](docs/refactor.md): Comprehensive refactoring plan
- [Timeline API](docs/timeline_api.md): Timeline API documentation
- [Resource Model](docs/resource_model.md): Resource ownership model

## Example Scenarios

The `examples/` directory contains scenario definitions:

- `simple_scenario.toml`: Basic cross-timeline resource transfer
- `causal_ordering_scenario.toml`: Demonstration of TimeMap and causal ordering
- `execution_log_scenario.toml`: Structured execution log with causal links
- `timeline_descriptors/`: Example timeline descriptor files

## License

This project is licensed under the Apache License 2.0 - see the LICENSE file for details.






# Time Bandits

## Introduction

Time Bandits is a new kind of distributed execution environment designed to operate across multiple independent timelines — blockchains, rollups, distributed ledgers, and even external event logs. At its core, Time Bandits lets developers write programs that live outside of any single chain, while still interacting with assets and state on many chains, all with strong causal consistency and a provable execution record.  

The system is built around a few key ideas: programs are composed entirely from declared effects; all program state transitions are captured in a content-addressed, hash-linked, append-only log; every effect observes and updates a formally modeled map of time that spans multiple timelines; and program execution happens over a peer-to-peer transient storage network that allows programs to move, replicate, and audit their own state history.

---

## Motivation

Cross-chain programs are fragile today. They rely heavily on brittle bridges, often trust specific validator sets, and struggle to provide clear causal consistency guarantees when events on one chain depend on events from another.  

Time Bandits was created to address this, by moving the program itself off-chain, while preserving provability, consistency, and replayability. In this model, chains don't need to "trust each other." Instead, programs track all relevant chains through a sharded map of time, and programs themselves enforce all the conditions that make their logic safe. Every program effect is applied only if all preconditions — across all timelines — are satisfied, and every applied effect is logged and proven.  

The result is a system where program state and program history are the only source of truth. Assets live directly on blockchains, but the logic that controls those assets lives off-chain, with strong cryptographic proofs ensuring that no transition could have happened without satisfying program-defined rules.

---

## The Model of Time

One of Time Bandits' core innovations is its formal model of time, designed to capture causality across multiple independent chains. Each chain has its own native clock — a block height, a slot, a timestamp — but programs in Time Bandits do not directly depend on any single clock. Instead, they rely on a time map, a structured, composite view of time that records:

- The latest observed state of every timeline the program interacts with.
- The real-world wall-clock times at which those states were observed.
- A set of Lamport clocks that track causal relationships between observed events across all timelines.

Every effect applied by a program must reference a specific time map snapshot, proving that the effect was applied only after all required events had occurred. This is what allows Time Bandits programs to enforce causal consistency, even when events on separate blockchains arrive at different times or in different orders.

---

## The Effect System

Programs in Time Bandits are not sequences of instructions, but sequences of declared effects. Each effect describes:

- What the program intends to do (e.g., move assets, call another program, update internal state).
- What preconditions must hold (e.g., specific balances, timeline state, external facts).
- What resources are involved (e.g., tokens, escrows, program memory slots).

Effects compose naturally: programs can escrow assets to each other, invoke each other, and respond to external events, all by emitting new effects. Because effects are first-class objects, they can be referenced, proven, and traced. This makes program execution a process of gradual, effect-by-effect materialization, rather than a single monolithic transaction.

This is a major break from traditional smart contracts. Time Bandits programs are not "executed to completion" — they are grown over time, as external actors submit new transition messages that apply one effect at a time.

---

## Immutable Objects and Hash-Linked Logs

Every applied effect produces a content-addressed, immutable object describing:

- The effect itself.
- The preconditions and time map it observed.
- The resulting state of the program.
- The cryptographic proof that all conditions were satisfied.

These objects are linked to their causal parents, forming a hash-linked, append-only log of the program's execution history. This log is replayable — anyone can start from the initial program deployment and replay all applied effects, recomputing the final state and verifying all proofs along the way.

This is more than an audit trail: it is a complete causal and semantic history of the program's evolution, independently verifiable by anyone, and independent of any particular blockchain or validator set.

---

## Programs as Self-Contained Objects

A Time Bandits program, at any point in time, can be fully represented by:

- Its immutable program definition (effects and guards).
- Its current program memory (resources it owns).
- Its current time map (what it knows about all relevant timelines).
- Its execution log (the full causal history of applied effects).

This makes programs portable. They can be migrated between operators, replicated for resilience, or discovered and audited by new participants. Every program is a standalone, hash-addressed object, meaning programs themselves fit naturally into content-addressed peer-to-peer networks.

---

## The P2P Transient Storage Network

Time Bandits programs do not live on any single machine or chain. Instead, they live in a P2P transient storage network, where programs are replicated across multiple peers, and each peer is responsible for:

- Storing and serving program state.
- Observing relevant timelines and updating the program's time map.
- Accepting and validating new transition messages.
- Applying effects and generating proofs.
- Gossiping new state to other peers.

This ensures that programs remain available and provable even if individual operators disappear, and makes Time Bandits highly resistant to both censorship and operator capture.

---

## Program Invocation as Resource Flow

Programs do not directly call each other. Instead, they pass resources, moving assets and capabilities between memory slots owned by different programs. This is inspired by linear types and move semantics: every resource has exactly one owner at all times, and the only way to interact with a resource is to receive ownership of it.  

This makes cross-program interactions explicit and provable. No program can tamper with another's memory — all interactions happen via controlled, logged transfers of ownership.

---

## Separation of Concerns: Travelers, Keepers, Bandits

The system deliberately separates:

- Time Travelers, who write programs and submit messages.
- Time Keepers, who maintain individual timelines and validate external messages.
- Time Bandits, who operate the P2P network, enforce causal consistency, apply effects, generate proofs, and maintain the logs.

This separation allows Time Bandits to remain timeline-agnostic — it can work across any timeline that offers basic append and query operations, without being bound to any specific chain architecture.

---

## Summary

Time Bandits is, at its heart, a system for building causally consistent, provable, cross-timeline programs. It achieves this by combining:

- A first-class effect composition language.
- A causally ordered map of time.
- A content-addressed execution log.
- A P2P transient storage and replication layer.
- A strict separation of state (assets) and logic (programs).

By moving program logic off-chain and tying it to provable effect logs, Time Bandits offers a new design space for cross-chain coordination — one that avoids trusted bridges, centralized sequencers, or fragile synchronous assumptions.

---

For more detail, see:
- `docs/spec.md` for the full technical specification.
- `docs/system_contract.md` for the formal system contract.
- `docs/refactor.md` for the work underway to align the codebase with this design.



Time Bandits has a pluggable event/effect system over timelines, where new types of effects can be registered dynamically. Effects should compose, meaning they can be sequenced, combined, and even interact in some way (e.g., dependencies between effects or higher-order effects like "retry on failure"). This leans into a "temporal effect system"

what you’re describing is essentially a timeline-local + cross-timeline distributed state machine, with:

- Resources representing both program state (internal conditions, requirements, capacities) and control authority (who can advance state, under what conditions).
- Effects representing state advances — i.e., token transfers, escrows, swaps — triggered by actors (time travellers or other programs).
- Timelines acting like independent shards or zones of causality, with the option to have programs that span timelines.
- The entire thing forms a resource-aware temporal process algebra — programs are compositions of token-based effects guarded by resource predicates.