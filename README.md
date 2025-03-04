# Time Bandits

![](map_of_time.png)

> Taking its name from [Terry Gilliam’s 1981 fantasy adventure](https://en.wikipedia.org/wiki/Time_Bandits), this project embraces the chaos of distributed time travel in the hopes of making cross-chain programming fun again.

Time Bandits is a distributed system for deploying and executing programs that coordinate logic and assets across multiple independent timelines (blockchains).

Programs are not executed directly on any single chain. Instead, they live as a set of commitments to zero knowledge verification keys on the involved chains. Programs are executed by a P2P network of "Time Bandits" that facilitate adventures through time. All program actions are captured by content-addressed, hash-linked logs. These append-only logs, combined with zk proofs of execution, ensure that programs are fully replayable and auditable, even across conflicting or adversarial timelines.

## Getting Started

### Enter the environment and build

```
# Set up development environment with Nix
nix develop

# Build the project
nix build
```

### Running a Simple Scenario

```bash
# Run an in-memory simulation with the basic scenario
./scripts/run_in_memory_simulation.sh --scenario basic

# Or run directly with cabal
cabal run time-bandits -- sim in-memory --scenario basic
```

### Documentation

For more detailed information about the Time-Bandits system, please refer to:

- [Codebase Overview](docs/codebase_overview.md) - Overview of the main components and architecture
- [Onboarding Guide](docs/onboarding_guide.md) - Detailed guide for new developers
- [Developer Workflow](docs/dev_workflow.md) - Common development workflows
- [Glossary](docs/glossary.md) - Definitions of key terms and concepts

## System Overview

### Actors

**Time Travelers** are the entities who deploy programs and submit state transition messages to timelines. They initiate program execution by creating signed messages that propose new effects, attach necessary proofs, and commit any required resources. Time Travelers never directly modify program state—instead, they act as external proposers who trigger causally consistent updates.

**Time Keepers** are timeline-specific actors responsible for maintaining the integrity of individual timelines. Each Time Keeper observes a particular blockchain, rollup, or event log, validating new messages (like deposits, claims, and cross-program calls) and ensuring they follow that timeline’s rules. Time Keepers also expose query interfaces so Time Bandits can request proofs, balances, and other timeline state needed to validate preconditions.

**Time Bandits** operate the P2P execution network that applies program effects, generates cryptographic proofs, and maintains the immutable execution log. They observe incoming messages, apply valid effects to program state, and link each effect to its causal predecessor. Time Bandits are responsible for enforcing all cross-program and cross-timeline security properties, ensuring that programs only advance if all preconditions are satisfied, time maps are up to date, and every resource transfer follows strict ownership rules.

### Programs

A program in Time Bandits is pure data: a list of effects, each with explicit preconditions, parameters, and causal dependencies. Programs have:
- Immutable code (their effect list).
- Mutable memory (resources and internal state they control).
- A linked execution log that records every applied effect, including proofs and the exact time map each effect observed.

### Effects

Effects are the only way programs can change state. Each effect:
- Describes a state transition (e.g., escrow, claim, invocation).
- Declares what preconditions must hold (e.g., "escrow must exist", "balance must be ≥ X").
- Produces a proof that all conditions held and the effect applied correctly.

Effects are intentionally simple and composable. Travelers write programs by composing sequences of effects, rather than writing arbitrary code.

### Time Map

The time map is the core causal clock of Time Bandits. It tracks:
- The latest observed block or event for every external timeline.
- Real-world timestamps for these observations.
- A logical Lamport clock that tracks causal ordering across chains.

Every applied effect references a specific time map snapshot, ensuring that no effect can be applied out of causal order.

### Execution Log

Every applied effect is recorded in a content-addressed, append-only log, linked to its causal parent. Each entry contains:
- The effect applied.
- The time map observed.
- A proof that the effect was applied correctly.
- The resulting program state hash.

This log makes every program fully replayable and auditable, even across different nodes.

### Timeline Adapters

Timeline Adapters handle the specific mechanics of interacting with different chains — encoding transactions, querying balances, fetching proofs. Time Bandits has a pluggable adapter system, so adding support for new timelines only requires adding an adapter, not modifying core logic.

### Simulation Environment

Time Bandits includes a flexible simulation environment for developing, testing, and experimenting with programs across multiple execution contexts. The simulation controller supports:

- In-Memory Simulation: All actors (Time Travelers, Keepers, and Bandits) run in a single process, with direct function calls between them. This is ideal for fast development and unit testing.
- Local Multi-Process Simulation: Each actor runs as a separate `nix run` process on the same machine, communicating through actual message passing. This simulates real network behavior without requiring multiple hosts.
- Geo-Distributed Simulation: Each actor runs on a different machine, and the controller orchestrates remote startup, shutdown, and monitoring. This allows full end-to-end distributed system testing.

Scenarios are defined declaratively in YAML, specifying:
- The actors to spawn.
- The timelines to watch.
- The programs to deploy.
- The initial messages to inject.

This makes it easy to test causal consistency, failure recovery, and performance characteristics under different deployment conditions.

## Motivation

More and more applications need to operate across multiple blockchains and rollups, but writing these cross-chain programs is far harder than most developers expect. Each chain has its own flavor of virtual machine, authenticated data structure, serialization formats, preferred bridging protocol, RPC interface, etc., meaning dApp developers are forced to juggle all of this before they can write any business logic . Worse, cross-chain interactions are inherently asynchronous—programs must initiate work on one chain, wait for confirmations from another, react to unpredictable reorgs or delays, and somehow ensure all of this resolves into a coherent, auditable flow. The typical tools for async programming—callbacks, retries, tracing, and structured error handling—are almost completely absent when working across decentralized networks, leaving developers to stitch solutions together by hand.

If this wasn't bad enough, cross-chain software is almost always high-stakes. As a result, cross-chain programs must be of the highest assurance and designed to gracefully fail into well-defined error states under adverse conditions. All this without relying on trusted intermediaries. Time Bandits tackles this by ensuring that programs are always owned and operated by the actors initiating them (Time Travelers). Zero knowledge proofs of execution give the assurance that off-chain actors adhere to pre-defined control flow.

To make this possible, Time Bandits introduces an temporal effect system, where programs are written as the composition of atomic algebreic effects, each reliant on their own preconditions, dependencies, and oracle definitions. This transforms the cross-chain programming experience from gluing together a brittle collection of scripts, to defining a verifiable data structure—a program isn’t "run" so much as it unfolds effect by effect, with every step causally linked to its predecessors, accompanied by a proof. The peer-to-peer network exists to provide the distributed transient storage these programs use to interact, while ensuring that no single actor can censor, reorder, or alter program history. The combination of effect system, content-addressed immutable logs, and verifiable distributed execution, creates the foundation for secure, auditable, actor-owned cross-chain applications, treating multi-chain programming as a first-class design space.