# Time Bandits

## Overview

Time Bandits is a distributed system for deploying and executing programs that coordinate logic and assets across multiple independent timelines (blockchains).

Programs in Time Bandits are not executed directly on any single chain. Instead, they live as a set of zero knowledge program keys committed to the involved chains, and executed within a P2P network. All program actions are captured in a content-addressed, hash-linked execution log. This log, combined with zk proofs of execution, ensures that programs are fully replayable and auditable, even across conflicting or adversarial timelines.

---

## Motivation

Today, cross-chain programs are fragile, because they rely on:
- Trusted bridges, which are centralized and vulnerable.
- Fragile, ad hoc cross-chain message passing.
- Implicit, often ambiguous assumptions about the ordering of events across timelines.

Time Bandits addresses these problems by:
- Moving program logic into a zk verified executor network.
- Explicitly modeling time across multiple timelines in a time map, a causally ordered, content-addressed structure capturing each program's view of external chains.
- Defining programs as sequences of declared effects, each explicitly referencing the time map and producing a new log entry.

This combination of verifiable off-chain execution, effect composition, and formal causal time tracking creates a foundation for safer, more auditable cross-chain coordination.

---

## Architecture: A Temporal Effect System

Time Bandits is designed as a timeline-local + cross-timeline distributed state machine with a pluggable event/effect system:

- Resources represent both program state (internal conditions, requirements, capacities) and control authority (who can advance state, under what conditions).
- Effects represent state advances—i.e., token transfers, escrows, swaps—triggered by actors (time travelers or other programs).
- Timelines act as independent shards or zones of causality, with the option to have programs that span multiple timelines.
- Effects compose dynamically: They can be sequenced, combined, and even interact (e.g., dependencies between effects or higher-order effects like "retry on failure").

The entire system forms a resource-aware temporal process algebra—programs are compositions of token-based effects guarded by resource predicates, creating a formal "temporal effect system" that ensures causality and security across distributed timelines.

---

## Core Actors

### Time Travelers
Time Travelers are the entities who deploy programs and submit state transition messages to the timelines. They are responsible for initiating program execution by creating signed transition messages that include necessary proofs and resources.

### Time Keepers
Time Keepers are per-timeline actors responsible for:
- Maintaining the integrity of individual timelines (e.g., blockchains, event logs).
- Validating and recording new messages (deposits, claims, calls).
- Serving timeline state queries to authorized actors.
- Ensuring that all applied transitions follow timeline-specific rules.

### Time Bandits
Time Bandits operate the P2P network that forms the backbone of the system. They:
- Execute program steps and generate cryptographic proofs.
- Disseminate messages through the network.
- Maintain the execution log and validate proofs.
- Enforce security properties across the system.

## 3. Security Verification

The security architecture of Time Bandits is enforced by the `SecurityVerifier`, which guarantees critical system-level properties:

- Prevention of double-spending: Through single-owner guarantees and explicit resource transfer.
- Protection against reentrancy attacks: Via Lamport clock ordering and causal verification.
- Maintenance of a complete audit trail: Through a verifiable, content-addressed execution log.
- Enforcement against backdated transitions: Using time map mechanisms to prevent time-based attacks.

## 4. Modular Architecture

The refactored codebase provides a robust foundation for cross-timeline programming:

- Clear type separation: First-class entities (Timeline, Resource, Program, Effect) with well-defined interfaces.
- Explicit effect execution: All state changes require formal guards and validation.
- Program invocation model: Tracks resource ownership and ensures secure transfer.
- Zero-knowledge proof integration: For guard validation and ownership verification.
- Multi-mode simulation: Support for in-memory, local multi-process, and geo-distributed deployments.

## 5. Timeline Descriptors and Adapters

Time Bandits supports multiple blockchains and distributed ledgers through:

- Formal timeline descriptors: Define the properties of different timelines (EVM, CosmWasm, etc.).
- Timeline adapters: Provide a uniform interface to diverse blockchain ecosystems.
- TOML-based configuration: Simplify adding new timeline types to the system.

## 6. Execution Log

Every program execution produces an append-only, content-addressed log that:
- Captures all applied effects with causal links.
- Provides replay capability for audit and verification.
- Includes proofs of correct execution.
- Forms the foundation for cross-timeline verification.

## 7. Centralized Effect Interpreter

A key feature is the centralized interpreter for all program effects, which:
- Provides uniform validation across all timelines.
- Ensures causal ordering with Lamport clocks.
- Enforces security invariants on all effects.
- Produces verifiable logs for every execution.

## 8. Cross-Program Resource Flow

Resource flow between programs follows strict rules:
- Resources have exactly one owner at all times.
- Transfer requires explicit escrow operations.
- Program invocations have clear resource contracts.
- The "temporal effect system" ensures that all operations respect causal constraints.