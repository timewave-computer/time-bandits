# Time Bandits System Specification (SPEC)

---

## Version

**Current Revision:** 2025-03-07

---

## Overview

This document defines the **technical specification** for Time Bandits, capturing the latest understanding of its architecture, data structures, processes, and operational semantics. This specification serves as a **technical reference** for implementers, contributors, and auditors.

---

# System Purpose

Time Bandits provides a **secure, provable, and composable execution environment** for programs that span **multiple timelines** (blockchains, event logs, or external systems of record). Programs interact with **resources** and **facts** from these timelines while preserving:

- Full causal traceability.
- Deterministic replayability.
- Sovereign ownership (time travelers own their programs entirely).
- Zero knowledge proofs of execution.
- Compatibility with **heterogeneous infrastructure** across chains.

---

# Core Actors

## Time Travelers

- Deploy programs.
- Own programs and account programs.
- Initiate cross-timeline deposits and withdrawals.
- Propose schema upgrades.
- Submit messages to timelines via Time Keepers.

## Time Keepers

- One per timeline.
- Observe external facts (balances, prices, transactions).
- Validate and timestamp facts.
- Append facts to per-timeline **FactLog**.
- Respond to fact queries from programs and bandits.

## Time Bandits

- Operate the execution network.
- Execute program logic.
- Propose and apply effects.
- Generate ZK proofs of execution.
- Maintain **unified logs** for all programs and account programs.
- Disseminate facts and effects across the P2P network.

---

# Core Programs

## Logic Programs

- Define user-defined effect pipelines (business logic).
- Apply effects based on facts and received invocations.
- Declare:
    - Schema (state format).
    - Safe state policy.
    - Evolution rules.

## Account Programs

- Each traveler has one account program per deployment context.
- Owns all external assets (tokens, balances) across timelines.
- Exposes:
    - Deposit API.
    - Withdrawal API.
    - Cross-program transfer API.
    - Balance query API.
- Tracks causal history in its own effect DAG.

---

# Core Data Structures

## Fact

```haskell
data Fact = Fact
    { factID :: FactID
    , timeline :: TimelineID
    , factType :: FactType
    , factValue :: FactValue
    , observedAt :: LamportTime
    , observationProof :: ObservationProof
    }
```

---

## Effect

```haskell
data Effect
    = Deposit { timeline :: TimelineID, asset :: Asset, amount :: Amount }
    | Withdraw { timeline :: TimelineID, asset :: Asset, amount :: Amount }
    | Transfer { fromProgram :: ProgramID, toProgram :: ProgramID, asset :: Asset, amount :: Amount }
    | ObserveFact { factID :: FactID }
    | Invoke { targetProgram :: ProgramID, invocation :: Invocation }
    | EvolveSchema { oldSchema :: Schema, newSchema :: Schema, evolutionResult :: EvolutionResult }
    | CustomEffect Text Value
```

---

## FactSnapshot (causal dependency record)

```haskell
data FactSnapshot = FactSnapshot
    { observedFacts :: [FactID]
    , observer :: KeeperID
    }
```

---

## Program State

```haskell
data ProgramState = ProgramState
    { schema :: Schema
    , safeStatePolicy :: SafeStatePolicy
    , effectDAG :: EffectDAG
    , factSnapshots :: Map EffectID FactSnapshot
    }
```

---

## Account Program State

```haskell
data AccountProgramState = AccountProgramState
    { balances :: Map (TimelineID, Asset) Amount
    , effectDAG :: EffectDAG
    }
```

---

# Effect Pipeline

- Effects are **causally ordered** — each effect references parents in the effect DAG.
- Every effect depends on a **FactSnapshot** — facts observed before the effect applied.
- Effects are **content-addressed** — their hash becomes part of the DAG.
- Effects are gossiped across the Bandit network before being finalized.

---

# Unified Log

Each program and account program maintains an **append-only, content-addressed log**, recording:

- Applied effects.
- Observed facts.
- Lifecycle events (e.g., schema upgrade, safe state transition).

Each log segment is:

- Immutable after write.
- Signed by the originating bandit.
- Replicated across the P2P network.

---

# Invocation Model

- Programs **invoke other programs** via `Invoke` effects.
- Invocations include:
    - FactSnapshot (dependencies at invocation time).
    - Program state root hash (proof of caller state).
- Responses are returned as **observed facts**.
- Invocations are **asynchronous** — the caller may continue execution while awaiting response.

---

# Resource Ownership

- **Programs do not own external resources directly.**
- All cross-timeline assets are held in **account programs**.
- Logic programs interact with account programs via `Deposit`, `Withdraw`, and `Transfer` effects.
- Account programs have standardized schemas and upgrade rules.

---

# Safe State Policy

Programs declare a **safe state policy** defining when schema upgrades and certain privileged operations (like external calls) are permitted.

Safe state requires:

- No pending cross-program calls.
- No pending asset transfers.
- No unobserved external facts referenced in the current effect.

---

# Schema Evolution

- Programs declare:
    - Current schema version.
    - Allowed evolution rules.
- Evolution is recorded as an `EvolveSchema` effect.
- Safe state must hold before any evolution can apply.
- Programs **opt into upgrades** — no forced schema changes.

---

# Content-Addressable Code

## Overview

Time Bandits implements a content-addressable code storage system, where code is identified by its content hash rather than by name. This approach ensures immutability, eliminates dependency conflicts, and simplifies refactoring.

## Components

### Code Repository

```haskell
data CodeDefinition = CodeDefinition
    { cdHash :: CodeHash       -- Content hash of the code
    , cdSource :: Text         -- Source code
    , cdType :: DefType        -- Type of definition (function or module)
    }

data DefType = FunctionDef | ModuleDef
```

### Hash-Based Identification

- Each function or module is identified by a cryptographic hash of its content.
- Any change to code results in a new hash, creating a new immutable definition.
- Hash generation ensures deterministic outputs for the same input.

### Decoupled Naming System

- Names are separate from code identities, serving as metadata pointers to content hashes.
- Renaming code does not break references, as all dependencies link to content hashes.
- Multiple names can point to the same code definition.

### Content-Aware Execution

- Code is executed by its content hash through the ContentAddressableExecutor.
- Execution context maintains state during evaluation.
- Both hash-based and name-based execution entry points are supported.

## Benefits

- **Dependency Conflict Resolution**: Different versions of code with the same name can coexist.
- **Code Immutability**: Once defined, code cannot be altered, enhancing stability.
- **Simplified Refactoring**: Renaming and restructuring become trivial operations.
- **Version Precision**: Programs can specify exact versions of dependencies by their content hash.

---

# Fact Management

- Facts are observed by keepers.
- Each observed fact is:
    - Signed by a keeper.
    - Appended to the **FactLog**.
    - Gossiped to Bandits.
- Programs consume facts through:
    - `ObserveFact` effects.
    - Fact queries to the FactLog.

---

# Replay Model

- Program state is **fully reconstructible** from:
    - Unified program log (effects, facts, events).
    - Timeline FactLogs.
- Replay applies:
    - Facts in observed order.
    - Effects in causal order.
    - Schema changes exactly as they occurred.
- No external queries are allowed during replay — all required state is logged.

---

# Concurrency Model

## System-Level Concurrency

- Bandits execute multiple programs concurrently.
- Programs contend for access to shared account programs.
- Cross-program calls are asynchronous and mediated by logs.

## Program-Level Concurrency

- Programs can spawn concurrent child programs.
- Programs can branch into concurrent workflows, provided:
    - Each branch has disjoint fact/resource dependencies.

---

# Time Model

- Each timeline has its own **Lamport Clock**.
- Effects and facts are timestamped using:
    - Timeline clock (external facts).
    - Program-local Lamport clock (internal effects).
- Programs only advance after observing facts with non-decreasing timestamps.
- Cross-timeline events respect:
    - External timeline ordering (fact observation).
    - Internal causal ordering (effect DAG).

---

# Sovereignty and Upgrade Control

- Travelers **own their programs entirely**.
- Travelers must explicitly approve:
    - Schema upgrades.
    - Logic upgrades.
- Bandits enforce:
    - Schema compatibility.
    - Protocol version compatibility.
- Programs pinned to old schemas can continue to run, provided:
    - They are supported by at least one Bandit.
- Time travelers may opt out of new protocol versions at the cost of reduced Bandit support.

---

# Key Interfaces (Program Runtime)

| Interface | Description |
|---|---|
| observeFact | Query a fact from the keeper. |
| applyEffect | Apply a state transition effect. |
| invokeProgram | Send an invocation to another program. |
| evolveSchema | Propose a schema change. |
| checkSafeState | Verify safe state before privileged operations. |

---

# Key Interfaces (Account Program)

| Interface | Description |
|---|---|
| deposit | Deposit asset into account. |
| withdraw | Withdraw asset from account. |
| transfer | Transfer asset to another program. |
| queryBalance | Query per-timeline balances. |

