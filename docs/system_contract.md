# Time Bandits System Contract

---

## Version

**Current Revision:** 2025-03-07

---

## Purpose

This document defines the **system contract** for Time Bandits, establishing the fundamental invariants, roles, ownership rules, and guarantees that underpin the system. It serves as both a specification and a **social/legal contract** between participants — travelers, keepers, bandits, and the broader execution network.

This document reflects the **latest design**, incorporating:
- Account programs for resource ownership.
- Unified log for facts, effects, and events.
- Fact observation pipeline.
- Safe state and schema evolution rules.
- Updated concurrency and invocation models.
- Role separation between actors.

---

# Core Invariants

The following invariants **must always hold**, across all modes of execution (in-memory, multi-process, geo-distributed), across all simulation and production deployments:

1. **Programs do not own resources directly.**
    - All external resources (tokens, balances, positions) are owned by **account programs**, not by individual programs.

2. **Every effect has a complete causal chain.**
    - Every effect must reference the exact prior facts and effects it depended on.
    - The full causal graph is append-only and content-addressed.

3. **Fact observations are first-class.**
    - Programs cannot act on external state unless that state has been observed and proven by a Time Keeper.
    - Facts are **immutable, content-addressed, and independently provable**.

4. **Programs cannot be forcibly upgraded.**
    - Time Travelers (program owners) must explicitly approve schema and logic upgrades.
    - Programs can only evolve schemas while in **safe states**.

5. **Replay is complete and deterministic.**
    - All program state, resource flows, facts, and effects must be fully reconstructible from logs alone — no external state should be required.

6. **Programs remain timeline-agnostic.**
    - Programs do not need timeline-specific logic. All cross-timeline interaction is mediated via account programs and fact observations.

---

# Core Actors

## Time Travelers

- Own programs.
- Deploy new programs.
- Propose schema and logic upgrades.
- Maintain full sovereignty over programs — no forced upgrades.
- Submit external deposits into account programs.
- Own account programs that hold and transfer assets.

## Time Keepers

- One per timeline.
- Observe external facts (balances, prices, inclusion proofs).
- Sign observation proofs.
- Append facts to **FactLog**.
- Validate external messages before accepting into a timeline.
- Respond to fact queries from programs and bandits.
- Manage per-timeline clocks.

## Time Bandits

- Operate the **execution network**.
- Execute program logic and generate **zero-knowledge proofs** of execution.
- Gossip facts and effects across the network.
- Maintain a content-addressed, append-only log of all:
    - Effects.
    - Facts.
    - Events.
- Enforce **safe state rules** before accepting upgrades.
- Enforce **schema compatibility** before running programs.
- Synchronize across timelines to enforce cross-timeline invariants.

---

# Core Programs

## Program (Logic Program)

- Defines **causal effect pipeline** (business logic).
- Declares:
    - Schema (state structure).
    - Protocol compatibility version.
    - Evolution rules (what schema changes are allowed).
- Does **not own resources directly** — interacts with resources via account programs.
- Includes:
    - Effect DAG (causal history).
    - FactSnapshots (external dependencies).
    - Current schema version.
    - Declared safe state policy.

## Account Program

- Each traveler has one account program per deployment context.
- Holds all traveler’s cross-timeline balances.
- Exposes:
    - Deposit API (timeline-specific).
    - Withdraw API (timeline-specific).
    - Transfer API (to programs).
    - Query API (balance reporting).
- Maintains its own **Effect DAG** (resource history).
- Separately replayable from logic programs.
- Schema is **stable and standard** across all account programs.

---

# Core Data Structures

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

## Program

```haskell
data Program = Program
    { programID :: ProgramID
    , travelerID :: TravelerID
    , schema :: Schema
    , safeStatePolicy :: SafeStatePolicy
    , effectDAG :: EffectDAG
    }
```

---

## AccountProgram

```haskell
data AccountProgram = AccountProgram
    { accountID :: AccountID
    , owner :: TravelerID
    , balances :: Map (TimelineID, Asset) Amount
    , effectDAG :: EffectDAG
    }
```

---

# Concurrency Model

## System-Level Concurrency

- Time Bandits executes **multiple programs concurrently**.
- Each account program acts as a **synchronization point** for resources.
- Programs interacting with the **same account** contend for resource access.
- Programs can operate concurrently if they do not depend on the same facts/resources.

## Program-Level Concurrency

- Programs can spawn **child programs** and wait for their results.
- Programs can split into independent concurrent branches, provided:
    - Each branch works on a **disjoint fact/resource set**.
- Programs receive **fact and effect streams** in causal order.

---

# Invocation Model

- Programs **invoke** other programs using an **invocation effect**.
- Invocations:
    - Reference **fact snapshots** (what was known at invocation time).
    - Include proof of **current state** of the caller.
- Cross-program calls are **asynchronous** — programs receive results via observed facts.

---

# Schema Evolution Rules

| Change | Allowed by Default? |
|---|---|
| Add optional field | - |
| Add field with default | - |
| Remove unused field | - |
| Rename field | ❌ |
| Change field type | ❌ |

Programs can override these defaults via their declared **evolution rules**.

---

# Safe State Definition

A program is in a **safe state** if:
- No pending cross-program calls.
- No pending resource withdrawals.
- All external facts referenced in the current effect are fully observed.
- All concurrent branches have terminated.

---

# Time Model

- Every timeline has its own **Lamport Clock**.
- Program facts and effects are timestamped using:
    - Timeline clock (external facts).
    - Program-local Lamport clock (internal effects).
- Programs can only advance **after observing facts with non-decreasing timestamps**.
- Time Bandits ensures that cross-timeline events respect:
    - External timeline ordering (via fact observation).
    - Internal causal ordering (via effect DAG).

---

# Replay Model

- Replay reconstructs:
    - All programs.
    - All account programs.
    - All facts.
    - All effects.
- Replay requires:
    - Unified logs for all programs and accounts.
    - FactLogs from all keepers.
    - Full effect DAGs for all programs.
- Replay is **deterministic** — given the same logs, replay always produces the same state.

---

# Sovereignty and Upgrade Control

- Time Travelers fully control:
    - Whether their programs upgrade schemas.
    - Whether their programs upgrade logic.
- Programs can only upgrade if:
    - They are in a safe state.
    - The traveler explicitly allows the upgrade.
- Bandits enforce schema compatibility when accepting new programs.

---

# Simulation Environment

- Time Bandits supports:
    - In-memory simulations.
    - Local multi-process simulations.
    - Geo-distributed simulations.
- All 3 modes run the exact same program logic, using the same logs and effect pipeline.

---

# Summary

This **system contract** codifies the guarantees that make Time Bandits a reliable foundation for cross-timeline, effect-driven programs:

- Programs are causally consistent.  
- External facts are provable and logged.  
- Resources are safely managed via account programs.  
- Programs remain timeline-agnostic.  
- Replay is always complete and deterministic.  
- Programs evolve safely and only with owner consent.  
- Execution can be verified independently using public logs.
