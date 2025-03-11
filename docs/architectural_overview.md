# Architectural Overview - Time Bandits

---

## Overview

**Time Bandits** is a decentralized execution system for **cross-timeline programs** — programs that operate across **multiple independent blockchains and distributed ledgers**, referred to collectively as **timelines**. Time Bandits enables **secure, replayable, causally-consistent execution of these programs** without requiring trust in any central party.

The system achieves this by:

- Defining **temporal effect programs** that describe **time-coupled logic** across multiple chains.
- Operating a **P2P network** where Bandit nodes execute effects and generate **zero-knowledge proofs of correct execution**.
- Relying on **time maps**, a causally-consistent snapshot of external blockchain states, to ground program effects in provable reality.

---

## System Components

### 1. Time Travelers

**Time Travelers** are the **users and authors** of programs. They:

- Write programs using the **Temporal Effect Language** (TEL).
- Deploy programs to the network.
- Initiate program execution by submitting **signed messages** to their **account program**.
- Manage resources (assets) via their account program.
- Own full custody and control over deployed programs.

---

### 2. Programs

Programs are **the units of cross-timeline logic**. A program:

- Is a **DAG of effects**, each of which describes an atomic step of state transition.
- Reads and writes **resources** (tokens, balances, escrows, etc.).
- Interacts with external blockchains via **effect adapters**.
- Operates under the **temporal effect grammar**.
- Exists entirely in the **unified log** (content-addressed immutable log).

---

### 3. Account Programs

Each traveler has exactly **one account program**. This program:

- Holds all the traveler's resources (multi-timeline wallet).
- Acts as a message gateway — all **inbound and outbound messages pass through it**.
- Maintains a **causal log of all asset transfers and message exchanges**.
- Enforces **traveler-defined policies** (e.g., multi-sig, rate limits).

---

### 4. Time Keepers

Time Keepers are **trusted external observers** for each timeline. They:

- Monitor block production on the chain.
- Generate **fact proofs** (e.g., proof that a deposit was made, or a price update occurred).
- Maintain the **time map**, which records the latest observed state of all timelines.
- Serve **timeline queries** to Bandits, ensuring Bandits only act on provable facts.

---

### 5. Time Bandits

Time Bandits are the **P2P network nodes** that:

- Execute program effects.
- Validate program state transitions.
- Maintain **per-resource effect logs**.
- Generate **zero-knowledge proofs of correct execution**.
- Gossip program state and fact proofs to other Bandits.
- Optionally act as **fact observers** in simulations (when no real blockchain is involved).

---

## Data Structures

### Unified Log

The **Unified Log** records all applied program effects, resource mutations, and time map observations. It provides:

- A **complete, append-only audit trail**.
- Cryptographic links between effects (parent hashes).
- Embedded time map snapshots.
- Content addressing for replay and proofs.

### Resource Ledger

Each **resource** (token, escrow, account) has its own:

- **Causal effect log** — all state changes applied to the resource.
- **Current state** — reconstructed by replaying the effect log.
- **Lock state** — whether the resource is locked by an ongoing effect.

### Time Map

The **Time Map** is the **causal bridge to external reality**. It contains:

^^^toml
[time_map]
Ethereum.height = 123456
Ethereum.hash = "0xabc123"
Ethereum.timestamp = 1710768000
Solana.height = 87654
Solana.hash = "0xdef456"
Solana.timestamp = 1710768012
^^^

Every proposed effect references the time map that was **observed when the effect was proposed**, ensuring causal traceability between programs and external reality.

---

## Execution Model

1. **Traveler Proposal**
    - Traveler submits a message (deposit, call, withdraw) to their **account program**.
    - Account program translates the message into a **proposed effect**.
    - Proposed effect references the **current time map**.

2. **Effect Execution**
    - Time Bandits process proposed effects.
    - Bandits apply the effect to the **resource ledger**, updating balances or state.
    - Bandits generate a **zero-knowledge proof** of correct effect application.

3. **Program Coordination**
    - Programs can call other programs.
    - These calls flow through **account programs**, ensuring causal linkage.
    - Programs wait for callbacks from dependent calls before advancing.

4. **External Observations**
    - Time Keepers observe deposits, withdrawals, and external facts.
    - Time Keepers generate **fact proofs**.
    - Programs watch for these facts to trigger conditional logic.

---

## Simulation and Deployment Modes

| Mode | Description |
|---|---|
| In-Memory | Single process, all actors in one runtime. |
| Multi-Process Local | Each actor is a process; messages via IPC. |
| Geo-Distributed | Actors run on different machines, messaging over network. |

In all cases, the **effect pipeline** remains identical — only the transport changes.

---

## Program Lifecycle

| Stage | Action |
|---|---|
| Write | Traveler authors program in TEL. |
| Deploy | Traveler deploys via account program. |
| Propose | Traveler submits messages to account program. |
| Execute | Bandits execute effects and generate proofs. |
| Observe | Programs watch for external events. |
| Return | Programs return results via account program inbox. |
| Upgrade | Traveler triggers optional program upgrade. |

---

## Upgrade and Evolution

| Item | Upgrade Type |
|---|---|
| Time Bandits Protocol | Epoch-based upgrades (applies to Bandits). |
| Program Schema | Schema evolution rules or explicit migration. |
| Program Logic | Optional traveler-supplied migration. |
| Timeline Adapters | Schema-defined generation. |

Upgrades always leave **effect traces** in the unified log.

---

## Security Model

- Travelers fully own programs (no forced upgrades).  
- All messages are signed and causally linked.  
- External observations are fully proven.  
- All programs are replayable from first principles.  
- Zero-knowledge proofs prove correct execution.

---

## Key Invariants

- Programs **only talk to other programs**, never directly to travelers.
- All external effects flow through **account programs**.
- Every effect carries a **time map snapshot**.
- Every effect is causally linked to its predecessor.
- Every applied effect produces a **zk proof**.
- All logs are **content-addressed and append-only**.

---

## Architecture Diagram

+--------------+        +------------------+
| Time Traveler|        |  Account Program |
+--------------+        +------------------+
        |                            |
        | Deposit, Call, etc.        |
        v                            v
+-------------------+   +-------------------+
| External Timeline |   |    Program DAG    |
| (Ethereum, etc.)  |<->| Effects + Proofs  |
+-------------------+   +-------------------+
                ^
                |
        Observed Facts
                |
        +----------------+
        | Time Keepers   |
        +----------------+
                |
        +----------------+
        | Time Map       |
        +----------------+
                |
        +----------------+
        | Unified Log    |
        +----------------+
                |
        +----------------+
        | Time Bandits   |
        +----------------+

---

## Temporal Effect Language (TEL)

The **Temporal Effect Language (TEL)** is the primary programming language for Time Bandits. It is designed specifically for cross-timeline programming with an emphasis on explicit effects, strong typing, and causal consistency.

### Key Features

| Feature | Description |
|---|---|
| Expression-Oriented | Everything in TEL evaluates to a value, supporting compositional programming. |
| Explicit Effects | Effects that interact with external systems are clearly marked and managed. |
| Pattern Matching | Comprehensive pattern matching for data transformation and control flow. |
| Strong Typing | Static type system ensures program correctness before execution. |
| Content-Addressable | All code is identified by its content hash through the content-addressable code system. |
| First-Class Time | Native support for time relationships, delays, timeouts, and timeline interactions. |

### Core Language Components

The TEL language consists of:

1. **Values and Expressions**: Basic building blocks including literals, variables, functions, and more complex data structures.
2. **Patterns**: Patterns for destructuring and matching against values.
3. **Effects**: Explicit effect operations that interact with timelines and resources.
4. **Programs**: Collections of function definitions that can be deployed to the Time Bandits network.

### Execution Model

TEL code is interpreted by the **TEL Interpreter**, which:

1. Parses and type-checks TEL code.
2. Evaluates expressions in a causally consistent manner.
3. Manages effects through a specialized effect system.
4. Integrates with the Time Bandits runtime for deployment and execution.

### Integration with Time Bandits Core

TEL is deeply integrated with the Time Bandits core systems:

- **Resource System**: TEL provides first-class operations for working with resources.
- **Effect System**: TEL effects are processed through the Time Bandits effect handler.
- **Content-Addressable Code**: TEL leverages the content-addressable code system for immutable, hash-addressed code.
- **Time Map**: TEL programs interact with the Time Map for causally consistent timeline observations.

---

## P2P System

The **P2P Network** operated by Time Bandits is the communication, synchronization, and execution backbone of the system. It is responsible for ensuring that all Bandits (the decentralized executor nodes) collectively maintain:

- Consistent, up-to-date views of all relevant programs, effects, and facts.
- Guaranteed delivery of proposed effects and facts across the network.
- Coordination between Bandits during multi-party effect execution.
- Efficient fact propagation from Time Keepers to Bandits and Programs.
- Secure propagation of **time maps**, so all Bandits are time-synchronized with respect to observed external timelines.

### P2P Responsibilities

| Task | Description |
|---|---|
| Effect Gossip | Proposed effects are gossiped between Bandits until all relevant actors have a copy. |
| Fact Gossip | Facts observed by Time Keepers are gossiped to all Bandits. |
| Time Map Sync | Updated time maps are broadcast to all Bandits. |
| Unified Log Sync | Bandits sync portions of the unified log as needed for audit or replay. |
| Execution Coordination | For multi-bandit program execution, Bandits coordinate who executes what step. |
| Proof Exchange | Generated zk proofs are gossiped so other Bandits can verify. |

### P2P Transport Layer

The P2P system will support:

- **Direct Gossip:** For effects, facts, proofs (fast path).
- **Content Addressed Retrieval:** For fetching historical data (replay, audit).
- **Partial Subscriptions:** Bandits only subscribe to programs and timelines they care about.

---

## Transient Storage and Content Addressing

All data flowing through Time Bandits is **content-addressed**, meaning:

- Programs, Effects, Facts, and Time Maps are all **hashed into unique identifiers**.
- Data is stored in **content-addressed logs** maintained by Bandits.
- Transient storage (effect queues, message inboxes, etc.) also uses content addressing.
- When Bandits communicate, they **share content hashes first**, only fetching actual content if it's missing locally.

This provides:

- Strong deduplication.  
- Efficient gossip (only send what's missing).  
- Built-in integrity (hash mismatch = data corruption).  
- Causal traceability (each effect links to its parent hash).  

### Example Content Addressing

| Object | Hash Example |
|---|---|
| Effect | sha256:beef... |
| Time Map | sha256:dead... |
| Fact Proof | sha256:cafe... |
| Program DAG | sha256:babe... |

---

## Execution Provenance and Replayability

Every applied effect in Time Bandits is:

- **Cryptographically Proven:** A zk proof demonstrates correct execution under the effect grammar.
- **Causally Linked:** Each effect records its parent hash, creating an immutable DAG.
- **Externally Anchored:** Every effect carries a time map snapshot linking it to external timeline reality.

This means **any Bandit, Time Keeper, or external observer can fully replay and audit every program execution** by:

1. Fetching the **program definition** (content-addressed).
2. Fetching the **unified log** (content-addressed).
3. Re-fetching the **time maps** (content-addressed).
4. Validating every effect by:
    - Checking zk proof.
    - Re-running effect logic.
    - Re-validating external preconditions using time map and fact proofs.

This creates an **immutable, replayable, auditable record of every state transition** across all programs.

---

## Simulation System

The simulation system provides:

- A **configurable environment** where Time Bandits components run under controlled conditions.
- Supports three modes:
    - **In-Memory:** Single process.
    - **Multi-Process Local:** Each actor is a process (using Nix flakes for orchestration).
    - **Geo-Distributed:** Full remote deployment over SSH or other transport.

### Simulation Features

| Feature | Description |
|---|---|
| Actor Configuration | Each actor (Bandit, Keeper, Traveler) is separately configurable. |
| Time Control | Simulations can run at wall-clock speed or accelerated. |
| Fault Injection | Bandits and Keepers can be selectively crashed, partitioned, or slowed. |
| Program Deployment | Programs are injected into the system at runtime. |
| Timeline Substitution | Real blockchains can be replaced with mocks for rapid testing. |

This allows developers to:

- Fully test programs across all expected failure modes.  
- Simulate both healthy and adversarial Bandits.  
- Re-run real program histories (via replay) to validate consistency.  
- Swap simulated timekeepers with real blockchain RPC connections for hybrid testing.

---

## Fact Management and Indexing

**Facts** are external observations made by Time Keepers. These facts are:

- **Content-addressed and hashed.**
- **Linked to specific time maps (block height, hash, etc.).**
- **Gossiped to Bandits as they arrive.**
- **Stored in the unified log.**

Facts are used to:

- Trigger program logic (e.g., observe deposit arrival).
- Prove correctness of external state dependencies (e.g., balance checks).

### Fact Types

| Type | Example |
|---|---|
| Deposit Fact | Proof that 100 USDC arrived at escrow contract. |
| Price Feed Fact | Proof that ETH/USDC was $3000 at block 123456. |
| Withdrawal Fact | Proof that traveler withdrew 1 BTC. |

### Fact Indexing

To support external dashboards, UIs, and audits, facts must be:

- **Queryable by timeline, program, and resource.**
- **Exportable to JSON, TOML, etc. for external analysis.**
- **Discoverable by content hash or logical time range.**

This supports **external fact explorers** that visualize cross-timeline asset flows and program behavior.

---

### Conclusion

This **causal, content-addressed, fully replayable architecture** ensures that Time Bandits programs can:

- Execute safely across multiple independent chains.
- Prove all state transitions to external parties.
- Handle protocol and program upgrades gracefully.
- Operate across both simulation and production environments.


