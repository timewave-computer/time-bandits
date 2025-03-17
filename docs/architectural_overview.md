# Architectural Overview - Time Bandits

---

## Overview

**Time Bandits** is a decentralized execution system for **cross-timeline programs** — programs that operate across **multiple independent blockchains and distributed ledgers**, referred to collectively as **timelines**. Time Bandits enables **secure, replayable, causally-consistent execution of these programs** without requiring trust in any central party.

The system achieves this by:

- Defining **temporal effect programs** that describe **time-coupled logic** across multiple chains.
- Operating a **P2P network** where Bandit nodes execute effects and generate **zero-knowledge proofs of correct execution**.
- Relying on **time maps**, a causally-consistent snapshot of external blockchain states, to ground program effects in provable reality.
- Enforcing **resource conservation** through formalized resource models with mathematical guarantees.
- Implementing **dual validation** (temporal and ancestral) for cross-timeline operations.

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
- Reads and writes **formalized resources** with explicit conservation laws.
- Interacts with external blockchains via **effect adapters**.
- Operates under the **temporal effect grammar**.
- Exists entirely in the **unified log** (content-addressed immutable log).

---

### 3. Account Programs

Each traveler has exactly **one account program**. This program:

- Holds all the traveler's resources using **formalized resource tuples**.
- Acts as a message gateway — all **inbound and outbound messages pass through it**.
- Maintains a **causal log of all asset transfers and message exchanges**.
- Enforces **traveler-defined policies** (e.g., multi-sig, rate limits).
- Ensures **resource conservation** through delta validation (∆TX = 0).
- Tracks **controller labels** for cross-timeline resources.

---

### 4. Time Keepers

Time Keepers are **trusted external observers** for each timeline. They:

- Monitor block production on the chain.
- Generate **fact proofs** (e.g., proof that a deposit was made, or a price update occurred).
- Maintain the **time map**, which records the latest observed state of all timelines.
- Serve **timeline queries** to Bandits, ensuring Bandits only act on provable facts.
- Implement the **Controller interface** for their respective timelines.
- Can **endorse** other controllers' states to enable state reduction optimizations.

---

### 5. Time Bandits

Time Bandits are the **P2P network nodes** that:

- Execute program effects.
- Validate program state transitions.
- Verify resource conservation through **delta validation**.
- Check **dual validation** (temporal and ancestral) for cross-chain operations.
- Gossip updates to other Bandits.
- Generate and verify proofs.

---

### 6. Resources

Resources are **formalized tuples** that:

- Have explicit **predicates** controlling consumption.
- Belong to **fungibility domains** that determine equivalence.
- Contain a **quantity** representing their amount.
- Include **metadata** for associated data.
- Generate **commitments** and **nullifiers** for security.
- Track their **controller history** when crossing timelines.
- Follow **conservation laws** (∆TX = 0) across all operations.

---

### 7. Controllers

Controllers are **timeline managers** that:

- Enforce the rules of their respective timelines.
- Are classified as **Safe**, **Live**, or **Byzantine** based on security properties.
- Maintain **state roots** of their timelines.
- Can **endorse** other controllers to enable state reduction.
- Serve as fallbacks in case of controller failures.
- Provide **controller labels** for cross-timeline resource tracking.

---

## Execution Model

### Resource Balancing

All operations in Time Bandits must **conserve resources** according to formal laws:

1. Every **effect** that creates or consumes resources has an associated **delta**.
2. The **net delta** across all effects in a transaction must be **zero** (∆TX = 0).
3. Resources are tracked using **commitments** and **nullifiers** to prevent double-spending.

### Dual Validation

Cross-timeline operations undergo two types of validation:

1. **Temporal Validation**: Ensures causal consistency using time maps, verifying that observations respect causal order.
2. **Ancestral Validation**: Verifies resource provenance through controller labels, ensuring valid history across timelines.

An operation is only valid if **both validations pass**, providing defense in depth against various attacks.

---

## System Architecture

The Time Bandits system is built around a **content-addressable execution model**, where:

- Every code entity has a **unique hash** based on its content.
- All **effects** and **resources** have **cryptographic commitments**.
- The **execution history** is fully **replayable** from program inception.
- **Resources** and **timelines** form a **bipartite graph** connected by **effects**.
- **Controllers** provide explicit trust models for cross-timeline operations.

---

## Resource Flow

Resources flow through the system according to these principles:

1. **Conservation**: Resources cannot be created or destroyed, only transformed or transferred.
2. **Formalized Definition**: Resources have explicit mathematical properties defined in tuples.
3. **Controller Tracking**: When resources cross timelines, their controller history is recorded.
4. **Delta Validation**: All operations must balance to zero, ensuring conservation.
5. **Nullifier Prevention**: Once consumed, resources cannot be reused thanks to nullifiers.

---

## Cross-Timeline Execution

When a program spans multiple timelines:

1. Time Keepers observe facts on each timeline.
2. Account programs hold resources for each timeline.
3. Bandits compose effects across timelines, ensuring causality.
4. Resources crossing timelines maintain controller labels for provenance.
5. Dual validation ensures integrity across chains.
6. Controller endorsements enable efficiency optimizations.

---

## Time Bandits Core Language

Time Bandits programs are written in **Temporal Effect Language (TEL)**, which:

- Is **purely functional** with explicit effects.
- Treats **time** as a first-class concept.
- Has strong **static typing**.
- Employs **resource formalization** for tracking assets.
- Ensures **delta validation** for all resource operations.
- Supports **dual validation** for cross-timeline transfers.

---

## Security Model

The security of Time Bandits relies on:

1. **Content-Addressable Code**: Immutable, verifiable programs.
2. **Causal Consistency**: Effects respect timeline order.
3. **Cryptographic Proofs**: Validations can be verified.
4. **Resource Conservation**: Formal mathematical guarantees.
5. **Controller Classification**: Explicit trust models for timelines.
6. **Dual Validation**: Defense in depth against various attacks.
7. **Resource Commitments and Nullifiers**: Prevent double-spending.

---

## Developer Experience

Developers interact with Time Bandits by:

1. Writing **TEL programs** with explicit time awareness.
2. Defining **resources** using the formalized model.
3. Creating **controller labels** for cross-timeline transfers.
4. Deploying to **account programs** that validate conservation.
5. Running **simulations** to test cross-timeline behavior.
6. Monitoring **execution** through time maps and effect logs.
7. Verifying **conservation** through delta validation.

---

This architecture enables developers to build robust cross-timeline applications with strong formal guarantees about state consistency, resource conservation, and security.


