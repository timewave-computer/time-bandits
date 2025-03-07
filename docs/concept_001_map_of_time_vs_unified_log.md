# Concept Reference: Relationship Between the Map of Time and the Unified Log


## Overview

This document explains the relationship between **the Map of Time** and **the Unified Log** in the Time Bandits system. While both relate to the **ordering of events**, they exist at different **levels of abstraction** and serve distinct but complementary purposes.


## The Unified Log: Local, Concrete History

### What It Is

The **Unified Log** is the **local, append-only record** maintained by each actor (program, account program, time keeper, bandit) that records **what that actor did or observed**. Every actor has its own **separate log**.

### What It Contains

Each entry records one of:

- An **effect** applied by the actor.
- A **fact** observed by the actor (in the case of keepers).
- A **lifecycle event** (such as schema evolution or safe state transition).

### Key Attributes

- **Actor-scoped:** Each actor has its own log.
- **Append-only:** New entries are appended, never overwritten.
- **Content-addressed:** Each entry has a unique hash (CID) derived from its content.
- **Immutable:** Once written, entries cannot be modified.
- **Replayable:** Replaying the log fully reconstructs actor state.


## The Map of Time: Global, Abstract Causal Graph

### What It Is

The **Map of Time** is a **causal graph** that spans across all actors and timelines. It shows the **causal relationships** between:

- Observed facts (external timeline events observed by keepers).
- Applied effects (program state transitions triggered by facts or prior effects).
- Cross-program invocations (causal links between programs).
- Cross-timeline operations (causal links between facts on different chains).

The Map of Time is **derived** from the logs — it is **not stored directly**, but can always be recomputed from the collection of all Unified Logs.


### What It Contains

Each node in the Map of Time is:

- A fact (observation of external state by a Time Keeper).
- An effect (a state transition applied by a program).
- An event (like schema evolution or safe state transition).

Each edge in the Map of Time records a **causal dependency**:

- This effect **depended on** this fact.
- This effect **followed from** this other effect.
- This invocation **caused** this response.


### Key Attributes

- **Global-scope:** Spans all programs, timelines, and actors.
- **Derived:** Computed from the logs, not stored directly.
- **Immutable:** The causal graph does not change once events are observed.
- **Explorable:** Programs and tools can query the graph to understand causal relationships.


## How They Relate

| Property | Unified Log | Map of Time |
|---|---|---|
| Scope | Per-actor | Global (across actors and timelines) |
| Content | Concrete entries (facts, effects, events) | Abstract causal edges between events |
| Storage | Written at runtime | Derived from logs |
| Immutability | Append-only | Immutable (causal order never changes) |
| Causality | Implied by sequence numbers and parent hashes | Explicit graph structure |
| Replay Role | Directly replayed to reconstruct state | Checked to validate replay order |
| Visualization Role | Raw data source | High-level causal diagram |
| Audit Role | Proof of execution | Proof of causal integrity |
| External Queries | Served directly by the actor | Constructed for external visualization or audit |


## Analogy

| Concept | Analogy |
|---|---|
| Unified Log | Personal diary of each actor |
| Map of Time | History textbook weaving all personal diaries into a coherent story |


## Example Flow: Cross-Timeline Swap

1. Ethereum Keeper observes a `PriceFact`.
    - Writes this fact into its **unified log**.
    - Gossips the fact to Bandits.
2. A trading program reads the fact and performs a `SwapEffect`.
    - Writes this effect into its **unified log**.
3. The program submits a transfer to Solana.
4. Solana Keeper observes incoming transfer.
    - Writes this fact into its **unified log**.
5. The Map of Time links:
    - Ethereum fact to Ethereum program effect.
    - Ethereum program effect to Solana fact.

In this case:

| Log | Content |
|---|---|
| Ethereum Keeper Log | Observed PriceFact |
| Ethereum Program Log | Applied SwapEffect |
| Solana Keeper Log | Observed TransferFact |

The **Map of Time** links these into a coherent causal chain:

^^^
PriceFact (Ethereum) -> SwapEffect (Program) -> TransferFact (Solana)
^^^


## Important Clarification: The Map of Time is Reconstructible

- No actor "owns" the Map of Time.
- It can be fully reconstructed at any time by scanning all Unified Logs and following their causal references (parent effects, observed facts).
- This is crucial for external audits — an auditor can build the Map of Time independently from the logs, without needing special permissions.


## Timekeeper Changes and Unified Log Construction

- In production, real blockchain nodes will replace simulated timelines.
- Time Keepers may become **stateless proxies**, only relaying facts without writing logs themselves.
- Bandits (or any external party) can reconstruct the keeper’s FactLog using:
    - Public blockchain data.
    - The same fact extraction rules used by the keeper.

This means the Map of Time can always be reconstructed as long as:

- Unified Logs for programs exist.
- Blockchain data is available.
- Fact extraction rules are known.


## Benefits of This Separation

| Benefit | Explanation |
|---|---|
| Minimal Time Keeper | Keepers focus on observation, not storage. |
| Flexible Fact Sources | Facts can come from real chains, simulated chains, or external providers. |
| Verifiability | Anyone can reconstruct the Map of Time. |
| Strong Replayability | Unified Logs remain self-sufficient for program replay. |
| Simplified Upgrades | Changing Map of Time logic does not change log formats. |


## Key Invariant

- Every effect in a program’s Unified Log must reference the **complete FactSnapshot** describing the external facts it depended on.  
- Every FactSnapshot must consist only of **facts that exist in some actor’s Unified Log**.  
- The Map of Time must always be reconstructible from the collection of all Unified Logs.


## Example Visualization

The **Map of Time** weaves together events from:

- Ethereum Keeper Log.
- Solana Keeper Log.
- Program Logs (for each deployed program).

This produces a visualization like:

```
Ethereum: Fact: ETH/USDC price = 2900
↓
Program A: Effect: Swap USDC → SOL
↓
Solana: Fact: SOL transfer received
```

This separation of **concrete logs** from **abstract causal graph** ensures:
- Time Bandits can work across simulated and real timelines.
- External auditors can validate both individual logs and global causal integrity.
- All actors remain sovereign, and logs are fully portable.