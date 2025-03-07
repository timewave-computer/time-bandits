# ADR 008: Unified Log for Facts, Effects, and Events

## Status

Proposed


## Context

In earlier versions of Time Bandits, different categories of data — facts, effects, and general events — were stored and transmitted using **separate mechanisms**. This introduced several challenges:

- **Causal Fragmentation:** Facts were not tightly coupled to the effects that relied on them.
- **Divergent Replay Paths:** Replaying effects required reconstructing facts from external timelines, breaking local replay determinism.
- **Log Inconsistency:** Each actor had its own ad hoc logging style, making it difficult to develop universal tools for visualization, auditing, and synchronization.
- **Incomplete Auditability:** There was no single **ground truth log** that captured **everything that happened** to a program in a given simulation or real deployment.
- **P2P Inefficiency:** Facts and effects were gossiped separately, even though they were causally linked.


## Decision

All actors — including programs, time keepers, account programs, and bandits — will write to a **unified append-only log**, which contains:

- **Effects:** State changes applied to programs.
- **Facts:** Observations of external data.
- **Events:** Lifecycle events like program deployment, version upgrades, safe state transitions, errors, and actor crashes/restarts.

This unified log is **content-addressed** (every entry has a unique hash), **append-only**, and **immutable** once written.


# Core Data Structures

## LogEntry

```haskell
data LogEntry = LogEntry
    { entryID :: LogEntryID
    , entryType :: LogEntryType
    , timestamp :: LamportTime
    , payload :: LogEntryPayload
    , entryHash :: Hash
    }
```

- `entryID`: Sequential or content-addressed unique identifier.
- `entryType`: Effect, Fact, or Event.
- `timestamp`: Lamport clock from local actor.
- `payload`: Actual content.
- `entryHash`: Hash of the serialized entry.


## LogEntryType

```haskell
data LogEntryType = EffectEntry | FactEntry | EventEntry
```


## LogEntryPayload

```haskell
data LogEntryPayload
    = EffectPayload Effect
    | FactPayload Fact
    | EventPayload Event
```


## Log Structure on Disk

The log is split into segments for operational flexibility, but each segment is **append-only**. Each log is **timeline-scoped** for keepers, **program-scoped** for programs, and **actor-scoped** for bandits and account programs.

- `/var/time-bandits/logs/{actor_id}/ 0001.log 0002.log`


Each segment is a list of content-addressed entries:

```json
[
    {
        "entryID": "bafy...1",
        "entryType": "FactEntry",
        "timestamp": 12345,
        "payload": { "fact": {...} },
        "entryHash": "bafy...1"
    },
    {
        "entryID": "bafy...2",
        "entryType": "EffectEntry",
        "timestamp": 12346,
        "payload": { "effect": {...} },
        "entryHash": "bafy...2"
    }
]
```


# Role in the System

| Component | Role of Unified Log |
|---|---|
| Time Keepers | Write fact observations and timeline events (e.g., reorgs). |
| Programs | Write applied effects and program lifecycle events. |
| Account Programs | Write deposit/withdrawal effects and balance events. |
| Bandits | Write proposed effects, accepted facts, and P2P events. |
| Replay Engine | Rehydrates program state by replaying only the log. |
| P2P Sync | Syncs log segments directly, not just raw effects or facts. |
| Observers | Read logs in real time to assert invariants. |


# Key Responsibilities

| Responsibility | How the Unified Log Fulfills It |
|---|---|
| Causal Linking | Every effect includes a `FactSnapshot` referencing observed facts. |
| Replayability | Program state = deterministic replay of unified log. |
| Proof of Execution | Log entries are **content-addressed** so any entry can be verified. |
| Auditability | External auditors only need the unified log — not extra context. |
| Visualization | Developers can generate timelines, DAGs, and traces directly from log. |
| Forensic Analysis | Invariant violations, safe state failures, and forks are fully reconstructible. |


# Interaction with Other Parts of the System

| System Component | Interaction with Unified Log |
|---|---|
| Program Execution | Every applied effect is written immediately to log. |
| Fact Observation | Every observed fact is written immediately to log. |
| Safe State Transitions | Every safe/unsafe state change is logged as event. |
| Schema Evolution | Every schema upgrade is logged as event. |
| Invocation Pipeline | Every cross-program invocation logs both sides of the call. |
| Observers | Observers tail the log to validate invariants. |
| P2P Synchronization | Logs are the **primary synchronization unit** between bandits. |


# Example Log Flow: Program Executes Cross-Timeline Swap

1. Trader deposits into account program.
    - `Deposit` effect logged to account log.
2. Account program records updated balance.
    - `BalanceUpdate` event logged.
3. Program queries ETH price from Ethereum Keeper.
    - Fact observed, logged to keeper log.
4. Program applies swap effect.
    - Swap effect logs to program log, with `FactSnapshot` referencing price fact.
5. Swap completes, funds transferred to Solana.
    - Solana Keeper observes incoming transfer, logs new fact.
6. Solana Account Program logs deposit effect.


# Example Log Entry: Observed Fact

```json
{
    "entryID": "bafy...1",
    "entryType": "FactEntry",
    "timestamp": 12345,
    "payload": {
        "fact": {
            "factID": "bafy...fact",
            "timeline": "Ethereum",
            "factType": "Price",
            "factValue": { "ETH/USDC": 2900 },
            "observedAt": 12345,
            "observationProof": { "inclusionProof": "0xabc...", "signedBy": "keeper.eth" }
        }
    },
    "entryHash": "bafy...1"
}
```


# Example Log Entry: Applied Effect

```json
{
    "entryID": "bafy...2",
    "entryType": "EffectEntry",
    "timestamp": 12346,
    "payload": {
        "effect": {
            "effectID": "bafy...effect",
            "type": "Swap",
            "parameters": { "fromAsset": "USDC", "toAsset": "SOL", "amount": 100 },
            "factSnapshot": {
                "observedFacts": ["bafy...fact"],
                "observer": "keeper.eth"
            }
        }
    },
    "entryHash": "bafy...2"
}
```


# Testing Plan

- Unit test log append and read.  
- Test fact observation writes facts to log.  
- Test effect application writes effects to log.  
- Test replay rebuilds state correctly from log.  
- Test cross-timeline fact/transfer logs correctly in all actors.  
- Test content-addressed integrity check (tampered logs should fail validation).


# Benefits Summary

- One unified log per actor, reducing fragmentation.  
- Single source of truth for replay, audit, and verification.  
- Content-addressed, proving each entry’s authenticity.  
- Works seamlessly across single-timeline and cross-timeline programs.  
- Consistent developer tooling — same log viewer works for all actors.  
- Directly supports `replayScenario` in new simulation system.  
- Makes external verification possible — third parties can independently audit Bandit behavior.


This unified log serves as **the system’s memory**, capturing everything a program, account, keeper, or bandit observes and does — in one consistent, provable, replayable stream.
