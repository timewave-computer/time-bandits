# ADR-005: Concurrency Model (Updated)


## Status

**Accepted - Updated 2025-03-07**


## Context

Time Bandits programs are **distributed, cross-timeline programs** that operate across multiple blockchains and distributed ledgers. These programs must handle **asynchronous events** originating from different timelines, while maintaining **strong causal consistency** across all system components.

Concurrency in this system is uniquely challenging because:

- Programs operate across **multiple independent timelines**, each with its own clock, ordering rules, and finality guarantees.
- Each program manages **multiple resources**, each with its own causal log and timeline dependencies.
- **Cross-program and cross-timeline effects** need safe concurrency and atomicity across network and ledger boundaries.
- Time Bandits must preserve both **internal causal consistency** (program effects) and **external consistency** (timeline observations).


## Decision

### Core Principle: Resource-Scoped Concurrency

The unit of concurrency in Time Bandits is **the resource**, not the program, actor, or timeline.

- Each resource maintains its own **per-resource effect log**.
- Programs apply effects to resources, and these effects are **causally ordered per-resource**.
- Disjoint resource sets can be acted upon concurrently.
- All external resource interactions (deposits, withdrawals, transfers) flow through **account programs** — which act as boundary points for safe concurrency.

This model **closely resembles**:
- **Software Transactional Memory (STM)** (resources act like transactional cells with versioned histories).
- **Optimistic Concurrency Control** (conflicts are resolved only when effects actually apply).
- **CRDT-inspired causal graphs**, where effects may fork and merge in a provable and replayable manner.


### High-Level Principles

1. **Resource-Scoped Execution**: Programs apply effects to resources, and each resource processes its effects sequentially. Different resources can advance independently.

2. **Account Programs as Resource Managers**: External resource ownership resides in **account programs**, which expose:
    - Deposit/withdrawal interfaces.
    - Fact observation links (external deposits become observed facts).
    - Internal transfer interfaces (cross-program resource flows).

3. **Effects as Atomic Execution Units**: Each effect is an atomic state transition for a specific resource. Effects are the **fundamental units of execution and causality**.

4. **Per-Resource Effect Logs**: Each resource maintains a **content-addressed, append-only log** of all effects applied to it.

5. **Per-Resource Locks**: Effects for a given resource apply sequentially under lock. Disjoint resources do not block each other.

6. **External Consistency via Facts**: All external state (timeline balances, prices, etc.) is observed and proven by keepers. Effects depending on external facts carry **fact snapshots**, linking internal causal chains to external timeline states.


## System-Level Concurrency

At the **system level**, concurrency is managed through:

1. **Global Effect Pipeline**: Proposed effects from all programs enter a global queue.
2. **Per-Resource Scheduling**: Each effect declares the resource(s) it reads/writes. These declarations inform the scheduler.
3. **Resource Locks**: When an effect applies to a resource, the system acquires an exclusive lock on that resource.
4. **Per-Resource Logs**: Applied effects are appended to each resource’s effect log.
5. **Time Map Observations**: Effects carry **time map snapshots**, proving which external facts were observed at the time of application.
6. **Parallel Account Programs**: Each account program processes requests for its traveler independently, allowing external actor requests to scale.


## Program-Level Concurrency Primitives

Within a program, developers express concurrent workflows using **temporal combinators**, including:

1. **watch** - Observe a resource or timeline until a condition is satisfied.
2. **barrier** - Wait until multiple conditions are satisfied.
3. **race** - Execute multiple branches concurrently; return when any completes.
4. **fork** - Spawn a concurrent child program or concurrent internal branch.
5. **invoke** - Call another program asynchronously.
6. **callback** - Register a response handler for an async invocation.


## Logical Time and Causality

Time Bandits does not rely on global clocks. Instead, it establishes ordering through:

1. **Per-Resource Effect Logs** - Define causal order within each resource.
2. **Fact Snapshots** - Link effects to the external facts they observed.
3. **Program Lamport Clocks** - Track internal causal order within each program.


## Safe Concurrency Invariants

The concurrency model guarantees:

- **Per-resource sequential consistency:** Each resource sees a totally ordered sequence of effects.
- **Cross-resource causal safety:** Effects across resources only depend on each other via explicit causal links.
- **External consistency:** Each program’s external dependencies are explicitly proven via observed facts in its effect log.
- **Replayability:** Full replay of any program’s execution requires only:
    - Its own effect log.
    - The referenced fact logs.
- **No direct resource ownership:** Programs do not own resources directly. All external resources are mediated via account programs.


## Advantages

- Enables safe parallelism for disjoint resources.  
- Preserves causal consistency across programs, resources, and timelines.  
- Scales naturally with resource set size (horizontal parallelism).  
- Ensures programs remain replayable, auditable, and independently verifiable.  
- Allows external actor requests (deposits, withdrawals) to parallelize cleanly through account programs.


## Example: Cross-Chain Swap

### Program Flow

```yaml
program:
  name: "cross_chain_swap"
  resources:
    - eth_escrow
    - sol_escrow
  
  effects:
    - fork:
        - create_escrow:
            timeline: "ethereum"
            amount: 1.5
            resource: eth_escrow
        - create_escrow:
            timeline: "solana"
            amount: 35
            resource: sol_escrow
    
    - barrier:
        watch: [eth_escrow, sol_escrow]
        condition: "created"
    
    - race:
        - sequence:
            - claim:
                resource: eth_escrow
                to: "sol_user"
            - claim:
                resource: sol_escrow
                to: "eth_user"
        
        - sequence:
            - watch:
                time: "+1 hour"
            - refund:
                resource: [eth_escrow, sol_escrow]
```


### How This Executes

1. Both escrows create concurrently (disjoint resources).
2. Barrier blocks further progress until both are created.
3. Race triggers either:
    - Both escrows claimed (success).
    - One-hour timeout leading to refunds (failure).

This expresses **resource-scoped concurrency directly in the DSL**, aligning program logic with system-level scheduling.


## Architectural Integration

| Component | Role |
|---|---|
| Effect System | Effects declare resource read/write sets. |
| Resource Ledger | Tracks per-resource logs and locks. |
| Account Programs | External resource gateway, isolates traveler from direct resource ownership. |
| Time Map | Links internal effects to external facts, preserving cross-timeline consistency. |
| Temporal Effect Language | Exposes concurrency combinators (fork, watch, barrier, etc). |


## Simulation and Replay

- In **simulations**, in-memory locks simulate resource-level concurrency.
- In **local/multi-process mode**, each actor maintains independent process-local logs.
- In **geo-distributed mode**, locks translate into distributed lease/acquire operations.
- Replay uses only the effect logs (per-resource) and fact logs (per-timeline), meaning replay requires no live RPC queries.


## Example: Resource Ledger API

```haskell
data ResourceLedger = ResourceLedger
    { acquireLock :: ResourceID -> IO ()
    , releaseLock :: ResourceID -> IO ()
    , appendEffect :: ResourceID -> Effect -> IO ()
    , readEffectLog :: ResourceID -> IO [Effect]
    }
```


## Summary

- Time Bandits programs describe **concurrent goals** declaratively.  
- The system schedules actual concurrency based on **resource safety**.  
- Programs remain replayable and auditable at all times.  
- No program can violate causal consistency or race against external facts.  
- Resource-scoped concurrency fits the **account program model** directly.
