# ADR-005: Concurrency Model

## Status

Accepted

## Context

Time Bandits programs are distributed, cross-timeline programs that operate across multiple blockchains and distributed ledgers. These programs must handle asynchronous events from different timelines while maintaining strong causal consistency across the entire system. The concurrency model must enable safe parallelism while preserving the security properties of the system.

Time Bandits presents unique challenges for concurrency:
- Programs must remain causally consistent across independent timelines
- Each program manages multiple resources with their own causal logs
- Cross-program effects need safe concurrency without deadlocks
- Time map observations provide external consistency

## Decision

We will implement a resource-scoped concurrency model with the following high-level principles:

Programs describe concurrent goals. The system resolves actual concurrency based on what’s causally safe.

This mirrors:

- STM (software transactional memory) in Haskell.
- Optimistic concurrency in databases (conflict resolution at commit time).
- CRDT-like causal graphs where effects can fork and merge.

### High-Level Principles

1. **Resource-Centric Concurrency**: The unit of concurrency is the resource, not the program or thread. Each resource can be updated independently and concurrently with other resources.

2. **Account Programs as Gateways**: All external effects must flow through account programs, which manage the concurrency of external interactions.

3. **Effects as Unit of Execution**: Each effect is an atomic unit of execution that updates a resource. Effects are applied in sequence to each resource but can be applied concurrently to different resources.

4. **Per-Resource Causal Logs**: Each resource maintains its own causal log, which records the sequence of effects applied to it. These logs provide the foundation for causal consistency.

### System-Level Concurrency

At the system level, concurrency is managed through:

1. **Global Effect Queue**: Proposed effects are placed in a global queue and processed in parallel when possible.

2. **Per-Resource Locks**: When an effect targets a specific resource, the system acquires a lock on that resource.

3. **Per-Resource Logs**: Effects are applied to resources in the order specified by their causal logs.

4. **Time Map Observations**: Effects include the time map they observed, ensuring causal consistency with external timelines.

5. **Account Program Isolation**: Each account program operates independently, allowing concurrent processing of actor requests.

### Program-Level Concurrency Primitives

Programs can express concurrency through several primitives:

1. **watch**: Observe a resource or timeline until a condition is met.
2. **barrier**: Wait for multiple watches or forks to complete.
3. **race**: Execute multiple branches concurrently, returning when any completes.
4. **fork**: Start a concurrent execution path without waiting.
5. **invoke**: Call another program, potentially triggering concurrent execution.
6. **callback**: Register a function to be called when an event occurs.

### Logical Time and Causality

Time Bandits programs operate without a global clock, instead relying on:

1. **Causal Logs**: Each resource's log defines a partial ordering of effects.
2. **Time Map Snapshots**: Effects are linked to external timeline observations.
3. **Lamport Timestamps**: Internal logical clocks maintain causal order.

## Consequences

### Summary of Concurrency Model Invariants

The concurrency model ensures:

1. Per-resource locking prevents concurrent updates to the same resource.
2. Disjoint effects (affecting different resources) can apply concurrently.
3. The history of each resource is totally ordered and replayable.
4. Cross-program effects maintain causal consistency through account programs.
5. Time map observations provide external consistency with independent timelines.

### Advantages

- Enables high throughput for programs with disjoint resource sets
- Preserves causal consistency across the entire system
- Allows safe parallel execution without complex locking schemes
- Creates a predictable and replayable execution model

### Limitations

- Resource-level granularity may limit parallelism for some workloads
- Requires careful resource design to avoid contention
- Complex cross-resource dependencies can reduce concurrency

## Implementation

### Example: Cross-Chain Swap with Parallel Escrows

```yaml
program:
  name: "cross_chain_swap"
  resources:
    - eth_escrow
    - sol_escrow
  
  effects:
    - # Parallel escrow creation
      fork:
        - create_escrow:
            timeline: "ethereum"
            amount: 1.5
            resource: eth_escrow
        - create_escrow:
            timeline: "solana"
            amount: 35
            resource: sol_escrow
    
    - # Barrier waits for both escrows
      barrier:
        watch: [eth_escrow, sol_escrow]
        condition: "created"
    
    - # Race condition for timeout or completion
      race:
        - # Happy path - both claim
          sequence:
            - claim:
                resource: eth_escrow
                to: "sol_user"
            - claim:
                resource: sol_escrow
                to: "eth_user"
        
        - # Timeout path - refund both
          sequence:
            - watch:
                time: "+1 hour"
            - refund:
                resource: [eth_escrow, sol_escrow]
```

In this example:
- Two escrows are created in parallel (fork)
- A barrier ensures both must be created before proceeding
- A race allows either successful completion or timeout handling

### Architecture Integration

The concurrency model integrates with the broader Time Bandits architecture:

1. **Effect System**: Each effect declares its resource read/write set
2. **Resource Ledger**: Maintains per-resource logs and locks
3. **Time Map**: Provides external timeline consistency
4. **Account Programs**: Act as concurrency boundaries for external actors

### Summary

The resource-scoped concurrency model enables safe parallelism while maintaining causal consistency. By focusing on resources as the unit of concurrency, Time Bandits programs can achieve high throughput while preserving their security properties.