# Concurrency Model

## 1. How Time Bandits Thinks About Concurrency

Concurrency here means:

Multiple programs (or multiple parts of a single program) operating concurrently on the same time map and resource graph, either in cooperation or competition.

This is NOT "parallel execution on a CPU" — it's about logical concurrency over shared state in a distributed, multi-timeline environment.

## 2. Key Concurrency Properties to Support

| Property | Description |
| --- | --- |
| Composability | One program can invoke and interact with other programs. |
| Isolated Causal State | Each program has its own Lamport clock & memory. |
| Cross-Program Escrow | Programs can deposit resources into each other's memory. |
| Conditional Triggers | Programs can automatically react to state changes in other programs. |
| Capability Transfer | A program can grant (or revoke) partial control over its resources to another program. |
| Atomic Batch Execution | Multiple effects can be applied atomically if they touch disjoint resources. |
| Partial Ordering | Programs don't need to globally synchronize unless they depend on each other. |

## Concurrency Primitives

Here's a set of primitives you should consider exposing to programs via the effect language. This allows flexible composition without global locks or serialization.

### 1. Resource Escrow Between Programs

#### Effect
```haskell
EscrowToProgram
    :: Resource
    -> ProgramId    -- Target program
    -> MemorySlot   -- Slot in target program's memory
```

#### Meaning
- Move a resource into another program's memory.
- The sending program releases control.
- The receiving program gains ownership.

### 2. Program Resource Invocation (Cross-Program Call)

#### Effect
```haskell
InvokeProgram
    :: ProgramId    -- Target program
    -> FunctionName -- Name of entry point
    -> [Resource]   -- Arguments (held by caller)
```

#### Meaning
- One program calls another.
- Like a remote procedure call (RPC) over the resource graph.
- Target program decides whether to accept.
- Ownership of provided resources transfers on success.

### 3. Capability Delegation

#### Effect
```haskell
DelegateCapability
    :: Capability
    -> ProgramId
    -> Expiry
```

#### Meaning
- A program can grant another program the right to act on its behalf for certain resources.
- This supports multi-party workflows where program A temporarily trusts program B.

### 4. Conditional State Watch

#### Effect
```haskell
WatchResource
    :: ResourceKey
    -> Condition     -- Balance above X, or state change
    -> Trigger       -- Effect to apply if condition holds
```

#### Meaning
- Program installs a watcher on another program or timeline.
- When the condition triggers, the program can react without active polling.

### 5. Atomic Multi-Effect Transaction

#### Effect
```haskell
AtomicBatch
    :: [Effect]
```

#### Meaning
- Apply several effects as a unit, if they touch disjoint resources.
- This allows local atomicity without blocking unrelated programs.

### 6. Claim Resource From Another Program

#### Effect
```haskell
ClaimFromProgram
    :: ProgramId
    -> MemorySlot
    -> Resource
```

#### Meaning
- If program B explicitly allowed it (via a delegation or escrow), program A can directly claim a resource from B.

### 7. Abort & Revoke

#### Effect
```haskell
RevokeResource
    :: ProgramId
    -> MemorySlot
```

#### Meaning
- If program A escrowed something to B, A retains the right to revoke if B hasn't yet consumed it.
- This allows timeout-style recovery.

### 8. Join/Barrier Synchronization (Optional)

#### Effect
```haskell
JoinWith
    :: [ProgramId]
```

#### Meaning
- Program blocks until all listed programs reach compatible state (optional).
- This is a stronger synchronization primitive — should only be used when strictly necessary.

## Concurrency Safety Invariants

| Invariant | Description |
| --- | --- |
| No Unscoped Access | Programs only touch other programs if explicitly allowed. |
| No Double Spend Across Programs | No resource can leave two programs at once. |
| Escrow = Atomic | Either a resource fully transfers or the escrow is aborted. |
| Cross-Program Invocations = Causal | No out-of-order invocation allowed. |
| Watches are Deterministic | Same input = same trigger firing. |

## Concurrency Model Summary

| Primitive | When to Use |
| --- | --- |
| EscrowToProgram | One program provides a resource to another. |
| InvokeProgram | One program calls another program's entry point. |
| DelegateCapability | One program grants limited rights to another. |
| WatchResource | One program watches a timeline or another program. |
| AtomicBatch | Atomic local actions. |
| ClaimFromProgram | Recover escrow or claim by delegation. |
| RevokeResource | Cancel unclaimed escrow. |
| JoinWith | Multi-program coordination (rare case). |

## Concurrency Graph Model

Each program can be seen as a node in a dependency graph, with:

- Resources as edges (flows of ownership).
- Watches as reactive edges (event-driven triggers).
- Invokes as direct call edges (causal triggers).

This graph defines the dynamic, evolving process network within each time map.

## Concurrency Guarantees in ZK Proofs

Every transition proof should:

- State which programs were involved.
- Show how ownership changed.
- Prove that all effects respected the declared capability model.
- Prove causal consistency (invokes only call programs that existed in that time map snapshot).

## Why This Matches the System Contract

| System Invariant | Supported By |
| --- | --- |
| Cross-Program Isolation | Escrow, delegation, explicit calls |
| Causal Order | Per-program counter + invocation chain |
| No Implicit Access | All program interaction via declared effect |
| ZK Proof for Everything | Every cross-program flow provable |
| Replayable | All cross-program calls leave traces |

## Concurrency Model Summary Diagram

```
[Program A] --> [Program B]
   |
   +---escrows 100 Gold to B
   |
   +---delegates claim right to C

[Program C] --> Claims escrow from B
   |
   +---watches for Silver deposit in B
   |
   +---calls B when Silver arrives
```

This is a causal, concurrent, capability-checked graph — no program can spontaneously act without being explicitly empowered to do so. This is exactly the secure composability you want for Time Bandits.
