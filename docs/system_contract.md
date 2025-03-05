System Contract for Time Bandits
Version: 1.2
Date: 2025-03-05

# System Contract

## Purpose

This document defines the **invariants, ownership rules, causal consistency guarantees, and actor/program boundary rules** for Time Bandits. It acts as a formal contract that Time Bandits actors (Travelers, Keepers, Bandits) must uphold, and that all program execution must respect.

---

## Core Ownership Rule

All resources in Time Bandits are **owned exclusively by programs**. No actor directly owns resources. This ensures:
- There is always an explicit log of how a resource’s ownership changed.
- Every ownership change is accompanied by an effect and proof.
- Resource ownership is tied directly to programs, making ownership history fully replayable.

---

## Actor-Program Separation

Actors (time travelers) do not directly own or interact with resources within Time Bandits. Instead, each actor has exactly **one Account Program**. This Account Program acts as the actor’s **sole point of entry into the system**. It is responsible for:
- Holding all resources owned by the actor.
- Sending deposits to external programs.
- Receiving withdrawals and refunds from external programs.
- Receiving callbacks from programs after requested work completes.
- Maintaining a structured **inbox and outbox**, recording all communication the actor has with the system.

**Invariants:**
- An actor may only propose effects via their Account Program.
- No external program may send a message directly to an actor — all responses flow through the Account Program.
- All assets held by an actor exist within the Account Program’s state.

---

## Resource Ledger

Resource ownership is tracked globally in a **Resource Ledger**, which maps each resource to its current owner (a program ID). This ledger enforces:
- Each resource has exactly **one owner** at all times.
- Ownership changes only via applied, proven effects.
- The owner can be either a regular program or an account program.
- Ownership history is auditable by walking the **per-resource execution log**.

---

## Effect Application and Causal Consistency

Effects are the **only mechanism for changing program state, resource ownership, and timeline observations**.  
Each effect:
- Declares a **read set** (resources and facts it observes).
- Declares a **write set** (resources it wants to modify).
- References a **specific time map snapshot** that it observed at the time the effect was proposed.

Effects apply only if:
- The observed time map is still valid (no unexpected external changes occurred).
- All preconditions hold (balances, capabilities, external facts).
- All resources in the effect’s write set are **locked by the effect** at the time of application.

**Concurrency Rule:**  
Effects that touch **disjoint resources** may apply concurrently, even within the same program.

---

## Per-Resource Logs and Replayability

Each resource maintains its own **per-resource execution log**, recording:
- Every effect that modified the resource.
- The time map snapshot the effect observed.
- The resulting resource state hash.
- A zero-knowledge proof (if applicable) demonstrating valid application.

A program’s complete history is the **union of its resource logs** plus any internal state changes applied to program memory.

**Invariant:**  
Replaying all per-resource logs, in causal order, must reconstruct the program’s state and memory exactly.

---

## Programs as Effects and Memory

A program itself is:
- A static list of possible **effects** (its "code").
- Mutable **program memory**, which can only be changed by internal state effects.
- A set of **owned resources**, tracked in the resource ledger.
- A set of **per-resource logs**, forming a causal graph of applied effects.

**Invariant:**  
A program’s memory and resources may only change via applied effects.  
Effects that modify different resources within the same program may apply concurrently.

---

## Time Map and Observed Facts

Every applied effect records the **time map snapshot** it observed.  
This is the **causal anchor** that guarantees every effect was applied in the correct context (e.g., with the correct external balances and proofs).

**Invariant:**  
An effect’s time map must match the latest known time map at the time of application — if not, preconditions must be re-evaluated before applying.

---

## Program-Program and Program-Account Communication

All cross-program communication flows via effects.  
Programs may:
- Send messages to other programs.
- Send callbacks to account programs.
- Deposit/withdraw funds to/from account programs.

Programs may **not**:
- Directly interact with external actors.
- Directly interact with external wallets.

---

## Account Program Interface

The Account Program exposes the following standard effects:

| Effect | Description |
|---|---|
| `Deposit` | Moves resources into a target program. |
| `Withdraw` | Withdraws resources from a program back to the account. |
| `Transfer` | Transfers resources to another account program. |
| `Watch` | Watches for an external deposit into the account program (cross-chain ingress). |
| `Invoke` | Sends a cross-program call, capturing the response into the account’s inbox. |
| `ReceiveCallback` | Accepts a callback from a program, appending it to the inbox. |
| `CustomMessage` | Allows arbitrary structured messages to flow through the account, providing extensibility for higher-level workflows. |

**Invariant:**  
Actors only communicate by proposing effects that target their own Account Program.  
All messages to/from programs pass through the Account Program’s **inbox and outbox**.

---

## Simulation Consistency Rule

All of these rules — causal effect application, resource ownership, account programs as gateways — must hold **in every simulation mode**:
- In-memory (single process)
- Multi-process local simulation
- Geo-distributed simulation

This ensures that causal consistency, replayability, and cross-program interaction behave identically across development, testing, and production deployment.

---

## Security and Proof Invariant

Every applied effect, once committed to the log, must:
- Be accompanied by a proof demonstrating that preconditions held, time map was valid, and the effect was correctly applied.
- Be verifiable from first principles using only the program’s static code, observed time map, and resource ledger.
- Be **immutable and content-addressed**, meaning the applied effect’s hash is its permanent identity.

---

## Summary of Key System Invariants

| Invariant | Description |
|---|---|
| Resource Ownership | Every resource has exactly one program owner at all times. |
| Actor-Program Separation | Actors only interact via account programs. Programs only talk to other programs. |
| Effect-Only State Changes | All changes (memory, resources, logs) must flow through proven effects. |
| Per-Resource Logs | Every resource tracks its own causal history, forming a causal DAG. |
| Causal Consistency | Every effect observes a specific time map; preconditions are rechecked if the time map changes. |
| Replayability | Replaying all logs reconstructs state exactly. |
| Proof Completeness | Every applied effect has a proof that can be independently verified. |
| Simulation Parity | These invariants hold across all simulation modes (in-memory, multi-process, geo-distributed). |

---

This system contract reflects the **latest design shift to resource-centric concurrency, account programs, and effect-based messaging**, and supersedes previous assumptions that programs were strictly single-threaded.
