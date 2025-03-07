# PRD 001: Time Bandits Temporal Effect Language

## Overview

The Time Bandits system needs a specialized **Temporal Effect Language (TEL)** to describe, validate, and replay **cross-timeline programs** that interact with multiple chains, external timelines, and internal program states.

This document defines:
- Core requirements for the language.
- A synthesis of research into existing languages and paradigms that inspire TEL.
- Short code examples from each language to illustrate the kind of constructs we want to support.
- Trade-offs between options.

## Core Requirements

### Requirements

- **One-Sitting Onboarding**: A meaningful subset of the language should be learnable in a single session (under 30 minutes).
- **Visual Control Flow**: Program source should translate 1:1 into a flowchart-like diagram that represents its high-level process flow.
- **Live Execution Visualization**: As a program executes, current state, traces, and future potential paths (based on resource availability) should be representable visually.
- **Correct-by-Construction**: Invalid programs should not compile (invalid message sequences, resource misuse, unsatisfied preconditions, message type mismatches).
- **Failure Handling**: The language should make **all failure modes explicit** and provide composable mechanisms for retry, compensation, and timeout recovery.
- **Replayability & Auditability**: Programs must be fully replayable from their effect logs, and their causal graph should match exactly the declared control flow.
- **Composability**: Programs should be modular and easily combined into larger programs.
- **Strong Typing**: Programs, effects, resources, messages, and participants should all be **typed** and checked statically.
- **Global Protocol Option**: Support a **choreographic mode**, where programs are declared as **global multi-party protocols** instead of isolated functions.
- **Program as Data**: Programs should be **first-class immutable data objects**, so they can be signed, content-addressed, and versioned.
- **Cross-Version Interoperability**: Backward compatibility should be manageable when evolving the language or its runtime.
- **Support for Simulation**: The language should have native constructs for running programs in in-memory, local, and geo-distributed simulations.

## Language Inspirations & Research Survey

### 1. Move (Aptos, Sui)

**Key Attributes:**
- First-class **resources** (linear types, can’t accidentally double-spend or lose assets).
- Explicit **ownership transfers**.
- Contract-level entrypoints.
- Static **permission checks** (who can touch which resource).

**Example:**
```rust
public fun transfer(recipient: address, amount: u64): Coin {
    assert!(balance >= amount, E_INSUFFICIENT_BALANCE);
    let coin = withdraw(amount);
    send(recipient, coin);
}
```

**Link:** https://move-language.github.io/move/

**Trade-offs:**
- Strong for single-chain assets, but doesn’t natively handle **multi-timeline causality**.

### 2. Unison

**Key Attributes:**
- All code is **content-addressed**.
- Supports **distributed execution** without needing to serialize functions.
- Strong **replayability** — code and data linked by content hash.

**Example:**
```unison
deposit : Amount -> Resource -> Transaction
deposit amt res = { amount = amt, resource = res }
```

**Link:** https://www.unison-lang.org/

**Trade-offs:**
- Strong at managing **immutable history**, but doesn’t have **first-class temporal operators** (watch, timeout, barrier).

### 3. TLA+

**Key Attributes:**
- Programs = **guarded transitions**.
- Explicit **precondition checks**.
- Built for reasoning about **distributed time and state machines**.

**Example:**
```tla
Deposit == 
    /\ accountBalance' = accountBalance + depositAmount
    /\ depositConfirmed'
```

**Link:** https://lamport.azurewebsites.net/tla/tla.html

**Trade-offs:**
- Excellent for verification but **low-level**, not developer-friendly.

### 4. BPMN (Business Process Model and Notation)

**Key Attributes:**
- Programs = **flowcharts** with formal semantics.
- Strong on **visual representation**.
- Native support for **timeouts, retries, compensation**.

**Example:**
```bpmn
Start -> DepositFunds -> WaitForPrice -> ExecuteTrade -> End
```

**Link:** https://www.bpmn.org/

**Trade-offs:**
- Good for **visual control flow**, but lacks **first-class resources** and strong typing.

### 5. Blech (Bosch Language)

**Key Attributes:**
- Programs = **time-aware state machines**.
- First-class **await, watch, timeout**.
- Strong for **reactive, time-sensitive workflows**.

**Example:**
```blech
activity crossChainSwap() {
    await deposit(ETH, 100);
    await deposit(TIA, 50);
    call finalizeSwap();
}
```

**Link:** https://bosch-blech.github.io/

**Trade-offs:**
- Great for **time-sensitive logic**, but not built for **multi-party coordination**.

### 6. Choreographic Languages (Chor, Scribble, Choral)

**Key Attributes:**
- **Global-first**: Write a single global protocol, compiler generates per-actor handlers.
- All **message types checked** globally.
- Ensures **no missing or unexpected messages**.

**Example:**
```yaml
choreography: CrossChainSwap
actors: [Traveler, Account, Program]
steps:
    - Traveler deposits.
    - Account transfers to Program.
    - Program confirms.
```

**Link:** https://www.scribble.org/

**Trade-offs:**
- Extremely strong for **multi-party correctness**, but requires upfront **global design**.

### 7. Erlang Process Model

**Key Attributes:**
- Each program = **process with mailbox**.
- Messages have **timeouts and retries** built-in.
- Native **linking and supervision**.

**Example:**
```erlang
receive
    {deposit, Amount} -> apply_deposit(Amount);
    after 10000 -> handle_timeout()
end
```

**Link:** https://www.erlang.org/

**Trade-offs:**
- Strong for fault-tolerance, but no built-in **global protocol checking**.

## Trade-Off Matrix

| Language | Strong Typing | First-Class Time | Resources | Visualizable | Causal Proof | Replayable | Composable |
|---|---|---|---|---|---|---|---|
| Move | ✅ | ❌ | ✅ | ❌ | ✅ | ✅ | ✅ |
| Unison | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| TLA+ | ✅ | ✅ | ❌ | ❌ | ✅ | ✅ | ❌ |
| BPMN | ❌ | ✅ | ❌ | ✅ | ❌ | ✅ | ✅ |
| Blech | ✅ | ✅ | ❌ | ❌ | ✅ | ✅ | ✅ |
| Choreographic | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Erlang | ✅ | ✅ | ❌ | ❌ | ✅ | ✅ | ✅ |

## Design Synthesis: What Time Bandits Needs

✅ **Typed Resources** (like Move).  
✅ **Causal Proof** (like TLA+).  
✅ **Visualizable Flow** (like BPMN).  
✅ **First-Class Time Primitives** (like Blech).  
✅ **Global Protocol Option** (like Choreographic).  
✅ **Replayable and Content-Addressed** (like Unison).  
✅ **Mailbox and Supervision** (like Erlang).

## Recommended Language Direction

The Temporal Effect Language for Time Bandits should:
- Combine **choreographic programming** (global protocols for correctness) with **typed effect handlers**.
- Be **visualizable** from source to flowchart.
- Be **time-aware** (watch, await, timeout).
- Be **resource-first** (linear types or ownership).
- Be **replayable and auditable** by design.
- Emit content-addressed artifacts (for proof and verification).
- Allow **pure effect logic** separated from impure observation.
- Support **simulation-first development** (run in-memory or distributed).

# Addendum: Why Choreographic Languages Are a Perfect Fit for Time Bandits

## The Core Insight: Global Programs, Local Execution

Choreographic programming offers a **global-first approach** to defining how multiple independent actors interact. Instead of writing separate programs for each participant (Travelers, Keepers, Bandits, etc.), a **single program describes their entire interaction flow**.

From this **one global truth**, the system generates **per-actor handlers**, ensuring:
✅ **No missing messages** (compile-time checked).  
✅ **No unexpected messages** (every message is expected).  
✅ **No protocol drift** (all participants get code derived from the same protocol).  
✅ **Causal correctness by design** (the program defines the allowed order of events).  

### Why This Fits Time Bandits

| Choreographic Concept | Time Bandits Need |
|---|---|
| **A single global program defines how actors interact.** | Travelers, Bandits, and Keepers follow well-defined protocols. |
| **Compile-time message correctness.** | No risk of format mismatches or missing messages. |
| **Causal execution is built-in.** | Ensures resources are locked and used in the right order. |
| **Each participant sees only the messages they need.** | Programs are scoped to their relevant role. |
| **Global protocols translate into causal graphs.** | Direct 1:1 mapping with Time Bandits' execution logs. |
| **Replayability and formal verification.** | Logs + protocol give a full proof of execution. |
| **Visualization is trivial.** | Every program can render directly into a flowchart. |

---

## Addendum: Why a Choreographic Language is a Great Fit for Time Bandits

### The Core Insight: Global Programs, Local Execution

Choreographic programming offers a **global-first approach** to defining how multiple independent actors interact. Instead of writing separate programs for each participant (Travelers, Keepers, Bandits, etc.), a **single program describes their entire interaction flow**.

From this **one global truth**, the system generates **per-actor handlers**, ensuring:
✅ **No missing messages** (compile-time checked).  
✅ **No unexpected messages** (every message is expected).  
✅ **No protocol drift** (all participants get code derived from the same protocol).  
✅ **Causal correctness by design** (the program defines the allowed order of events).  

### Why This Fits Time Bandits

| Choreographic Concept | Time Bandits Need |
|---|---|
| **A single global program defines how actors interact.** | Travelers, Bandits, and Keepers follow well-defined protocols. |
| **Compile-time message correctness.** | No risk of format mismatches or missing messages. |
| **Causal execution is built-in.** | Ensures resources are locked and used in the right order. |
| **Each participant sees only the messages they need.** | Programs are scoped to their relevant role. |
| **Global protocols translate into causal graphs.** | Direct 1:1 mapping with Time Bandits' execution logs. |
| **Replayability and formal verification.** | Logs + protocol give a full proof of execution. |
| **Visualization is trivial.** | Every program can render directly into a flowchart. |

---

## What a Time Bandits Choreographic Language (TBCL) Might Look Like

### **1. Choreography as a Global Protocol**
A **TBCL program** would look something like this:

```yaml
choreography: CrossChainSwap

participants:
    - Traveler
    - EthereumAccountProgram
    - CelestiaAccountProgram
    - SwapProgram
    - SettlementProgram

protocol:
    - Traveler deposits 100 USDC into EthereumAccountProgram.
    - EthereumAccountProgram transfers USDC to SwapProgram.
    - Traveler deposits 50 TIA into CelestiaAccountProgram.
    - CelestiaAccountProgram transfers TIA to SwapProgram.
    - SwapProgram waits for both escrows.
    - SwapProgram calls finalizeSwap() on SettlementProgram.
    - SettlementProgram confirms and sends result to Traveler's EthereumAccountProgram.
```

### **2. Compiler-Generated Per-Actor Code**
From the **single protocol**, the compiler generates **separate effect handlers** for each actor.

#### **EthereumAccountProgram’s Generated Code**
```haskell
handleMessage (Deposit USDC 100) =
    transfer USDC 100 SwapProgram
```

#### **SwapProgram’s Generated Code**
```haskell
onMessage (Receive USDC) =
    if received(TIA) then finalizeSwap()
```

This ensures each participant **only sees the logic relevant to them**.

---

## **How This Enhances Program Execution & Visualization**

A Time Bandits program must be:
1. **Readable as a program.**
2. **Visualizable as a control-flow diagram.**
3. **Visualizable as an execution trace.**
4. **Replayable deterministically.**

Choreographic programming **directly enables** all four:

| Requirement | How Choreography Helps |
|---|---|
| **Readable** | The full process is declared in a single place. |
| **Visualizable** | The protocol can be rendered **directly** into a graph. |
| **Execution Traceable** | Each participant follows a script derived from the same protocol. |
| **Replayable** | Logs reconstruct the exact execution history. |

---

## **Correctness & Safety Guarantees**

Time Bandits needs **correct-by-construction programs**. Choreographic languages provide **built-in correctness** via:

✅ **Causal Ordering** → Events happen **only in the declared sequence**.  
✅ **Message Safety** → No risk of sending unexpected messages.  
✅ **Effect Composition** → Programs can be **composed** without breaking correctness.  
✅ **Typed Resources & Participants** → Every actor only handles **valid messages**.  

### **Example: Preventing an Invalid Execution**
Suppose a traveler **forgets** to deposit funds before invoking a swap.

- In a **normal imperative system**, this might cause a runtime failure.
- In **TBCL**, this is a **compile-time error** because the deposit **must** occur before the swap.

The compiler **enforces**:
```yaml
protocol:
    - Traveler deposits -> ✅ Required step
    - Swap is triggered -> ❌ Invalid if deposit missing (compile error)
```

---

## **Handling Failures Gracefully**
Since all message flows are **declared explicitly**, failures are **first-class citizens**.

### **Built-In Recovery Mechanisms**
- **Timeouts** → If a deposit doesn’t arrive within 15 minutes, **auto-refund**.
- **Fallback Paths** → If a swap fails, execute **rollback** transactions.
- **Dispute Resolution** → A dispute case can be part of the protocol.

### **Example: Adding Failure Handling**
```yaml
protocol:
    - Traveler deposits 100 USDC.
    - If deposit does not arrive in 15m, refund.
    - SwapProgram waits for deposits.
    - If one deposit arrives but the other doesn’t, refund.
```

**How This Helps**
✅ **Every failure mode is declared upfront.**  
✅ **No undefined failure states.**  
✅ **Programs are guaranteed to handle failures correctly.**  

---

## **Replayability & Verification**
Because all Time Bandits programs are **immutable effect logs**, choreographic programming **fits perfectly**:
- Every execution step is **linked to its causal parent**.
- Every message flow is **verifiable** against the original protocol.
- **Replay is trivial** → The logs **directly match** the declared process.

### **Example: Verifying Execution Logs**
Imagine a **log of program execution**:

