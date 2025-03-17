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
- **Formalized Resource Model**: The language must support the formalized resource model (ADR_018) with explicit resource tuples, controller labels, and conservation laws.
- **Dual Validation Support**: Native constructs for both temporal and ancestral validation of cross-chain resources.
- **Resource Conservation**: Compile-time checks to ensure resource conservation laws (ΔTX = 0) are maintained.

## Language Inspirations & Research Survey

### 1. Move (Aptos, Sui)

**Key Attributes:**
- First-class **resources** (linear types, can't accidentally double-spend or lose assets).
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
- Strong for single-chain assets, but doesn't natively handle **multi-timeline causality**.
- Lacks **controller labels** for tracking cross-chain resource provenance.

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
- Strong at managing **immutable history**, but doesn't have **first-class temporal operators** (watch, timeout, barrier).
- No native support for **formalized resources** with controller labels.

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
- Could model **resource conservation**, but not with the complete resource formalization.

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
- No support for **controller labels** or **dual validation**.

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
- No native concept of **resource conservation** or **controller labels**.

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
- Could be extended to support **formalized resources** and **controller labels**.

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
- No native support for **resource formalization** or **conservation laws**.

### 8. Anoma Resource Machine

**Key Attributes:**
- Formal **resource logic** with explicit creation/consumption.
- Strong **conservation properties** guaranteed by the type system.
- **Controller** concept for tracking resource provenance.

**Example:**
```
transaction transfer_tokens() {
    input resource token_a: TokenResource { owner: Alice, amount: 10 };
    output resource token_b: TokenResource { owner: Bob, amount: 10 };
    
    validate {
        assert(token_a.amount == token_b.amount);
        assert(token_b.controller_label.creatingController == token_a.controller_label.creatingController);
    }
}
```

**Link:** https://anoma.net/

**Trade-offs:**
- Strong for **resource formalization** but lacks **temporal validation**.
- Would need integration with **time maps** for complete causality tracking.

## Trade-Off Matrix

| Language | Strong Typing | First-Class Time | Resources | Visualizable | Causal Proof | Replayable | Composable | Resource Formalization | Dual Validation |
|---|---|---|---|---|---|---|---|---|---|
| Move | ✅ | ❌ | ✅ | ❌ | ✅ | ✅ | ✅ | 🟡 | ❌ |
| Unison | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ | ❌ | ❌ |
| TLA+ | ✅ | ✅ | ❌ | ❌ | ✅ | ✅ | ❌ | 🟡 | ❌ |
| BPMN | ❌ | ✅ | ❌ | ✅ | ❌ | ✅ | ✅ | ❌ | ❌ |
| Blech | ✅ | ✅ | ❌ | ❌ | ✅ | ✅ | ✅ | ❌ | ❌ |
| Choreographic | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ |
| Erlang | ✅ | ✅ | ❌ | ❌ | ✅ | ✅ | ✅ | ❌ | ❌ |
| Anoma | ✅ | ❌ | ✅ | ❌ | ❌ | ✅ | ✅ | ✅ | 🟡 |

## Design Synthesis: What Time Bandits Needs

✅ **Typed Resources** (like Move).  
✅ **Causal Proof** (like TLA+).  
✅ **Visualizable Flow** (like BPMN).  
✅ **First-Class Time Primitives** (like Blech).  
✅ **Global Protocol Option** (like Choreographic).  
✅ **Replayable and Content-Addressed** (like Unison).  
✅ **Mailbox and Supervision** (like Erlang).  
✅ **Formalized Resources with Controller Labels** (like Anoma).  
✅ **Resource Conservation Laws** (like Anoma).  
✅ **Dual Validation** (unique to Time Bandits - combining temporal and ancestral validation).

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
- Implement the **resource formalization model** from ADR_018 with explicit resource tuples.
- Support **controller labels** for tracking resource provenance across chains.
- Enforce **resource conservation laws** (ΔTX = 0) at compile time.
- Provide native constructs for **dual validation** (temporal and ancestral).

# Addendum: Why Choreographic Languages Are a Perfect Fit for Time Bandits

## The Core Insight: Global Programs, Local Execution

Choreographic programming offers a **global-first approach** to defining how multiple independent actors interact. Instead of writing separate programs for each participant (Travelers, Keepers, Bandits, etc.), a **single program describes their entire interaction flow**.

From this **one global truth**, the system generates **per-actor handlers**, ensuring:
✅ **No missing messages** (compile-time checked).  
✅ **No unexpected messages** (every message is expected).  
✅ **No protocol drift** (all participants get code derived from the same protocol).  
✅ **Causal correctness by design** (the program defines the allowed order of events).  
✅ **Resource conservation** (formalized resources follow ΔTX = 0 law).  
✅ **Dual validation** (resources undergo both temporal and ancestral validation).

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
| **Resource formalization model.** | Resources are tracked with explicit tuples and controller labels. |
| **Conservation laws are enforced.** | ΔTX = 0 is maintained across all resource operations. |
| **Dual validation is built-in.** | Both temporal and ancestral validation ensure cross-chain security. |

---

## What a Time Bandits Choreographic Language (TBCL) Might Look Like

### **1. Choreography as a Global Protocol with Resource Formalization**
A **TBCL program** would look something like this:

```yaml
choreography: CrossChainSwap

participants:
    - Traveler
    - EthereumAccountProgram
    - CelestiaAccountProgram
    - SwapProgram
    - SettlementProgram

resources:
    - USDCToken:
        fungibilityDomain: "ERC20:USDC"
        resourceLogic: "TokenLogic"
        ephemeral: false
    - TIAToken:
        fungibilityDomain: "CELESTIA:TIA"
        resourceLogic: "TokenLogic"
        ephemeral: false

protocol:
    - Traveler deposits 100 USDCToken into EthereumAccountProgram:
        createResource:
            resource: USDCToken
            quantity: 100
            controllerLabel:
                creatingController: "ethereum"
                terminalController: "ethereum"
    
    - EthereumAccountProgram transfers USDCToken to SwapProgram:
        consumeResource:
            resource: USDCToken
            quantity: 100
        createResource:
            resource: USDCToken
            quantity: 100
            controllerLabel:
                creatingController: "ethereum"
                terminalController: "ethereum"
                affectingControllers: ["ethereum"]
    
    - Traveler deposits 50 TIAToken into CelestiaAccountProgram:
        createResource:
            resource: TIAToken
            quantity: 50
            controllerLabel:
                creatingController: "celestia"
                terminalController: "celestia"
    
    - CelestiaAccountProgram transfers TIAToken to SwapProgram:
        consumeResource:
            resource: TIAToken
            quantity: 50
        createResource:
            resource: TIAToken
            quantity: 50
            controllerLabel:
                creatingController: "celestia"
                terminalController: "celestia"
                affectingControllers: ["celestia"]
    
    - SwapProgram waits for both escrows.
    - SwapProgram calls finalizeSwap() on SettlementProgram:
        dualValidation:
            temporal: ["ethereum", "celestia"]
            ancestral: ["ethereum.tokenLogic", "celestia.tokenLogic"]
    
    - SettlementProgram confirms and sends result to Traveler's EthereumAccountProgram:
        consumeResource:
            resource: USDCToken
            quantity: 100
        consumeResource:
            resource: TIAToken
            quantity: 50
        createResource:
            resource: USDCToken
            quantity: 96
            controllerLabel:
                creatingController: "ethereum"
                terminalController: "ethereum"
                affectingControllers: ["ethereum", "celestia", "ethereum"]
        createResource:
            resource: TIAToken
            quantity: 2
            controllerLabel:
                creatingController: "celestia"
                terminalController: "celestia"
                affectingControllers: ["celestia", "ethereum", "celestia"]
```

### **2. Compiler-Generated Per-Actor Code with Resource Formalization**
From the **single protocol**, the compiler generates **separate effect handlers** for each actor.

#### **EthereumAccountProgram's Generated Code**
```haskell
handleMessage (Deposit resource quantity) = do
    -- Validate resource has proper controller label
    validateControllerLabel resource.controllerLabel "ethereum"
    
    -- Check resource conservation
    let initialDelta = calculateDelta []
    
    -- Create the resource with proper controller label
    let newResource = resource { 
        quantity = quantity,
        controllerLabel = ControllerLabel {
            creatingController = "ethereum",
            terminalController = "ethereum",
            affectingControllers = ["ethereum"]
        }
    }
    
    -- Transfer to SwapProgram
    transfer newResource quantity SwapProgram
    
    -- Verify conservation law
    let finalDelta = calculateDelta [newResource]
    assert (finalDelta - initialDelta == 0)
```

#### **SwapProgram's Generated Code**
```haskell
onMessage (Receive resource) = do
    -- Validate resource using dual validation
    validateResource resource
    
    -- Track received resources
    trackResource resource
    
    -- Check if we have all required resources
    if hasResource "ERC20:USDC" && hasResource "CELESTIA:TIA" then do
        -- Apply dual validation before finalizing
        let usdcResource = getResource "ERC20:USDC"
        let tiaResource = getResource "CELESTIA:TIA"
        
        -- Temporal validation
        validateTemporalConsistency "ethereum" usdcResource
        validateTemporalConsistency "celestia" tiaResource
        
        -- Ancestral validation
        validateControllerAncestry usdcResource.controllerLabel
        validateControllerAncestry tiaResource.controllerLabel
        
        -- Finalize swap with resource conservation
        finalizeSwap()
```

This ensures each participant **only sees the logic relevant to them**, while maintaining resource formalization principles.

---

## **How This Enhances Program Execution & Visualization**

A Time Bandits program must be:
1. **Readable as a program.**
2. **Visualizable as a control-flow diagram.**
3. **Visualizable as an execution trace.**
4. **Replayable deterministically.**
5. **Resource-aware with conservation laws.**
6. **Secure with dual validation.**

Choreographic programming with resource formalization **directly enables** all six:

| Requirement | How Choreography Helps |
|---|---|
| **Readable** | The full process is declared in a single place. |
| **Visualizable** | The protocol can be rendered **directly** into a graph. |
| **Execution Traceable** | Each participant follows a script derived from the same protocol. |
| **Replayable** | Logs reconstruct the exact execution history. |
| **Resource Conservation** | Explicit resource creation and consumption with ΔTX = 0 law. |
| **Dual Validation** | Both temporal and ancestral validation ensure cross-chain correctness. |

---

## **Correctness & Safety Guarantees**

Time Bandits needs **correct-by-construction programs**. Choreographic languages with resource formalization provide **built-in correctness** via:

✅ **Causal Ordering** → Events happen **only in the declared sequence**.  
✅ **Message Safety** → No risk of sending unexpected messages.  
✅ **Effect Composition** → Programs can be **composed** without breaking correctness.  
✅ **Typed Resources & Participants** → Every actor only handles **valid messages**.  
✅ **Resource Conservation** → Formal guarantee that ΔTX = 0 across all operations.  
✅ **Controller Tracking** → Resources maintain provenance through controller labels.  
✅ **Dual Validation** → Both temporal and ancestral validation ensure cross-chain security.

### **Example: Preventing an Invalid Execution**
Suppose a traveler **forgets** to deposit funds before invoking a swap.

- In a **normal imperative system**, this might cause a runtime failure.
- In **TBCL with resource formalization**, this is a **compile-time error** because:
  1. The deposit **must** occur before the swap (temporal correctness).
  2. Resources cannot be created from nothing (resource conservation).
  3. Controller labels must track valid provenance (ancestral correctness).

The compiler **enforces**:
```yaml
protocol:
    - Traveler deposits -> ✅ Required step (creates resource with valid controller)
    - Swap is triggered -> ❌ Invalid if deposit missing (compile error: resource conservation violated)
```

---

## **Handling Failures Gracefully with Resource Conservation**
Since all message flows and resource operations are **declared explicitly**, failures are **first-class citizens**.

### **Built-In Recovery Mechanisms**
- **Timeouts** → If a deposit doesn't arrive within 15 minutes, **auto-refund**.
- **Fallback Paths** → If a swap fails, execute **rollback** transactions.
- **Dispute Resolution** → A dispute case can be part of the protocol.
- **Controller Backup** → If a terminal controller fails, fall back to backup controllers.
- **Resource Conservation** → Even in failure cases, resource conservation laws must be maintained.

### **Example: Adding Failure Handling with Resource Formalization**
```yaml
protocol:
    - Traveler deposits 100 USDCToken:
        createResource:
            resource: USDCToken
            quantity: 100
            controllerLabel:
                creatingController: "ethereum"
                terminalController: "ethereum"
    
    - If deposit does not arrive in 15m:
        consumeResource:
            resource: USDCToken
            quantity: 100
            nullifier: generateNullifier()
        createResource:
            resource: USDCToken
            quantity: 100
            controllerLabel:
                creatingController: "ethereum"
                terminalController: "ethereum"
                affectingControllers: ["ethereum"]
        refund.
```

**How This Helps**
✅ **Every failure mode is declared upfront.**  
✅ **No undefined failure states.**  
✅ **Programs are guaranteed to handle failures correctly.**  
✅ **Resources are conserved even during failures.**  
✅ **Controller labels track resource history through failures.**  

---

## **Replayability & Verification with Dual Validation**
Because all Time Bandits programs are **immutable effect logs**, choreographic programming with resource formalization **fits perfectly**:
- Every execution step is **linked to its causal parent**.
- Every message flow is **verifiable** against the original protocol.
- Every resource operation follows **conservation laws**.
- Every cross-chain resource undergoes **dual validation**.
- **Replay is trivial** → The logs **directly match** the declared process.

### **Example: Verifying Execution Logs with Resource Formalization**
Imagine a **log of program execution** that includes resource operations and controller label updates. Verification checks:
1. All resources follow conservation laws (ΔTX = 0).
2. All resources have valid controller labels.
3. All cross-chain operations pass dual validation.
4. The causal graph matches the declared choreography.

