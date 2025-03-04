# Time Bandits Invocation Model
Version: 1.0  
Date: 2025-03-07

## 1. Introduction

Time Bandits provides a safe, composable invocation model that allows programs to invoke and interact with each other while preserving ownership and causal consistency. Inspired by the Move smart contract language, Time Bandits enforces strict ownership rules, explicit resource movement, and causal ordering via Lamport clocks.

This document defines:

- How programs invoke each other.
- How resources transfer between programs.
- How capabilities delegate access.
- How concurrent execution is handled.
- How cross-timeline safety is maintained.

## 2. Key Principles (Inspired by Move)

| Principle | Move's Approach | Time Bandits' Approach |
| --- | --- | --- |
| Explicit Ownership Transfer | Resources belong to exactly one owner at a time. | Resources belong to exactly one program memory slot at a time. |
| No Implicit Access | A function cannot modify external state unless explicitly passed a resource. | A program cannot modify another program's state unless it receives a resource. |
| Atomic Transfers | A resource is fully moved or not moved at all. | A resource transfer is ZK-verifiable and atomic. |
| Borrowing Instead of Ownership Transfer | Programs can temporarily borrow resources without full ownership transfer. | Programs delegate capabilities instead of borrowing. |
| Prevent Reentrancy Loops | Prevents cyclic function calls. | Uses Lamport clocks to ensure causal consistency. |

## 3. Invocation Model Overview

### 3.1 How Programs Interact

Programs in Time Bandits do not share global state. Instead, they interact by invoking each other and transferring resources explicitly.

| Interaction | Mechanism |
| --- | --- |
| Program A gives Program B a resource | EscrowToProgram |
| Program A requests Program B to perform an action | InvokeProgram |
| Program A retrieves a resource from Program B | ClaimFromProgram |
| Program A temporarily allows Program B to use a resource | DelegateCapability |
| Program A reacts to a state change in Program B | WatchResource |

### 3.2 Invocation Workflow

Example: Program A escrows collateral in Program B, and Program B executes a trade.

**Escrow**:

1. ProgramA escrows 100 GoldCoin into ProgramB's memory.
2. ProgramA loses ownership, ProgramB gains ownership.

```haskell
EscrowToProgram (TokenBalanceResource "GoldCoin" "Alice" 100) "ProgramB" "escrow_slot"
```

**Invocation**:

1. ProgramA calls ProgramB's execute_trade function.

```haskell
InvokeProgram "ProgramB" "execute_trade" []
```

**Trade Execution**:

1. ProgramB consumes the escrowed resource.
2. ProgramB transfers proceeds back to ProgramA.

```haskell
EscrowToProgram (TokenBalanceResource "SilverCoin" "Bob" 200) "ProgramA" "trade_proceeds"
```

**ProgramA Claims Proceeds**:

```haskell
ClaimFromProgram "ProgramB" "trade_proceeds" (TokenBalanceResource "SilverCoin" "Bob" 200)
```

This ensures atomic ownership transfer and prevents state modification outside explicit calls.

## 4. Concurrency & Cross-Timeline Safety

### 4.1 Causal Consistency
- Lamport clocks prevent out-of-order execution.
- Cross-timeline time maps ensure a consistent snapshot of multiple timelines.

### 4.2 Cross-Program Invocation Rules

| Rule | Enforced By |
| --- | --- |
| A program cannot access another program's state without explicit transfer. | EscrowToProgram, ClaimFromProgram |
| A program cannot modify another program's memory unless the program accepts it. | InvokeProgram |
| A program cannot execute an effect before its causal dependencies are met. | Lamport clocks |
| A program cannot withdraw a resource before the expected deposit is confirmed. | ZK-proof verification |

## 5. Invocation Effects

### 5.1 Escrow Resource to Another Program

```haskell
EscrowToProgram :: Resource -> ProgramId -> MemorySlot
```

- Transfers a resource into another program's memory.
- Sending program loses ownership.
- Receiving program gains full control.

### 5.2 Invoke Another Program

```haskell
InvokeProgram :: ProgramId -> FunctionName -> [Resource]
```

- Calls another program's function explicitly.
- The receiving program must define the function to handle it.

### 5.3 Claim a Resource from Another Program

```haskell
ClaimFromProgram :: ProgramId -> MemorySlot -> Resource
```

- A program retrieves a resource it was previously granted.
- This cannot happen unless explicitly escrowed.

### 5.4 Delegate Capability Without Moving Resource

```haskell
DelegateCapability :: Capability -> ProgramId -> Expiry
```

- Grants another program temporary rights over a resource without transferring it.
- Expiry ensures automatic revocation.

### 5.5 Watch for Changes in Another Program

```haskell
WatchResource :: ResourceKey -> Condition -> Trigger
```

- A program can set up a watcher on another program's state.
- When the condition is met, the trigger fires.

## 6. Example: Composable Trading System

**Scenario**:
- ProgramA (Collateral Manager) escrows 100 GoldCoin to ProgramB (Trade Engine).
- ProgramB executes a trade if SilverCoin deposits are observed.

**Step 1: Program A Escrows GoldCoin**

```haskell
EscrowToProgram (TokenBalanceResource "GoldCoin" "Alice" 100) "ProgramB" "escrow_slot"
```

**Step 2: Program B Watches for SilverCoin Deposit**

```haskell
WatchResource (TokenBalanceResource "SilverCoin" "Bob" 200)
  (BalanceAtLeast "SilverCoin" "Bob" 200)
  (InvokeProgram "ProgramB" "execute_trade" [])
```

**Step 3: Bob Deposits SilverCoin (Observed by Watcher)**

```haskell
EscrowToProgram (TokenBalanceResource "SilverCoin" "Bob" 200) "ProgramB" "trade_slot"
```

**Step 4: Program B Executes Trade**

```haskell
InvokeProgram "ProgramB" "execute_trade" []
```

- Consumes 100 GoldCoin (escrow) + 200 SilverCoin (trade deposit).
- Transfers proceeds back to Program A.

```haskell
EscrowToProgram (TokenBalanceResource "SilverCoin" "Bob" 200) "ProgramA" "trade_proceeds"
```

**Step 5: Program A Claims Trade Proceeds**

```haskell
ClaimFromProgram "ProgramB" "trade_proceeds" (TokenBalanceResource "SilverCoin" "Bob" 200)
```

## 7. Security Guarantees

| Guarantee | Enforced By |
| --- | --- |
| No program modifies another's memory without consent. | Escrow & Claim Effects |
| No double-spending. | ZK Proofs |
| No cyclic reentrancy. | Lamport Causal Order |
| No cross-timeline backdating. | Time Map Constraints |

## 8. Conclusion

This invocation model:
- ✅ Preserves Move-style ownership guarantees.
- ✅ Supports composable program interactions.
- ✅ Ensures cross-program consistency.
- ✅ Prevents unauthorized memory access.
- ✅ Allows cross-timeline workflows.
