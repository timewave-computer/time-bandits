# ADR 006: Resource Ownership and the Role of Account Programs

## Status

Proposed


## Context

In earlier iterations of Time Bandits, **programs owned resources directly**. Programs themselves maintained token balances and directly managed deposits, withdrawals, and transfers. This model was simple but introduced **several critical issues**:

- **Concurrency Hazards:** If multiple programs interacted with the same resource (a token or position), they could race to apply effects.
- **Schema Evolution Pain:** Each program defined its own internal resource schema, which made upgrading programs difficult because every schema had to evolve separately.
- **Cross-Timeline Complexity:** Programs had to be aware of which timeline they were interacting with, embedding **timeline-specific logic** into business logic.
- **Replay Ambiguity:** Inconsistent handling of deposits and withdrawals across programs made it hard to reconstruct accurate historical state.
- **Sovereignty Problems:** Programs were either tightly coupled to their original deployment timeline, or required custom bridging logic for every cross-chain case.

### New Approach: Account Programs Own All External Resources

We now introduce **account programs** as **the sole owners of resources** in the system.  
Programs themselves do not own resources — they **request resource operations via their account program**.


# Decision

## Account Program Definition

Each time traveler (developer deploying a program) has a dedicated **account program**, which:

- Holds all **assets/resources** for that traveler’s programs.
- Exposes a standard **deposit/withdrawal API**.
- Tracks **balances per timeline**.
- Can delegate **resources to programs** as inputs to their effect pipelines.
- Logs all resource changes into its own **Effect DAG** (separate from individual program DAGs).
- Is **content-addressed** and **fully replayable**.


## Core Data Structures

### AccountProgram

```haskell
data AccountProgram = AccountProgram
    { accountID :: AccountID
    , owner :: TravelerID
    , balances :: Map (TimelineID, Asset) Amount
    , effectDAG :: EffectDAG
    }
```

- `accountID`: Unique identifier for the account.
- `owner`: The traveler who owns this account.
- `balances`: Per-timeline balances.
- `effectDAG`: Full causal history of deposits, withdrawals, and cross-program transfers.


### Account Effects

```haskell
data AccountEffect
    = Deposit { timeline :: TimelineID, asset :: Asset, amount :: Amount }
    | Withdraw { timeline :: TimelineID, asset :: Asset, amount :: Amount, destination :: Address }
    | TransferToProgram { programID :: ProgramID, asset :: Asset, amount :: Amount }
    | ReceiveFromProgram { programID :: ProgramID, asset :: Asset, amount :: Amount }
```


## Interface with Programs

When a program needs resources (e.g., for trading or cross-program calls), it **requests resource inputs from its account program**.  
The program itself does not have a balance table — it receives **resources embedded inside effects** during its invocation.


## Interface with Timelines (Keepers)

When a timeline (via a Time Keeper) observes an external deposit or withdrawal, it **posts an observed fact to the relevant account program**.  
The account program applies this fact as an `ObservedDeposit` or `ObservedWithdrawal` effect in its own DAG.


## Interface with Bandits (P2P)

Bandits do **not propose resource operations directly to programs**.  
Instead, Bandits:
- Propose effects directly to **account programs**.
- Programs **reference account effects causally** inside their own effect DAGs.


# How This Changes the System

| Component | Before | Now |
|---|---|---|
| Programs | Owned resources directly | Request resource flows via accounts |
| Timelines | Deposited directly into programs | Deposit into accounts only |
| Bandits | Applied deposits directly to programs | Apply deposits only to accounts |
| Effect DAGs | Mixed resource and program effects | Separate DAG for accounts and programs |


# Example Flow: Cross-Timeline Swap

1. Traveler deploys `SwapProgram`.
2. Traveler deposits USDC into their account program on Ethereum.
3. SwapProgram calls `withdraw` on account program to get USDC.
4. SwapProgram swaps USDC for SOL via external protocol.
5. SwapProgram deposits SOL back into account program (on Solana).
6. Account program logs:
    - Deposit (USDC)
    - Withdrawal (USDC)
    - Receive (SOL)


# Benefits

- Simplified Program Logic - Programs focus on **effects and logic**, not asset management.  
- Standard Interface - All programs use the **same deposit/withdrawal API** regardless of timeline.  
- Full Replayability - Every asset flow is logged causally in account program effect DAGs.  
- Schema Stability - Account programs have a **fixed schema**, so schema evolution focuses on logic programs.  
- Cross-Timeline Abstraction - Programs remain **timeline-agnostic** — only accounts deal with external chains.  
- Developer Experience - Travelers have a **single account per identity**, simplifying asset management.


# Interaction with Other Parts of the System

| Component | Relationship to Account Programs |
|---|---|
| Program Execution | Reads from account to get input assets, logs effects referencing account transactions |
| Fact Observation | Timelines post facts directly into accounts |
| Invocation Pipeline | Programs invoke accounts for deposits/withdrawals instead of handling them internally |
| Replay Engine | Replays account effects separately from program logic |
| Observers | Observe both program effects and account effects, treating accounts as **first-class actors** |
| Safe State Management | Accounts help enforce **no pending returns** by acting as a neutral asset ledger |


# New Standard Account Program API

### Deposit

```json
{
    "type": "Deposit",
    "timeline": "Ethereum",
    "asset": "USDC",
    "amount": "100"
}
```

### Withdraw

```json
{
    "type": "Withdraw",
    "timeline": "Ethereum",
    "asset": "USDC",
    "amount": "50",
    "destination": "0xabc..."
}
```

### Transfer to Program

```json
{
    "type": "TransferToProgram",
    "programID": "swap123",
    "asset": "USDC",
    "amount": "50"
}
```


# New Standard Account Program TOML Manifest

```toml
[program]
name = "TravelerAccount"
type = "AccountProgram"
version = "1.0.0"
owner = "traveler:alice"
timelines = ["Ethereum", "Solana"]
```


# Success Criteria

- Each traveler has exactly one account program per deployment context.  
- All cross-timeline deposits/withdrawals pass through accounts.  
- No program owns external balances directly.  
- Account effects are causally linked to program effects when consumed.  
- Account programs are fully replayable.


# Migration Plan

- Programs must drop internal balance tracking.
- Timeline keepers must deposit to accounts instead of programs.
- Effect pipeline must split into:
    - Account Effects (managed by account programs).
    - Program Effects (managed by programs themselves).
- Replay engine must replay account effects first, then program effects.
- Simulation system must launch account programs as first-class actors.
