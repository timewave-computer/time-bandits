# ADR-001: Invocation Model (Updated)


## Status

**Accepted - Updated 2025-03-07**


## Context

The Time Bandits system requires a **formal invocation model** to describe:

- How actors (time travelers) trigger program execution.
- How programs communicate with each other.
- How external facts (like cross-chain deposits) enter the system.
- How responses flow back to actors after a program completes an action.

This invocation model must:
- Maintain **strong causal traceability**.
- Be fully **auditable and replayable**.
- Be **timeline-agnostic** (work across heterogeneous chains).
- Enforce **separation between actors and programs** — programs should never directly trust or communicate with off-chain actors.
- Integrate with the **account program** model, where each actor owns a **single gateway program** that intermediates all asset and message flows.


## Decision

### Core Principle: Account Programs as Invocation Gateways

- **Actors (time travelers) do not directly communicate with programs.**
- Each traveler owns exactly **one account program**, which:
    - Holds all assets for that traveler across timelines.
    - Mediates all **actor-initiated messages**.
    - Receives all **program responses** intended for the traveler.
    - Records all inbound and outbound messages in a **per-resource effect log**.
- Programs only trust messages from **other programs** (never directly from actors), and rely on **account programs** for actor-to-program and program-to-actor communication.


## Invocation Flow Overview

### 1. Actor Initiates Action

- Traveler signs a message proposing an action (deposit, withdrawal, invocation, observation).
- The message is submitted to the Time Bandits network.
- The message is applied as an **effect** to the actor's **account program**.
- The account program records the message in its **outbox**, causally linking it to prior effects.


### 2. Account Program Dispatches Message

- The account program evaluates the message type:
    - **Deposit:** Transfers assets to the target program.
    - **Withdrawal:** Pulls assets back from a program.
    - **Invocation:** Sends a cross-program invocation message.
- Each action is recorded in the account program’s **per-resource effect log**.


### 3. Target Program Receives Invocation

- The target program receives the invocation as a proposed **effect**.
- The effect includes:
    - The calling account program’s ID.
    - The target function (entrypoint).
    - Arguments.
    - Time map snapshot (what facts were known at invocation time).

- The program applies the effect and logs it into its **own effect log**, causally linking it to prior effects.


### 4. Program Generates Optional Response

- If the program produces a result for the traveler, it sends a **SendCallback** effect to the originating account program.
- This callback is applied to the account program’s **inbox**, causally linking it to the prior invocation.


### 5. Traveler Retrieves Response

- Travelers poll their account program to read their **inbox**, retrieving all received callbacks in causal order.
- This completes the invocation lifecycle.


## Message Types

| Message Type | Origin | Destination | Purpose |
|---|---|---|---|
| Deposit | Traveler | Account Program | Transfer assets into a program |
| Withdraw | Traveler | Account Program | Retrieve assets from a program |
| Invoke | Traveler | Account Program | Call a program function |
| Transfer | Account Program | Program | Transfer assets to a program |
| SendCallback | Program | Account Program | Return results to traveler |
| ReceiveCallback | Account Program | Traveler (via inbox query) | Retrieve program responses |
| Watch | Account Program | Timeline Keeper | Observe external deposit or event |


## Account Program State

Each account program tracks:

```haskell
data AccountProgramState = AccountProgramState
    { balances :: Map (TimelineID, Asset) Amount
    , inbox :: [ReceivedMessage]
    , outbox :: [SentMessage]
    , effectDAG :: EffectDAG
    }
```

- **Balances:** Current asset holdings across all timelines.
- **Inbox:** Messages received from programs (e.g., callbacks).
- **Outbox:** Messages sent to programs.
- **EffectDAG:** Full causal history of all applied effects.


## Security and Provenance Guarantees

This invocation model guarantees:

- Programs only talk to programs — programs never need to trust off-chain travelers directly.  
- All actor actions are signed and logged via account program effects.  
- Programs can verify the **full provenance** of any incoming message by querying the sender’s account program log.  
- All communication produces permanent, auditable log entries.


## Actor-Program Separation Invariant

| Communication Type | Mediated By |
|---|---|
| Actor to Program | Account Program Outbox |
| Program to Actor | Account Program Inbox |
| Program to Program | Direct (via invocation effects) |


## External Consistency via Time Map Snapshots

- Each cross-program message references a **time map snapshot**, proving which external facts were known at the time the message was generated.
- If external facts change before an effect applies, the preconditions are re-validated before the effect is accepted.


## Examples

### Deposit Flow

**Traveler -> AccountProgram -> TargetProgram**

1. Traveler submits:

```toml
type = "deposit"
resource = "USDC"
amount = 100
destination = "TradeProgram"
```

2. Account program creates a `Transfer` effect, moving USDC to TradeProgram.
3. TradeProgram applies the effect, updating its internal balances.


### Cross-Program Invocation

**Traveler -> AccountProgram -> TargetProgram -> AccountProgram -> Traveler**

1. Traveler submits:

```toml
type = "invoke"
target = "SettlementProgram"
entrypoint = "finalizeTrade"
arguments = ["order123"]
```

2. Account program packages this into:

```haskell
Effect.Invoke
    { targetProgram = "SettlementProgram"
    , entrypoint = "finalizeTrade"
    , arguments = ["order123"]
    , observedFacts = [...]
    }
```

3. SettlementProgram applies the effect.
4. SettlementProgram generates a result:

```toml
type = "callback"
target = "actor123"
payload = { result = "Trade Settled" }
```

5. Account program logs the callback in its inbox.


### Traveler Polling Inbox

Traveler queries:

```http
GET /account/{travelerID}/inbox
```

Response:

```json
[
    { "source": "SettlementProgram", "payload": { "result": "Trade Settled" } }
]
```


## Summary Flow Diagram

## Summary Flow Diagram

```
+-----------------+
| Time Traveler   |
| (proposes)      |
+-----------------+
        |
        v
+--------------------+
| Account Program    |
| (owned by actor)   |
+--------------------+
        |
        v
+------------------+
| Target Program   |
+------------------+
        |
        v
+--------------------+
| Account Program    |
| (for responses)    |
+--------------------+
        |
        v
+-----------------+
| Time Traveler   |
| (polls inbox)   |
+-----------------+
```

## Replay and Auditability

- Every message and response is recorded as a **causal effect** in an **append-only log**.
- Each effect has:
    - Full causal ancestry.
    - Cryptographic signature (proving origin).
    - Content hash (proving integrity).
- Replay re-applies effects in order, guaranteeing deterministic reconstruction of program state.


## Relationship to Other Parts of the System

| Component | Role in Invocation Model |
|---|---|
| Account Program | Gatekeeper and message queue for each traveler |
| Effect Pipeline | Processes all proposed effects |
| Resource Logs | Record every effect per resource |
| Time Map | Provides external consistency for observed facts |
| Fact Logs | Track external events that trigger actor messages (e.g., deposits) |
| Unified Log | Combined view of all applied effects, facts, and events |


## Benefits

- Fully auditable and replayable.  
- No direct actor-program trust.  
- Clear separation of concerns (actors only control account programs).  
- Built-in support for external consistency (time map snapshots).  
- Actor policies (rate limits, multi-sig) enforced at account level.  

