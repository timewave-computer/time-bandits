# ADR-001: Invocation Model

## Status

Accepted

## Context

The Time Bandits system needs a clear model for how actors trigger program execution, how programs communicate with each other, and how external facts (like cross-chain deposits) enter the system. We need to establish a unified, causal, auditable invocation flow.

## Decision

We will implement an invocation model based on account programs as message gateways with the following core principle:

**Actors (time travelers) never send messages directly to programs.**  
Instead, each actor owns exactly one account program, which acts as:

- The only gateway for actor-initiated actions.
- The recipient of all responses intended for the actor.
- The holder of all the actor's assets within the system.

### Invocation Flow Overview

#### Actor Initiates

1. Actor signs a message describing the desired action (deposit, call, etc.).
2. Message is submitted to the Time Bandits network.
3. The actor's account program applies this message as an effect to itself.

#### Account Program Handles Message

- If the message is a deposit, the account program:
    - Transfers assets to the target program.
    - Records the transfer in the per-resource log.

- If the message is an invocation, the account program:
    - Packages the call into a message sent to the target program.
    - Records the outgoing call in its outbox.

- If the message is a withdrawal, the account program:
    - Pulls assets back from the target program (if allowed).
    - Records the withdrawal in the per-resource log.

#### Program Applies Incoming Message

Programs receive incoming messages as proposed effects.
These effects:
- Reference the calling account program.
- Specify the desired function call and arguments.
- Optionally declare a callback — indicating where to return the result.

If the program chooses to process the message, it:
- Applies the effect to its state.
- Records the effect in the per-resource logs.
- Optionally generates a response message to the caller's account program.

#### Program Sends Callback

Programs never send callbacks directly to actors.
Instead, callbacks are effects applied to the caller's account program.

- Each callback is recorded in the receiving account program's inbox.
- This inbox is part of the account program's causal log.
- The traveler can query their account program to check for responses.

### Message Types

| Message Type | Origin | Destination | Purpose |
|-------------|--------|-------------|---------|
| Deposit | Traveler | Account Program | Move assets into a program. |
| Withdraw | Traveler | Account Program | Retrieve assets back into account. |
| Invoke | Traveler | Account Program | Call a program function. |
| Transfer | Account Program | Program | Move assets to a program. |
| SendCallback | Program | Account Program | Return result to actor. |
| ReceiveCallback | Account Program | Traveler (via query) | Actor retrieves program responses. |
| Watch | Account Program | Timeline | Watch for external deposit. |

### Inbox/Outbox Structure

Each account program maintains:

```haskell
data AccountState = AccountState
    { balances :: Map ResourceId Integer
    , inbox    :: [ReceivedMessage]
    , outbox   :: [SentMessage]
    }
```

- **Inbox:** Messages received from programs (e.g., callbacks).
- **Outbox:** Messages sent to programs (e.g., deposits, calls).

Both are part of the account program's causal log, meaning they are auditable, replayable, and tied to specific effects.

## Consequences

### Security Model

This invocation flow enforces:

- Programs only talk to programs (never actors directly).
- All actor actions are signed and applied as account program effects.
- Account programs can enforce additional policy (multi-sig, rate limits, etc.) if desired.
- Every message is causally linked and replayable.
- Programs can verify the provenance of any incoming message by inspecting the account program's log.

### Actor-Program Separation Invariant

No program ever interacts with an actor directly.
All program-to-actor and actor-to-program communication flows through the account program. This ensures:

- Complete causal traceability.
- Simplified security model (programs only authenticate other programs, never off-chain keys).
- Unified audit log (the account program's log is a complete record of all actor actions).

### Effect-Only Guarantee

All communication (deposits, calls, callbacks) is mediated by applied effects. This means:

- Each communication produces a log entry.
- Each entry is provably linked to its causal predecessor.
- Each entry is accompanied by a proof of correct application.

### Time Map Consistency

Each cross-program message references a specific time map snapshot. This ensures:

- External facts (like balances or external proofs) were observed at a known point in time.
- If external facts change before the effect applies, the preconditions are re-validated.

## Implementation

### Example Message Lifecycle

#### Deposit Flow

`Traveler -> AccountProgram -> TargetProgram`

1. Traveler submits:

```yaml
message:
    type: deposit
    resource: USDC
    amount: 100
    to: TradeProgram
```

2. Account program creates a `Transfer` effect, moving USDC to TradeProgram.
3. TradeProgram applies the effect and updates its internal balances.

#### Cross-Program Call Flow

`Traveler -> AccountProgram -> TargetProgram -> AccountProgram -> Traveler`

1. Traveler submits:

```yaml
message:
    type: invoke
    targetProgram: SettlementProgram
    entrypoint: finalizeTrade
    arguments: ["order123"]
```

2. Account program packages this into:
```yaml
effect:
    type: Invoke
    targetProgram: SettlementProgram
    entrypoint: finalizeTrade
    arguments: ["order123"]
```

3. SettlementProgram applies the effect and performs some work.
4. SettlementProgram sends:

```yaml
effect:
    type: SendCallback
    to: account::actor123
    payload: { result: "Trade Settled" }
```

5. Account program appends the callback to its inbox.

#### Querying Inbox (Traveler View)

To retrieve program responses, the traveler queries:

```bash
GET /account/{actor}/inbox
```

This returns all received callbacks, in causal order.

### Summary Flow Diagram

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

This invocation model guarantees:

- Full causal traceability — every message has a clear predecessor and time map reference.
- Tamper resistance — messages are part of the immutable effect log.
- Flexible policy enforcement — account programs can apply arbitrary rules (multi-sig, delegation, etc.) on outbound messages.