# ADR-003: Effect Adapter Generation (Updated)


## Status

**Accepted - Updated 2025-03-07**


## Context

Time Bandits programs interact with **multiple external blockchains** (referred to as "timelines" in system terminology), each with different:

- **APIs** (transaction formats, signing schemes, RPC models).
- **Proof formats** (inclusion proofs, state proofs, receipt proofs).
- **Asset models** (UTXO vs account-based).
- **Serialization formats** (RLP, Borsh, protobuf, etc.).

Initially, these interactions were handled by manually coded adapters for each blockchain, which was:
- Error-prone.
- Difficult to maintain.
- A security risk if inconsistencies crept into proof handling.
- A major barrier to adding support for new chains.

As the system evolved, **effect adapters** became the **standard boundary between programs and external blockchains**. These adapters **mediate all external effects**, ensuring that:

- External data is correctly observed and proven (fact creation).
- External calls (deposits, withdrawals) correctly map to blockchain transactions.
- External facts and events become **observed facts** with valid proofs.


## Decision

### Core Principle: Generated, Schema-Driven Adapters

Each supported timeline will have a **generated effect adapter** responsible for:

- **Encoding outgoing effects into timeline-specific transactions.**  
- **Validating incoming proofs and facts against the external timeline.**  
- **Converting external facts into canonical Time Bandits facts.**  
- **Preserving external time observations into time map snapshots.**


## Adapter Scope

| Adapter Scope | Description |
|---|---|
| Per Timeline | Each adapter only handles one timeline (e.g., Ethereum, Solana). |
| Effect Mapping | Each adapter implements only effects relevant to that timeline. |
| Proof Handling | Each adapter validates timeline-specific proofs. |
| Time Map Observation | Each adapter records the timeline observation (block height, hash, etc.) at the time of effect application. |


## Changes in Effect Flow

1. **Account Programs as Mediators**  
    - All external deposits, withdrawals, transfers pass through account programs.
    - Account programs **delegate timeline-specific handling to adapters**.

2. **Fact Observation through Adapters**  
    - Time Keepers no longer encode all fact logic directly.
    - Keepers **invoke adapters to transform timeline data into canonical facts**.

3. **Proof Embedding**  
    - Each effect recorded in the unified log will include:
        - The effect payload itself.
        - The **timeline proof** (timeline-specific).
        - The **time map snapshot** (external observations).

4. **Replay Requires Adapters**  
    - Replay engines will load the appropriate adapter to verify effects during replay.
    - This keeps replay deterministic even if external formats evolve.


## Adapter Generation Process

Adapters will be **generated from schemas** that define:

| Field | Description |
|---|---|
| Timeline ID | Which chain the adapter applies to. |
| Effect Mappings | Supported effects (deposit, withdraw, transfer, etc.). |
| Proof Formats | How to validate proofs for each effect type. |
| RPC Interfaces | External API bindings for submitting transactions and querying state. |
| Serialization Rules | How to encode/decode effect payloads and proofs. |

This ensures:

- Adapters are consistent across timelines.  
- New timelines can be added quickly by providing schemas.  
- Security is centralized into well-defined, auditable rules.  


## Example Effect Adapter Schema (TOML)

```toml
timeline = "Ethereum"

[[effects]]
type = "Deposit"
tx_format = "RLP"
proof_format = "MPT"
rpc_call = "eth_getTransactionReceipt"
required_fields = ["amount", "asset", "destination"]

[[effects]]
type = "Withdraw"
tx_format = "RLP"
proof_format = "MPT"
rpc_call = "eth_call"
required_fields = ["amount", "asset", "source"]

[proofs]
inclusion = "eth_getProof"
receipt = "eth_getTransactionReceipt"
```


## Account Program Integration

Account programs handle:

- Maintaining per-timeline balances.
- Applying all external effects (deposits, withdrawals, transfers) by **invoking adapters**.
- Enforcing traveler-defined policy (rate limits, multi-sig).
- Linking all external interactions to **time map observations**.

This ensures:

- All traveler-visible actions are recorded in their account program log.
- All cross-program resource flows are causally linked to external facts.
- All external dependencies are cryptographically proven.


## Time Map Enforcement

- Every external effect **records the external timeline state (time map snapshot)** at the time the effect applies.
- This snapshot is hashed and linked into the effect DAG.
- This guarantees that every replay will see **exactly the same external observations** as the original run.


## Generated Adapter Interface

Each generated adapter will expose:

```haskell
class EffectAdapter where
    applyEffect :: Effect -> AccountProgramState -> ExternalTimeline -> IO (Either AdapterError Receipt)
    validateProof :: Effect -> ExternalTimeline -> IO (Either ProofError ())
    observeFact :: ExternalTimeline -> IO (Either ObservationError ObservedFact)
```


## Security Benefits

- Standardizes all timeline-specific proof handling.  
- Ensures all effects are processed via **well-tested, auditable generated code**.  
- Fully decouples **program logic from timeline specifics** — programs only emit abstract effects.  
- Forces all observed facts to pass through the same schema-defined process, ensuring they match replay expectations.  


## Extensibility Benefits

- Adding a new blockchain requires only a new **adapter schema** and a generated adapter.  
- Simulation can stub in mock adapters, while production uses real adapters.  
- Programs remain **timeline-agnostic**, since they never directly call external RPC.


## Example Lifecycle - Deposit

| Step | Actor | Action |
|---|---|---|
| 1 | Traveler | Submits deposit message to account program |
| 2 | Account Program | Invokes Ethereum Adapter’s `applyEffect` |
| 3 | Ethereum Adapter | Builds RLP transaction |
| 4 | Ethereum Adapter | Sends transaction via RPC |
| 5 | Ethereum Adapter | Observes deposit proof |
| 6 | Ethereum Adapter | Packages proof + time map snapshot |
| 7 | Account Program | Records effect + proof in unified log |


## Example Lifecycle - Observed Fact

| Step | Actor | Action |
|---|---|---|
| 1 | Time Keeper | Observes new block on Ethereum |
| 2 | Keeper | Invokes Ethereum Adapter’s `observeFact` |
| 3 | Ethereum Adapter | Applies extraction rules (price feed, balances, etc.) |
| 4 | Ethereum Adapter | Packages fact + proof + time map snapshot |
| 5 | Keeper | Signs and gossips the observed fact |
| 6 | Bandits | Receive and store fact in FactLog |


## Replay and Simulation

- During replay, the replay engine loads the relevant adapter.
- The adapter is invoked to re-verify effects and proofs.
- In simulation mode, mock adapters can stub timeline calls (but still generate valid proof structures).


## Summary - What This Changes

| Area | Change |
|---|---|
| Effect Pipeline | External effects always mediated by adapters. |
| Account Programs | Route all external effects through adapters. |
| Fact Observation | Time Keepers invoke adapters to generate facts. |
| Replay | Replay loads adapters to validate proof correctness. |
| Timeline Extensibility | New timelines added via schemas, not hardcoded logic. |
