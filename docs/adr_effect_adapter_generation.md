# ADR: Effect Adapter Generation

**Date:** 2025-03-07  
**Status:** Draft  
**Author:** Sam Hart  
**Decision:** Adopt adapter generation for conventional effects, optional for complex effects  

---

## Context

In the Time Bandits system, programs deployed by Time Travelers need to interact with multiple **timelines** (blockchains, rollups, DA layers, and other stateful systems). These interactions occur through **effects** like:

- `EscrowToProgram` (locking assets into a program-managed account)
- `ClaimFromProgram` (retrieving assets from a program-managed account)
- `WatchResource` (subscribing to external state changes like token balance changes)
- `InvokeProgram` (invoking another program, possibly with cross-program transfers)

Each effect that touches external timelines requires an **effect adapter**, a piece of code that:

- Constructs the appropriate transaction for that timeline’s virtual machine (VM)
- Signs and submits the transaction (or message)
- Retrieves inclusion proofs
- Validates state (e.g., balances, escrow presence)
- Translates between program state and external state

This creates a heavy **per-timeline development burden**. Every new supported timeline requires writing new effect adapters for all supported effects. This is both repetitive and error-prone.

---

## Proposal: Timeline Descriptors and Adapter Generation

### Overview

Instead of requiring hand-written adapters for each timeline, we will generate adapters automatically from:

- A machine-readable **timeline descriptor** that defines:
    - The timeline’s VM type (`evm`, `wasm`, `utxo`, etc.)
    - The expected transaction formats (ABI, schema)
    - The state query interfaces (RPC calls, queries, subscriptions)
    - The proof formats (receipt proofs, light client proofs, etc.)

- This descriptor can optionally be paired with an **on-chain published schema**, allowing Time Bandits nodes to fetch live descriptors when interacting with new timelines.

- The `time-bandits` project will include a code generator that reads these descriptors and outputs type-safe `EffectAdapter` modules in Haskell (or other supported languages).

---

### Example Descriptor (TOML)

```toml
timeline_id = "ethereum-mainnet"
vm_type = "evm"
rpc_url_template = "https://mainnet.infura.io/v3/{api_key}"

[[assets]]
token = "USDC"
contract = "0xA0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"

[[effects]]
type = "EscrowToProgram"
method = "depositEscrow"
params = ["program_id", "resource_type", "amount"]
proof = "receiptProof"

[[effects]]
type = "ClaimFromProgram"
method = "withdrawEscrow"
params = ["program_id", "resource_type", "amount"]
proof = "receiptProof"
```

### What Gets Generated

For each timeline, the generator will produce:

- An EffectAdapter module exposing:
  - `applyEscrowToProgram :: ProgramId -> Resource -> IO ()`
  - `applyClaimFromProgram :: ProgramId -> Resource -> IO ()`
  - `watchResource :: ResourceKey -> IO ObservedState`

This adapter handles:

- Encoding and signing
- Transaction submission
- State querying
- Inclusion proof collection
- Standard error handling and retries
- The effect interpreter (EffectExecutor.hs) will call these adapters instead of custom timeline-specific code.

### Benefits

- Faster onboarding for new timelines
- Consistent handling across all conventional effects
- Clear separation between program logic and external system interfaces
- Standardized structured logging (every adapter logs exactly the same fields for all operations)
- Encourages developers to treat effects in a timeline-agnostic way

## Optional Path for Complex Effects

- For advanced cases, generation may not be feasible or desirable.
- For example:
  - Custom escrows requiring multi-sig challenge-response flows
  - Non-ABI based systems like Celestia namespaces or UTXO scripts
  - Cross-namespace proofs in DA layers
- In these cases, the descriptor can point to a hand-written plugin instead of trying to generate the code.
- The time-bandits framework will support falling back to a manually implemented `EffectAdapter` if the generator is insufficient for the timeline’s needs.

## Security Requirement
- Every generated adapter will be:
  - Versioned
  - Logged at generation time
  - Included in deterministic builds
- This ensures:
  - Generated code is auditable
  - Effect execution logs always point to the adapter version used at runtime
- This also allows re-running effects against the exact same adapter version during audits or re-execution.

## Tradeoffs and Limitations
- Keeping Timeline Descriptors Updated
  - If a timeline changes (e.g., new proof scheme), the descriptor and generator need updating. This creates operational coupling between timeline maintainers and Time Bandits maintainers.

- Limited Expressiveness
  - Declarative descriptors can handle most "call this method with args" cases. More complex workflows (multi-step flows, oracles, non-deterministic outcomes) are not a good fit for generation and will require hand-written logic.

- Proof Handling
  - Different timelines use radically different proof schemes. Proof validation will need to be plugged in separately — generation will only stub the calls to collect the proof.

- Code Quality
  - Generated code tends to be verbose and harder to debug than hand-written code. Strict logging and structured tracing will be essential.

- Trust Gap
  - Developers may distrust generated code. Full documentation and audit tooling will be required to address this.

## Final Decision

We will adopt the adapter generation approach for:
- Conventional effects that fit a "call function with args" model
- Primitive calls like asset transfers, escrows, and basic queries
- EVM-based chains first (Ethereum, Polygon, etc.), then extend to WASM and DA layers

We will not mandate generation for:
- Complex, multi-step protocols
- Proof systems that do not fit into a simple declarative schema
- Non-ABI systems like Celestia namespaces or custom rollup data models

The generator will support:
- Optional manual override (per effect, per timeline)
- Structured logging of every generated adapter call
- Strict versioning (every generated adapter includes a generator_version field)

This balances:
- Development velocity (faster onboarding via generation)
- Flexibility (manual overrides for edge cases)
- Auditability (every execution log links to the adapter version used)

## Action Items

- Define timeline.toml schema.
- Build initial EffectAdapterGenerator.hs for EVM.
- Migrate existing hand-written EVM adapters to generated format.
- Update EffectExecutor.hs to call generated adapters.
- Document fallback process for manual adapters.
- Build structured logger for all adapter operations.
- Update spec.md and system_contract.md to reference adapter generation.

## Appendix: Example Generated Interface

```haskell
module EffectAdapter.EthereumMainnet where
    applyEscrowToProgram :: ProgramId -> Resource -> IO ()
    applyClaimFromProgram :: ProgramId -> Resource -> IO ()
    watchResource :: ResourceKey -> IO ObservedState
```