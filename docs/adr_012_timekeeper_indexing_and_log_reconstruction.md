# ADR 011: Time Keeper as External Indexer Proxy and Unified Log Reconstruction


## Status

**Proposed**


# Context

In early versions of Time Bandits, Time Keepers acted as **simulated blockchain nodes** with complete control over the timeline. This included:

- Direct ingestion of simulated transactions.
- Custom indexing and fact extraction.
- Writing a **FactLog** directly to disk.
- Serving fact queries to programs from that log.

However, this model does not align with real blockchain deployments. Many production blockchains:

- **Do not support custom indexing natively**.
- **Expose limited RPC query surfaces**.
- **Have no native notion of program-specific facts**.

As a result, Time Keepers will migrate from being **embedded simulated nodes** to **external indexer proxies** that:

- Connect to **real blockchain nodes** (full, archive, RPC).
- Observe timeline data using **pre-configured fact extraction rules**.
- Gossip these facts to Bandits and optionally persist them.
- Expose fact queries using **either direct node queries** or derived local FactLogs.
- No longer own the canonical FactLog — Bandits and external users can reconstruct FactLogs themselves by reapplying fact extraction rules to blockchain data.


# Decision

## Revised Time Keeper Role

| Responsibility | Previous (Simulated) | New (Real Blockchain Client) |
|---|---|---|
| Timeline Control | Owned timeline data | Observes external timeline |
| Fact Extraction | Direct from internal data | Queries from external node, applies rules |
| Fact Storage | Writes direct FactLog | Optional — Bandits can reconstruct |
| Fact Proofing | Internal proof | Uses real blockchain proofs (inclusion, headers) |
| Fact Gossip | Gossip to Bandits | Same |
| Fact Queries | From local log | From node or reconstructed log |
| Transaction Acceptance | Accept program messages | Same |
| Timeline State Queries | Serve direct state queries | Proxy to real node |


## Unified Log and FactLog Construction as On-Demand Process

### Previous
- Each Time Keeper maintained its own FactLog.
- Replay relied directly on these.

### New
- FactLogs can be constructed by **any Bandit or external user** with:
    - Direct blockchain access (RPC).
    - Fact extraction rules.
- Unified Logs for programs will still exist, but they will link to:
    - Facts provided by Time Keepers (real-time or derived).
- This makes FactLogs a **derived product** rather than a source of truth.
- The unified log of a program links to the **FactIDs**, ensuring causal traceability — the **facts themselves** can be fetched later, even if the original Time Keeper disappears.


## Actor Data Sharing Interface

To support **fact retrieval** from other actors who may have the data, a new **Data Discovery and Request Protocol** will be added.

### Actor Request Interface
Every actor exposes:
```haskell
class DataProvider where
    queryFact :: FactID -> IO (Maybe Fact)
    queryLogSegment :: SegmentID -> IO (Maybe LogSegment)
    queryFactLogRange :: TimelineID -> (LamportTime, LamportTime) -> IO [Fact]
```


## Fact Observation Pipeline (Revised)

| Step | Action |
|---|---|
| 1 | Keeper connects to real blockchain node (full node, archive, RPC). |
| 2 | Keeper loads fact extraction rules (TOML). |
| 3 | As blocks arrive, the keeper extracts matching facts. |
| 4 | Facts are signed by the keeper (proving it observed them). |
| 5 | Facts are gossiped to Bandits. |
| 6 | Bandits update their **local FactLog cache** (optional). |
| 7 | Bandits and programs refer to facts only by `FactID`, not the full content. |
| 8 | External tools (dashboards, auditors) can reconstruct full FactLogs using the same rules. |


## Example Fact Extraction Rule (TOML)

```toml
[[observation]]
type = "PriceObservation"
path = "uniswapV3Pool:ETH/USDC.price"
proof = "inclusion"

[[observation]]
type = "DepositObservation"
path = "timebandits.escrow.deposit"
proof = "inclusion"
```


## Example Fact (Gossiped)

```json
{
    "factID": "bafy123...",
    "timeline": "Ethereum",
    "factType": "PriceObservation",
    "factValue": {"ETH/USDC": 2900},
    "observedAt": 12345678,
    "proof": {
        "blockHeader": "...",
        "proofPath": ["0xabc", "0xdef"],
        "signedBy": "keeper.eth"
    }
}
```


## Simulation vs Production Config

| Environment | Timeline Source | FactLog Source | Query Source |
|---|---|---|---|
| In-Memory Sim | Embedded mock timeline | Direct writes | Direct queries |
| Local Process Sim | Embedded mock timeline | Direct writes | Direct queries |
| Geo-Distributed | Real blockchain nodes | Derived from RPC | Queries via RPC or reconstructed log |


## New CLI/Config Options

### Keeper Config Example (TOML)

```toml
[keeper]
timeline = "Ethereum"
mode = "Real"
rpcEndpoint = "https://mainnet.infura.io/v3/...-api-key"
factRules = "./fact_rules/ethereum.toml"
```


## New Public FactLog Construction Tool

To support dashboards and 3rd party tools, provide a CLI:

```bash
nix run .#factlog-reconstruct -- \
    --timeline Ethereum \
    --rpc https://mainnet.infura.io/v3/... \
    --rules ./fact_rules/ethereum.toml \
    --out ./ethereum_factlog.jsonl
```


## Visualizations and Dashboards

- Dashboards can consume reconstructed FactLogs alongside program Unified Logs.
- This allows visualization of:
    - Cross-timeline causal flow.
    - Program execution progress.
    - Fact-to-effect causality chains.


## Benefits

- Supports real blockchain deployments without requiring native indexing changes.  
- Separates fact observation from storage — Time Keepers can remain lightweight.  
- Enables third-party fact and effect visualization.  
- Keeps FactLogs reconstructible independently of Time Keeper storage.  
- Provides a consistent interface for fact discovery across all simulation and real modes.  
- Allows Time Bandits to support any blockchain with reasonable RPC access.  


## Example Visualization Flow

1. Dashboard fetches program’s Unified Log.
2. Dashboard queries FactIDs referenced in each effect.
3. Dashboard either:
    - Fetches facts from Bandits (if cached).
    - Reconstructs facts from blockchain using fact rules.
4. Dashboard renders full causal DAG, including external facts.


## Architectural Implications

| Component | Change |
|---|---|
| Time Keeper | No longer primary FactLog store (optional). |
| Bandits | May store derived FactLogs. |
| Replay | Replay uses derived FactLogs if needed. |
| Visualization | Dashboards query facts directly or reconstruct them. |
| External Auditors | Can independently reconstruct FactLogs. |


## Invariant

- All replayable programs must have a complete causal trace linking to **FactIDs**.  
- All referenced facts must be either:
    - Present in Bandit cache.
    - Reconstructible from blockchain data and fact rules.
- Programs can only observe external state through these facts — no direct RPC queries.

