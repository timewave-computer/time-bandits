# ADR 009: Evolution of the Simulation System and Unified Actor Control Plane

## Status

Accepted

## Context

The Time Bandits system currently supports three simulation modes:

- **In-memory:** All actors run in a single process, directly invoking each other’s functions.
- **Local multi-process:** Each actor runs in its own separate process, started by the simulation controller.
- **Geo-distributed:** Actors run on remote machines, started via SSH, and orchestrated by a central controller.

While these modes exist, the current implementation treats them as **separate concerns**, with limited common structure. As a result:
- There is **duplication of control logic** across modes.
- Scenarios are defined **procedurally**, instead of being declared up front.
- There is **no standard way to observe and modify running simulations**.
- There is **no first-class replay mechanism**, despite the importance of replay in a causal effect system.

At the same time, Time Bandits aims to support a clear **developer experience** where travelers can:
- Start by deploying programs that interact with a **single timeline**.
- Confidently evolve those programs into **multi-timeline workflows**.
- Replay and audit all program executions — across all timelines — from effect logs alone.

The existing simulation system does not adequately support these needs.


## Decision

### Unify the Simulation System

We will evolve the simulation system into a unified Scenario-Driven Simulation Framework for time-aware, effect-driven cross-timeline programs.

This will introduce:

- A **Scenario** type that defines all actors, timelines, initial facts, and invariants for a test.
- A **Controller** API that exposes lifecycle control (start, pause, resume, stop) and observability (logs, state queries, fact injection) for running scenarios.
- A **standard actor interface** that works identically in all three modes (in-memory, local-process, geo-distributed).
- A **unified log format** (append-only, content-addressed) that captures all facts and effects in every run.
- **Deterministic replay** that can fully reconstruct program state and causal history from logs alone.


### Evolve the 3 Simulation Modes

| Mode | Actor Execution | Communication | Deployment |
|---|---|---|---|
| In-memory | Direct function calls | In-memory message queue | `nix run` local controller |
| Local multi-process | `nix run` per actor process | IPC or sockets | `nix run` local controller |
| Geo-distributed | `nix copy` to remote host + remote execution | Secure RPC (TLS) | `nix run` remote deploy script |

The **controller** abstracts all these differences, so from a developer perspective, scenarios **just work** in all 3 modes.


### Scenario-First Simulation

Each simulation will start from a **TOML-defined scenario file**, which declares:

```toml
[scenario]
name = "CrossChainArbTest"
mode = "LocalProcesses"

[[actors]]
id = "trader1"
type = "Trader"

[[actors]]
id = "keeper_eth"
type = "TimeKeeper"
timeline = "Ethereum"

[[actors]]
id = "keeper_sol"
type = "TimeKeeper"
timeline = "Solana"

[[initialFacts]]
timeline = "Ethereum"
fact = { balance = { asset = "USDC", amount = 100 } }

[invariants]
noNegativeBalances = true
```


## Simplified Evolution from Single to Multi-Timeline

This structure makes it trivial to:
- Start with a single timeline program (one keeper, one program).
- Add timelines incrementally by adding `actors` to the scenario.
- The program code does not change — all cross-timeline coordination flows through the **effect pipeline**.
- The same logs and replay system work seamlessly as programs expand from single to multi-timeline.

This directly supports the goal of letting travelers start simple, **but grow into full cross-chain programs with no rewrites**.


## Implications for Current Architecture

### 1. Actor Lifecycle Moves to Controller

All actor startup logic (what binary, what env vars, what initial state) moves out of individual actors and into the **Scenario + Controller** system.

### 2. Actors Implement a Consistent Interface

Each actor (trader, keeper, bandit) implements:

```haskell
class Actor a where
    runActor :: a -> IO ()
    actorId :: a -> ActorID
```

This applies across all modes.

### 3. Actor Discovery Centralized

Actors no longer discover each other (e.g., a trader looking for a keeper). Instead, the **controller injects the necessary peer addresses** into each actor at startup.


### 4. Logging Standardized

All logs (effects, facts, events) follow the same append-only, content-addressed format, regardless of mode. This log format becomes the **canonical replay source**.


### 5. Observation Layer Added

A new **Observer** component watches:
- Effects applied by each actor.
- Facts observed by each actor.
- Lifecycle events (actor start/stop, errors).
This supports:
- Live invariant checks during tests.
- Live fault injection.
- External monitoring for developer convenience.


### 6. Unified Replay Engine

A single `replayScenario` function can reconstruct the full state of every actor by replaying its logs — across all 3 modes.


## Role of Nix

### Actor Packaging
Each actor (trader, keeper, bandit) is a **separate Nix flake**, with:
- `defaultPackage` (the actor binary for process/geo modes).
- `defaultApp` (the actor runnable via `nix run`).
- `devShell` (for local development of that actor).

### Controller Packaging
The **controller** flake depends on all actor flakes:
- When running in-memory, it calls actors directly.
- When running multi-process, it `nix run`'s actors as subprocesses.
- When running geo-distributed, it:
    - `nix build`'s the actor.
    - `nix copy`'s it to the remote.
    - `ssh` runs it remotely.

This achieves:
- Reproducibility.  
- No manual dependency management.  
- Seamless development across all 3 modes.  


### Example Actor Flake (Trader)

`simulation/actors/trader/flake.nix`:

```nix
{
  description = "Time Bandits Trader Actor";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.default = nixpkgs.haskellPackages.callCabal2nix "trader" ./. { };
    apps.default = {
      type = "app";
      program = "${self.packages.x86_64-linux.default}/bin/trader";
    };
  };
}
```


## Developer Experience

- Declare scenario in TOML.  
- `nix run .#controller -- --scenario ./scenarios/swap.toml` runs it anywhere.  
- Logs captured automatically in content-addressed format.  
- Replay with `nix run .#controller -- --replay ./logs/swap-20250310` recovers exact state.  
- Same controller works for all 3 modes.


## Benefits Summary

- Easy to evolve from single-chain to multi-chain programs.  
- Identical logic runs across dev, CI, and geo-distributed testnets.  
- Logs provide a **single source of truth** for audit and replay.  
- Developer tooling is **first-class** — inject facts, watch logs, assert invariants.  
- Full reproducibility via Nix.  
- Simplifies the runtime architecture by centralizing lifecycle control.


## Suggested New Files

| Path | Purpose |
|---|---|
| simulation/scenario/Scenario.hs | Scenario definition |
| simulation/scenario/ScenarioLoader.hs | TOML parsing |
| simulation/controller/Controller.hs | Unified control API |
| simulation/observer/Observer.hs | Log watchers, invariants, injectors |
| simulation/runner/InMemoryRunner.hs | In-memory runner |
| simulation/runner/LocalProcessRunner.hs | Multi-process runner |
| simulation/runner/GeoRunner.hs | Remote runner |
| simulation/actors/trader/flake.nix | Trader flake |
| simulation/actors/timekeeper/flake.nix | Keeper flake |
