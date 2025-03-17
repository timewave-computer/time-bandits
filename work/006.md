# Refactor 008: Step-by-Step Implementation Checklist for Simulation System Rework


## Objective

This document provides a **detailed, step-by-step implementation plan** for evolving Time Bandits’ simulation system into a **unified scenario-driven framework** as described in [ADR 009: Simulation System](../docs/adr_009_simulation_system.md).

Each step includes:
- **Context** - why the step matters.
- **Implementation strategy** - how to do it.
- **Affected code** - specific modules/files to modify or create.
- **Interfaces** - data types, functions, and interfaces to add.
- **Success criteria** - what makes this step complete.
- **Testing plan** - how to verify correctness.

This document assumes **no prior knowledge of our discussion**, so all steps are self-contained.


# Step 1 - Define the Scenario Data Structure

### Context
The scenario is the **source of truth** for any simulation run. It defines actors, timelines, initial facts, and invariants.

### Implementation Strategy
- Create `simulation/scenario/Scenario.hs`
- Define:
```haskell
data Scenario = Scenario
    { name :: Text
    , mode :: SimulationMode
    , actors :: [ActorSpec]
    , initialFacts :: [FactSpec]
    , invariants :: [InvariantSpec]
    }

data SimulationMode = InMemory | LocalProcesses | GeoDistributed

data ActorSpec = ActorSpec
    { actorID :: ActorID
    , actorType :: ActorType
    , timeline :: Maybe TimelineID
    }

data FactSpec = FactSpec
    { timeline :: TimelineID
    , fact :: Fact
    }

data InvariantSpec = InvariantSpec
    { name :: Text
    }
```

- Actors link to a timeline (for keepers) or are standalone (trader).

### Affected Code
- New file: `simulation/scenario/Scenario.hs`

### Success Criteria
- Scenario data structure exists.
- Can represent a simple single-timeline simulation (trader + keeper).

### Testing
- Create a sample `Scenario` in a test file.
- Serialize to TOML and back.
- Assert roundtrip preserves structure.


# Step 2 - Implement Scenario Loader

### Context
Scenarios should be defined in TOML files, loaded at runtime by the controller.

### Implementation Strategy
- Add `simulation/scenario/ScenarioLoader.hs`
- Use `toml-parser` or `tomland` library.
- Parse:
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

[[initialFacts]]
timeline = "Ethereum"
fact = { balance = { asset = "USDC", amount = 100 } }

[invariants]
noNegativeBalances = true
```

### Affected Code
- New file: `simulation/scenario/ScenarioLoader.hs`

### Success Criteria
- TOML parses into `Scenario`.

### Testing
- Roundtrip parse and serialize in unit test.


# Step 3 - Define Actor Interface

### Context
All actors (traders, keepers, bandits) must follow a **uniform interface** to work across all modes.

### Implementation Strategy
- Create `simulation/actors/Actor.hs`.
- Define:
```haskell
class Actor a where
    runActor :: a -> IO ()
    actorId :: a -> ActorID
```

- Implement this for:
    - Trader
    - TimeKeeper
    - Bandit

### Affected Code
- New file: `simulation/actors/Actor.hs`
- Modify trader, keeper, bandit to implement this.

### Success Criteria
- Each actor can be invoked via `runActor`.

### Testing
- Call `runActor` directly in test for each actor type.


# Step 4 - Implement Controller Interface

### Context
The controller abstracts how actors are started, stopped, and interacted with.

### Implementation Strategy
- Create `simulation/controller/Controller.hs`.
- Define:
```haskell
data Controller = Controller
    { queryActorState :: ActorID -> IO ActorState
    , injectFact :: Fact -> IO ()
    , observeLog :: ActorID -> IO [LogEntry]
    , pauseActor :: ActorID -> IO ()
    , resumeActor :: ActorID -> IO ()
    , stopScenario :: IO ()
    }
```

- Implement controller for all 3 modes:
    - In-memory
    - LocalProcesses
    - GeoDistributed

### Affected Code
- New file: `simulation/controller/Controller.hs`
- Create: `InMemoryRunner.hs`, `LocalProcessRunner.hs`, `GeoRunner.hs`.

### Success Criteria
- Controller can start and stop actors across all 3 modes.

### Testing
- In a test, load a scenario, run it in each mode, query state from actors.


# Step 5 - Introduce Standard Logging

### Context
All actors must log effects, facts, and events in a standard content-addressed append-only log.

### Implementation Strategy
- Create `Core.Log`.
- Define:
```haskell
data LogEntry = LogEffect Effect
              | LogFact Fact
              | LogEvent Event
```

- Implement append-only `writeLog` and `readLog`.

### Affected Code
- New file: `Core/Log.hs`
- Modify actors to write to this log.

### Success Criteria
- Logs are written for all core actions (deposit, withdraw, observe).

### Testing
- Run simple program, inspect log.


# Step 6 - Add Observers

### Context
Observers inject faults, watch logs, and check invariants during runs.

### Implementation Strategy
- Create `simulation/observer/Observer.hs`
- Define:
```haskell
data Observer = Observer
    { onEffect :: Effect -> IO ()
    , onFact :: Fact -> IO ()
    , onEvent :: Event -> IO ()
    , injectFault :: IO ()
    , checkInvariants :: IO [InvariantViolation]
    }
```

- Add observer support to controller.

### Affected Code
- New file: `simulation/observer/Observer.hs`
- Modify controller to install observers.

### Success Criteria
- Observers can log all events and inject a fact during a run.

### Testing
- Run test scenario with mock observer, check observer log.


# Step 7 - Implement Replay System

### Context
Every scenario should be fully replayable from logs.

### Implementation Strategy
- Create `simulation/replay/ReplayEngine.hs`.
- Implement:
```haskell
replayScenario :: FilePath -> IO ()
```

- Reads all logs.
- Replays into a fresh controller and verifies state matches.

### Affected Code
- New file: `simulation/replay/ReplayEngine.hs`

### Success Criteria
- Can replay from logs and get identical state.

### Testing
- Run scenario, save logs, replay, check final balances.


# Step 8 - Integrate All into CLI

### Context
The CLI (`Main.hs`) should expose:
- `--scenario` to run a scenario.
- `--replay` to replay logs.

### Implementation Strategy
- Modify `simulation/controller/Main.hs`.
- Wire:
    - `loadScenario`
    - `runScenario`
    - `replayScenario`

### Affected Code
- Modify `Main.hs`

### Success Criteria
- `nix run .#controller -- --scenario path/to/scenario.toml` works.
- `nix run .#controller -- --replay path/to/logs` works.

### Testing
- Run CLI tests covering both.


# Final Success Criteria

- Scenarios load from TOML.  
- Actors implement standard interface.  
- Controller starts, stops, manages actors across all 3 modes.  
- Logs capture all effects, facts, events.  
- Observers provide live insight and fault injection.  
- Full replay works deterministically.


# Suggested File Name

