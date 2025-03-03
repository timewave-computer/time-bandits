# Time Bandits Refactor Plan

Including:

- Program-based execution with explicit resource ownership
- Causal ordering via Lamport clocks and time maps
- Invocation model with safe resource passing
- Multi-mode simulation (in-memory, local, geo-distributed)
- Nix flakes for per-actor builds & simulation controller

## Phase 1: Core Type Abstractions & Separation of Concerns

**Goal**: Establish clear Timeline, Resource, Program, Effect separation.

### Step 1.1 Introduce Timeline, Resource, Program, and Effect as First-Class Types

- Move timeline interactions into `Timeline.hs`
- Define resources as first-class entities in `Resource.hs`
- Define program state and memory in `Program.hs`
- Define effects as explicit operations in `Effect.hs`

Example Type Structure:

```haskell
data Timeline = Timeline 
    { timelineId :: TimelineId 
    , eventLog :: [Event] 
    , clock :: TimelineClock 
    }

data Resource 
    = TokenBalanceResource TokenId Address Amount 
    | EscrowReceiptResource EscrowId 
    | ContractWitnessResource ContractId Value 
    | SyntheticInternalMarker Text

data Program = Program 
    { programId :: ProgramId 
    , executionState :: ProgramState 
    , memory :: ProgramMemory 
    }

data Effect 
    = EscrowToProgram Resource ProgramId MemorySlot 
    | InvokeProgram ProgramId FunctionName [Resource] 
    | ClaimFromProgram ProgramId MemorySlot Resource 
    | DelegateCapability Capability ProgramId Expiry 
    | WatchResource ResourceKey Condition Trigger 
    | AtomicBatch [Effect]
```

### Step 1.2 Replace Implicit State Updates with Explicit Effect Execution

- All timeline changes should now happen via Effect application.
- No more direct state modification—everything must go through an effect handler.
- Implement initial effect execution pipeline in `EffectExecutor.hs`:

```haskell
applyEffect :: ProgramState -> Effect -> Either ExecutionError ProgramState
applyEffect state (EscrowToProgram res pid slot) = modifyMemory pid slot (Just res) state
applyEffect state (InvokeProgram pid fn args) = executeProgram pid fn args state
applyEffect state (ClaimFromProgram pid slot res) = claimResource pid slot res state
```

## Phase 2: Implement Program Invocation & Resource Ownership Model

**Goal**: Enable programs to own and transfer resources explicitly.

### Step 2.1 Implement EscrowToProgram & ClaimFromProgram

- Introduce escrow memory slots inside `ProgramMemory.hs`.
- Programs lose ownership of resources when escrowing them.
- Only the target program can claim escrowed resources.

Example in `ProgramMemory.hs`:

```haskell
data MemorySlot = MemorySlot Text

data ProgramMemory = ProgramMemory 
    { slots :: Map MemorySlot Resource }

escrowToProgram :: ProgramMemory -> MemorySlot -> Resource -> ProgramMemory
escrowToProgram mem slot res = mem { slots = insert slot res (slots mem) }

claimFromProgram :: ProgramMemory -> MemorySlot -> Maybe (Resource, ProgramMemory)
claimFromProgram mem slot = do
    res <- lookup slot (slots mem)
    let newMem = mem { slots = delete slot (slots mem) }
    return (res, newMem)
```

### Step 2.2 Implement InvokeProgram for Cross-Program Calls

- Implement function call execution between programs.
- Programs should declare entry points and allow safe invocation.

Example in `InvokeProgram.hs`:

```haskell
invokeProgram :: ProgramId -> FunctionName -> [Resource] -> Either ExecutionError ProgramState
invokeProgram pid fn args = do
    program <- lookupProgram pid
    case lookupFunction fn program of
        Just f -> f args
        Nothing -> Left FunctionNotFound
```

## Phase 3: Implement Time Maps & Lamport Clocks

**Goal**: Ensure programs execute in causal order across timelines.

### Step 3.1 Introduce LamportClock.hs

- Track logical time per timeline and per program.
- Enforce strict happens-before ordering for all invocations.

```haskell
data LamportClock = LamportClock { counter :: Int }

incrementLamport :: LamportClock -> LamportClock
incrementLamport (LamportClock c) = LamportClock (c + 1)
```

### Step 3.2 Implement TimeMap.hs to Track Cross-Timeline State

- Maintain a snapshot of multiple timelines.
- Programs execute against a consistent time map.

```haskell
data TimeMap = TimeMap 
    { timelines :: Map TimelineId LamportClock
    , observedHeads :: Map TimelineId BlockHeader 
    }
```

### Step 3.3 Integrate Time Map Checks in EffectExecutor.hs

- Before executing an effect, verify that time constraints hold.

```haskell
applyEffect state (InvokeProgram pid fn args) = do
    ensureCausalOrder state.pid fn args
    executeProgram pid fn args state
```

## Phase 4: Implement Multi-Mode Simulation

**Goal**: Support in-memory, local multi-process, and geo-distributed execution.

### Step 4.1 Implement Controller (Controller.hs)

- Decides execution mode (InMemory | LocalProcesses | GeoDistributed).
- Starts actors in the correct way (function call, process spawn, SSH).

```haskell
data SimulationMode = InMemory | LocalProcesses | GeoDistributed

runSimulation :: SimulationMode -> Scenario -> IO ()
runSimulation mode scenario = case mode of
    InMemory -> runInMemory scenario
    LocalProcesses -> runLocally scenario
    GeoDistributed -> runGeoDistributed scenario
```

### Step 4.2 Implement Actor Abstraction

- All actors implement the same interface, whether in-memory or spawned as a process.

```haskell
class Actor a where
    runActor :: a -> IO ()
    actorId :: a -> ActorID
```

### Step 4.3 Implement Scenario.hs for Defining Simulations

- Scenarios define actors, their roles, and execution mode (TOML-based).

Example scenario:

```TOML
mode = "LocalProcesses"
actors = [
  { id = "trader1", type = "Trader" },
  { id = "pricefeed1", type = "PriceFeed" }
]
```

### Step 4.4 Implement Nix Flakes for Actors & Controller

- Each actor is a standalone flake.
- The controller aggregates and launches them.

```nix
{
  description = "Time Bandits Simulation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    trader.url = "./simulation/actors/trader";
    pricefeed.url = "./simulation/actors/pricefeed";
    controller.url = "./simulation/controller";
  };

  outputs = { self, nixpkgs, trader, pricefeed, controller }:
  let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
  in {
    packages = {
      inherit (trader.packages) default;
      inherit (pricefeed.packages) default;
      inherit (controller.packages) default;
    };
  };
}
```

### Step 4.5 Implement Geo-Distributed Deployment (Deploy.hs)

- Allows spawning remote actors via SSH.

```haskell
startActorRemote :: ActorSpec -> IO ()
startActorRemote spec = do
    let host = lookupHostForActor (actorId spec)
    _ <- spawnProcess "ssh" [host, "nix run", flakeForActorType (actorType spec)]
    pure ()
```

## Phase 5: Implement Final Security & Invariant Checks

**Goal**: Enforce system-level safety guarantees.

### Step 5.1 Enforce Single-Owner Rule in ProgramMemory.hs

- Only one program can hold a resource at a time.

```haskell
validateOwnership :: ProgramMemory -> Resource -> Either ExecutionError ()
validateOwnership mem res = 
    if res `elem` (elems (slots mem))
        then Left DoubleSpend
        else Right ()
```

### Step 5.2 Implement Replayable Execution Checks in Verifier.hs

```haskell
verifyCausalOrder :: ExecutionLog -> Bool
verifyCausalOrder log = all isValidCausalRelation (generatePairs log)
```

### Step 5.3 Implement Security Property Tests

**Property: No Double Spend**
- Verify validateOwnership prevents reuse.

**Property: No Implicit Invocation**
- Ensure all program calls happen via InvokeProgram.

**Property: No Causal Violations**
- Ensure all execution logs respect LamportClock order.

## Final Deliverables

- Timeline.hs
- Resource.hs
- Program.hs
- Effect.hs
- EffectExecutor.hs
- InvokeProgram.hs
- LamportClock.hs
- TimeMap.hs
- Controller.hs
- Scenario.hs
- Deploy.hs
- Nix flakes for per-actor builds
- TOML-based scenario files
