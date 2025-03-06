# Refactor: Module Wiring

This document outlines the plan for cleaning up the module structure and imports in the Time Bandits codebase.

**Note: The remaining tasks from this plan have been moved to `refactor_rewire_2.md`. Please refer to that file for the current task list.**

### Audit of Module Imports

We need to audit all module imports to ensure:

1. No circular dependencies
2. Clear separation of concerns
3. Proper abstraction boundaries
4. Minimal coupling between subsystems

### Module Structure

The new module structure will be:

```
src/
├── Core/              # Core types and data structures
├── Programs/          # Program model and state management
├── Execution/         # Program execution and effect handling
├── Adapters/          # External system interfaces
├── Actors/            # Actor model implementations
├── Simulation/        # Simulation framework
├── Proofs/            # Cryptographic proofs and verification
└── CLI/               # Command-line interface
```

### Import Policy

1. Core modules should not import from any other subsystem
2. Programs should only import from Core
3. Execution can import from Core and Programs
4. Adapters should only import from Core
5. Actors can import from Core, Programs, and Execution
6. Simulation can import from Core, Programs, Execution, and Actors
7. Proofs can import from Core, Programs, and Simulation
8. CLI can import from any subsystem

This structure prevents coupling and makes the codebase easier to understand and document for contributors.

### Completed Tasks

✅ Consolidate all types into `src/Core/`
✅ Split `Effect` into data vs interpreter
✅ Split `Program` into static vs mutable state
✅ Introduce `PreconditionEvaluator`
✅ Introduce `ResourceLedger`
✅ Build `TimelineAdapter` typeclass
✅ Move all external calls into `Adapters/`
✅ Standardize logging via `ExecutionLog`
✅ Centralize all effect handling into `EffectInterpreter`
✅ Move all scenario handling and messaging to `Simulation/`
✅ Introduce `Actor.hs` and unify actor lifecycle handling
✅ Clean up `Main.hs` into a pure CLI dispatcher
✅ Consolidate test helpers
✅ Tighten module exports and apply consistent naming
✅ Review and apply Haddock headers
✅ Document the clean dependency graph in `docs/codebase_overview.md`

### Specific Refactorings

1. Move `TimeBandits.Programs.Types` to `Programs.Types`
2. Move `TimeBandits.Core.Timeline` to `Core.Timeline`
3. Move `TimeBandits.Core.TimeMap` to `Core.TimeMap`
4. Move `TimeBandits.Core.Resource` to `Core.Resource`
5. Move `TimeBandits.Effects` to `Execution.Interpreter`
6. Move `TimeBandits.Controller` to `Execution.Controller`
7. Move `TimeBandits.Actor` to `Actors.Actor`
8. Move `TimeBandits.Network` to `Adapters.Network`
9. Move `TimeBandits.Scenario` to `Simulation.Scenario`
10. Move `TimeBandits.CLI.Main` to `CLI.Main`

**For remaining implementation tasks and details, please see `refactor_rewire_2.md`.**

## File structure

time-bandits/
├── src/
│   ├── Core/                        # Fundamental data types and basic helpers (no logic)
│   │   ├── Effect.hs                 # Effect data definition (no logic)
│   │   ├── Resource.hs               # Resource data only
│   │   ├── Timeline.hs               # Timeline data only
│   │   ├── TimeMap.hs                 # Time map data only
│   │   ├── Program.hs                 # Static program definition
│   │   ├── ProgramId.hs               # Strongly typed program identifiers
│   │   ├── ResourceId.hs              # Strongly typed resource identifiers
│   │   ├── TimelineId.hs              # Strongly typed timeline identifiers
│   │   ├── Common.hs                   # Utility types (Asset, Address, etc.)
│
│   ├── Programs/                     # Program lifecycle and mutable state
│   │   ├── ProgramState.hs            # Mutable memory, log pointers, etc.
│   │   ├── Preconditions.hs           # Preconditions as data
│
│   ├── Execution/                    # Core interpreter, state updates, logging
│   │   ├── EffectInterpreter.hs       # Unified interpreter (effect application lifecycle)
│   │   ├── PreconditionEvaluator.hs   # Centralized guard checking
│   │   ├── ResourceLedger.hs          # Global ownership tracking
│   │   ├── ExecutionLog.hs            # Structured append-only log handling
│
│   ├── Adapters/                      # External timeline adapters
│   │   ├── TimelineAdapter.hs         # Adapter typeclass
│   │   ├── EthereumAdapter.hs         # EVM-specific implementation
│   │   ├── CelestiaAdapter.hs         # Celestia-specific implementation
│   │   ├── MockAdapter.hs             # For testing
│
│   ├── Actors/                        # Individual actor logic
│   │   ├── Actor.hs                    # Shared actor interface
│   │   ├── TimeTraveler.hs             # Time Traveler behavior
│   │   ├── TimeKeeper.hs               # Time Keeper behavior
│   │   ├── TimeBandit.hs                # Time Bandit behavior
│
│   ├── Simulation/                    # Orchestration and scenarios
│   │   ├── Controller.hs               # Simulation driver
│   │   ├── Messaging.hs                # Actor-to-actor message types
│   │   ├── Scenarios.hs                 # Scenario parsing (YAML)
│
│   ├── Proofs/                        # Proof generation and verification
│   │   ├── ProofGenerator.hs
│   │   ├── ProofVerifier.hs
│
│   ├── CLI/                           # CLI entrypoints
│   │   ├── Main.hs                      # CLI dispatcher (minimal)
│
├── test/                             # Tests (unit, property, scenario)
│   ├── TestSupport.hs                  # Common test fixtures and mocks
│   ├── Core/
│   │   ├── TimeMapSpec.hs
│   │   ├── EffectSpec.hs
│   ├── Execution/
│   │   ├── EffectInterpreterSpec.hs
│   ├── Simulation/
│   │   ├── ScenarioRunnerSpec.hs
│
├── docs/                             # Project documentation
│   ├── README.md                      # High-level project intro
│   ├── system_contract.md             # Formal system contract
│   ├── spec.md                         # Technical specification
│   ├── refactor.md                     # Refactor plan (and addenda)
│   ├── codebase_overview.md            # Explanation of file structure
│   ├── onboarding_guide.md             # Getting started for developers
│   ├── glossary.md                     # Key terms and definitions
│   ├── architecture_diagram.png        # Diagram showing system flow
│
├── scenarios/                        # Example scenarios for simulation
│   ├── simple.yaml                     # Example in-memory scenario
│   ├── crosschain_trade.yaml           # Multi-timeline example
│
├── scripts/                          # Helper scripts
│   ├── run-simulation.sh               # Example helper for launching
│
├── flake.nix                         # Nix flake (adjusted to new layout)
├── cabal.project                     # Updated cabal project file

