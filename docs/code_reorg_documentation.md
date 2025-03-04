# Codebase Reorganization and Documentation Plan

- ✅ Review the existing directory and module structure to identify **tightly related functionality** that should live together.
- ✅ Group code into **high-level functional areas** based on system roles and responsibilities:

    ```
    time-bandits/
    ├── core/                    # Shared types, data structures, foundational logic
    │   ├── Effect.hs
    │   ├── Resource.hs
    │   ├── Timeline.hs
    │   ├── TimeMap.hs
    ├── programs/                 # Program-specific logic
    │   ├── Program.hs
    │   ├── ProgramMemory.hs
    │   ├── Preconditions.hs
    ├── actors/                    # Time travelers, keepers, bandits
    │   ├── TimeTraveler.hs
    │   ├── TimeKeeper.hs
    │   ├── TimeBandit.hs
    │   ├── Messaging.hs
    ├── execution/                 # Unified interpreter, logging, state transitions
    │   ├── EffectInterpreter.hs
    │   ├── ExecutionLog.hs
    ├── adapters/                   # External timeline adapters
    │   ├── EthereumAdapter.hs
    │   ├── CelestiaAdapter.hs
    ├── proofs/                     # Proof generation and verification
    │   ├── ProofGenerator.hs
    │   ├── ProofVerifier.hs
    ├── cli/                        # CLI entrypoints
    │   ├── Main.hs                  # Simulation controller, entrypoint
    ├── docs/                       # Internal documentation
    │   ├── refactor.md
    │   ├── spec.md
    │   ├── system_contract.md
    │   ├── codebase_overview.md
    │   ├── onboarding_guide.md
    │   ├── dev_workflow.md
    │   ├── glossary.md
    │   ├── architecture_diagram.png
    ├── scripts/                    # Helper scripts for common workflows
    ├── flake.nix                    # Updated to match new layout
    ├── cabal.project                # Updated to match new layout
    ```

- ✅ Consolidate **scattered types and helpers** into these functional areas — remove free-floating types from arbitrary files.
- ✅ Define a **clear import convention**:
    - Top-level modules import only from their own functional area.
    - Functional areas import from `core`, but do not import directly from each other (except via explicit public interfaces if necessary).
- ✅ Add a **top-level README.md** inside each major folder (core, programs, actors, etc.) briefly explaining what the folder contains.
- ✅ Create a new document: `docs/codebase_overview.md` explaining:
    - What the main components are.
    - What each folder does.
    - Typical flow of data and control during program execution.
- ✅ Create a new `docs/onboarding_guide.md` explaining:
    - How to clone and build the project (Nix setup, dependencies).
    - How to run tests, simulations, and individual actors.
    - What to read first when learning the codebase.
- ✅ Add `docs/dev_workflow.md` explaining:
    - How to add a new effect.
    - How to onboard a new timeline.
    - How to update the system contract.
    - How to add a property test for an invariant.
- ✅ Update the **root README.md** to add a **clear Getting Started section**, including:
    - How to clone.
    - How to run a simple scenario.
    - Where to find architectural documentation.
- ✅ Add a `scripts/` folder with helper scripts for:
    - Starting an in-memory simulation.
    - Launching a multi-process local simulation.
    - Starting a geo-distributed simulation.
    - Running all property tests.
- ✅ Ensure every **public module** has a top-level Haddock comment explaining:
    - What the module does.
    - What role it plays in the overall architecture.
- ✅ Review `Main.hs` — refactor it into a **thin entrypoint** that does mode dispatch only.
- ✅ Move actual simulation logic into `Controller.hs` under `cli/`, keeping `Main.hs` clean.
- ✅ Review all module exports — make sure each module only exports what external code actually needs.
- ✅ Add a `docs/glossary.md` that defines all key system terms, including:
    - Timeline
    - Time Traveler
    - Time Keeper
    - Time Bandit
    - Effect
    - Resource
    - Time Map
    - Execution Log
    - Program Memory
- ✅ Add `docs/architecture_diagram.md` — detailed diagram showing:
    - Programs
    - Effects
    - Time Map
    - Execution Log
    - Actors (Travelers, Keepers, Bandits)
    - External Timelines
- ✅ Once the file structure is improved, update:
    - `flake.nix` so all new modules are visible to the build.
    - `cabal.project` so all new modules are correctly included.
  