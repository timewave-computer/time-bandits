# Time Bandits

A framework for causally consistent cross-timeline operations, resource management, and provable effects.

## Architecture Overview

The Time Bandits system implements a causally consistent execution environment across multiple timelines (blockchains, rollups, event logs, etc.) with clear actor role separation and verifiable state transitions.

### Key Components

#### 1. Actor Roles

The system explicitly separates three actor roles:

- **Time Travelers**: Deploy programs and submit transition messages to timelines
- **Time Keepers**: Maintain individual timelines, validate messages against timeline rules
- **Time Bandits**: Operate P2P execution network, enforce causal order, maintain time maps

#### 2. TimeMap

The `TimeMap` type is a first-class concept that:
- Tracks observed timeline heads and timestamps
- Maintains per-timeline Lamport clocks
- Is explicitly passed to every effect application
- Is updated after every successful transition

#### 3. Execution Log

All operations are recorded in a structured, content-addressed execution log with:
- Applied effect
- Timestamp
- Parent effect hash (causal link)
- Resulting program state hash
- Attached proofs

#### 4. Timeline Descriptors

Each supported timeline has a `timeline.toml` descriptor that defines:
- Clock mechanism (block height, slot number, timestamp)
- Resource mappings
- Effect adapters
- RPC endpoints
- State query methods

#### 5. Centralized Effect Interpreter

The `EffectInterpreter` enforces the system contract by:
- Validating effect preconditions using the current `TimeMap`
- Applying effects through appropriate timeline adapters
- Updating program memory
- Updating the `TimeMap`
- Appending to the `ExecutionLog`

#### 6. Cross-Program Resource Flow

The system enforces strict resource ownership rules:
- Every resource has exactly one owner at any time
- Cross-program transfers use explicit escrow/claim operations
- All transfers leave a traceable log entry proving custody transfer

## Running Scenarios

The system supports three simulation modes:
- **InMemory**: All actors operate in a single process
- **LocalProcesses**: Actors run in separate processes with Unix socket messaging
- **GeoDistributed**: Actors run on remote machines with TCP/RPC messaging

Scenarios can be defined in TOML files:

```bash
# Run an example scenario
cabal run time-bandits-scenario -- examples/simple_scenario.toml
```

## Development

The project uses a Nix-based development environment:

```bash
# Enter development environment
nix develop

# Build the project
cabal build

# Run tests
cabal test
```

## Documentation

- [Refactor Plan](docs/refactor.md): Comprehensive refactoring plan
- [Timeline API](docs/timeline_api.md): Timeline API documentation
- [Resource Model](docs/resource_model.md): Resource ownership model

## Example Scenarios

The `examples/` directory contains scenario definitions:

- `simple_scenario.toml`: Basic cross-timeline resource transfer
- `causal_ordering_scenario.toml`: Demonstration of TimeMap and causal ordering
- `execution_log_scenario.toml`: Structured execution log with causal links
- `timeline_descriptors/`: Example timeline descriptor files

## License

This project is licensed under the Apache License 2.0 - see the LICENSE file for details.
