# Time Bandits

A Haskell library for distributed time travel.

## Overview

Time Bandits is a framework for creating distributed programs that execute across multiple timelines with explicit ownership tracking. It enables the safe and verifiable transfer of resources between programs, providing clear tracking of causality and resource ownership.

The framework's core abstractions are:

- **Programs**: Stateful, resource-owning entities that execute according to defined rules
- **Resources**: Explicit state objects owned by programs that can be transferred between them
- **Timelines**: Causally ordered event streams with their own consistency models
- **Actors**: Entities that interact with the system by submitting messages
- **Controllers**: Entities that enforce the system contract and validate transitions

## Architecture

The Time Bandits architecture has several core abstractions:

- **Programs** have state and memory with explicit resource contracts
- **Resources** are owned by programs and can be transferred between them
- **Timelines** are causally ordered event streams with their own consistency models
- **Effects** are explicit operations that programs can perform
- **Controllers** enforce the system contract across different simulation modes

All operations follow a strict contract:
1. Each program transition is triggered by a TransitionMessage
2. Messages include proof of resource control and guard validation
3. Applied effects produce entries in an append-only execution log
4. All resource ownership is tracked explicitly

## Actor Roles

Time Bandits defines three specialized actor roles:

### Time Travelers

Time Travelers are the primary users of the system and have the following responsibilities:
- Creating and deploying new programs with initial time maps
- Submitting TransitionMessage objects to advance program state
- Querying program states and timeline information
- Managing resource ownership and transfers

Time Travelers interact with programs by creating transition messages that represent desired state changes. Each transition must be validated by Time Keepers before it can be applied.

### Time Keepers

Time Keepers are responsible for maintaining the integrity of timelines:
- Validating incoming transition messages against timeline rules
- Accepting or rejecting messages based on their validity
- Providing timeline state queries to authorized actors
- Enforcing consistency rules within and across timelines

Time Keepers act as the trusted authority for timelines, ensuring that all state transitions follow the system's rules and maintain causal consistency.

### Time Bandits

Time Bandits operate the underlying P2P network:
- Executing programs and generating cryptographic proofs
- Maintaining the execution log across the distributed network
- Facilitating communication between system components
- Providing the infrastructure for distributed execution

Time Bandits work behind the scenes to ensure the system runs smoothly, generating the cryptographic proofs needed to verify program execution and maintaining the distributed log of all operations.

## Simulation Modes

The system supports three simulation modes:

1. **In-Memory Mode**: All actors run in the same process for rapid testing and development
2. **Local Multi-Process Mode**: Actors run in separate processes on the same machine, communicating via IPC
3. **Geo-Distributed Mode**: Actors run on separate machines, communicating over the network

## Getting Started

### Prerequisites

- GHC 9.4 or later
- Cabal 3.6 or later
- Nix with flakes enabled (for multi-process mode)

### Building

```bash
cabal build
```

### Running

```bash
cabal run time-bandits -- --scenario examples/resource_transfer.toml
```

### Testing

```bash
cabal test
```

## Example Scenario

Here's an example of a scenario defined using TOML files:

```toml
# resource_transfer.toml
[scenario]
name = "Resource Transfer Example"
mode = "LocalProcesses"

[[time_travelers]]
id = "alice"
capabilities = ["ResourceCreation", "ResourceTransfer"]
resources = ["resource_1", "resource_2"]

[[time_travelers]]
id = "bob"
capabilities = ["ResourceReceive"]
resources = []

[[time_keepers]]
id = "ethereum_keeper"
timelines = ["ethereum_main"]
validation_rules = ["EthereumValidation"]

[[time_bandits]]
id = "node_1"
network_role = "Executor"
proof_generators = ["ZkResourceTransfer"]

[programs.escrow]
type = "EscrowProgram"
initial_state = { locked = false }
```

This scenario defines a simple resource transfer between two Time Travelers, with a Time Keeper validating the transfer and a Time Bandit executing the program and generating proofs.

## License

This project is licensed under the Apache License 2.0 - see the LICENSE file for details.

```haskell
-- | Interpreter configuration for controlling effect inclusion
data InterpreterConfig = InterpreterConfig
    { -- | How to handle trace logs
      traceConfig :: TraceConfig
    }

-- | Default interpreter configuration
defaultConfig :: InterpreterConfig
defaultConfig = InterpreterConfig
    { traceConfig = SimpleTracing
    }
```

You can run the application with different configurations:

```haskell
-- Using default configuration
result <- interpretAppEffects timeRef resourceLogRef storeRef subsRef program

-- Using verbose configuration
result <- interpretWithConfig verboseConfig timeRef resourceLogRef storeRef subsRef program

-- Using silent configuration
result <- interpretWithConfig silentConfig timeRef resourceLogRef storeRef subsRef program
```

The application supports command line options for controlling logging:

- `--verbose` - Enable verbose logging with timestamps
- `--silent` - Disable all tracing
- (default) - Standard logging without timestamps

## Development, Building, and Running

```bash
# Enter the development shell
nix develop

# Build the project
nix build

# Run the application
nix run

# Run with verbose logging
nix run -- --verbose

# Run with tracing disabled
nix run -- --silent
```

