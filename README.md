# Time Bandits

![Time Bandits](./map_of_time.png)

Time Bandits is a distributed system for coordinating resources across different timelines i.e. blockchains.
Naming is in reference to the 1981 Terry Gilliam film, [Time Bandits](https://en.wikipedia.org/wiki/Time_Bandits).
The system allows "time travellers" to write distributed programs that interact with multiple timelines at once.

This is a protoype for working through various ideas about how to write a secure distribtued programming language as the composition of algebraic effects, along with the supporting P2P architecture.

## Overview

### Actors
- Time Traveler: Users who can deploy programs to travel across time
- Time Bandit: Nodes that perform P2P transport of light client proofs (+ eventually execution proofs)
- Time Keeper: Entities that sign-off on updates to a designated timeline (light clients)

### Data structures
- Hash-linked append-only logs
- Content addressed identities, types, messages, and data
- Commitments to partially ordered state per timeline are represented via trie

### Lamport Clocks
- Used for causal ordering
- Each timeline and program has its own lamport clock
- The Map of Time is a partial order of all timelines, syncronized by cross-timeline messages

### P2P + Transient Storage
- Uses rendezvous hashing with a replication factor
- Time Bandit nodes subscribe to timelines to provide program I/O
- All messages are cryptographically signed

### Sharded On-chain Storage
- Each timeline maintains its own event trie, updated by Time Bandits
- All events are hash linked and content addressed
- "Map of Time" registers actors, resources, and programs

### Resource Management
- UTXO-like resource system
- Tracks ownership and metadata with append-only log

### Effect System

The application uses a composable effect system built with Polysemy, which allows for separation of concerns via type-safe handling of side effects.

TA modular interpreter system allows for configurable trace logging, selective effect inclusion/exclusion, and consistent ordering of interpreters.

Trace configuration:

- `NoTracing` - Disable all trace logs
- `SimpleTracing` - Enable standard trace logs
- `VerboseTracing` - Enable detailed trace logs with timestamps

The verbose tracing mode adds timestamps to each trace message, providing more detailed context about when each operation occurs. This is particularly useful for debugging timing issues or understanding the sequence of operations.

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

