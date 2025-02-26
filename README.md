# Time Bandits: Distributed Timetravel

![Time Bandits](./map_of_time.png)

Time Bandits is a distributed system for coordinating resources across different timelines, owing it's name to the 1981 Terry Gilliam film of same name. The system allows actors to define and execute operations that interact with different timelines, writing time travel programs by composing structured effects.

This was written as a protoype to work through some ideas about how to write a distribtued programming language using algebraic effects, as well as the supporting distributed system architecture.

## Overview

### Actors
- Time Traveler: Users who can deploy programs to travel across time
- Time Bandit: P2P transport and proof generation nodes
- Time Keeper: Entities that sign-off on updates to a given timeline (light clients)

### P2P + Transient Storage
- Uses rendezvous hashing with replication factor
- Time Bandit nodes subscribe to timelines to provide I/O

### Sharded On-chain Storage
- Each timeline maintains its own event trie, updated by Time Bandits
- All events are hash linked and content addressed
- Registers actors, resources, and programs

### Lamport Clocks for Causal Ordering
- Each timeline has its own lamport clock
- Each program has it's own lamport clock
- The "Map of Time" is a partial order of all timelines, syncronized by cross-timeline txs

### Resource Management
- UTXO-like resource capability system
- Resource state tracking with ownership and metadata


## Status

### Implemented
- Core type system and data structures
- Event processing
- Cryptographic operations
- Effect system
- Resource model

### In Progress
- Timeline synchronization
- Cross-timeline operations
- Event storage
- Actor registration
- P2P message routing via rendezvous hashing
- Code restructuring
- Program that does anything useful

### Planned
- Test suite
- Effect composition DSL
- State snapshots and pruning
- P2P node discovery
