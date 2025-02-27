# Time Bandits

![Time Bandits](./map_of_time.png)

Time Bandits is a distributed system for coordinating resources across different timelines i.e. blockchains.
Naming is in reference to the 1981 Terry Gilliam film, [Time Bandits](https://en.wikipedia.org/wiki/Time_Bandits).
The system allows "time travellers" to write distributed programs that interact with multiple timelines at once.

This is a protoype for working through various ideas about how to write a secure distribtued programming language as the composition algebraic effects, along with the supporting P2P architecture.

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
- "Map of Time registers actors, resources, and programs

### Resource Management
- UTXO-like resource system
- Tracks ownership and metadata with append-only log


## Status

### Implemented
- Core type system and data structures
- Event processing
- Cryptographic operations
- Basic effect system
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
