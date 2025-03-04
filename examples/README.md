# Time Bandits Examples

This directory contains executable examples that demonstrate the Time Bandits system in action. These examples serve as both documentation and practical guides for working with the system.

## Directory Structure

- `scenarios/`: Simple examples focused on demonstrating specific features
  - `simple_transfer.toml`: Basic resource transfer between actors
  - `execution_log.toml`: Demonstrates the execution logging system
  - `causal_ordering.toml`: Shows causal ordering of operations

- `simulations/`: More complex examples that demonstrate multiple system components working together
  - `multi_timeline.toml`: Cross-timeline operations
  - `distributed_actors.toml`: Geo-distributed actor deployment

## Running Examples

Examples can be run using the Time Bandits CLI:

```bash
# Run a simple scenario
time-bandits run examples/scenarios/simple_transfer.toml

# Run a more complex simulation
time-bandits run examples/simulations/multi_timeline.toml --verbose
```

See the individual example files for specific documentation about what each example demonstrates. 