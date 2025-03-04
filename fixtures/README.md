# Time Bandits Fixtures

This directory contains fixture files used by tests and examples in the Time Bandits system.

## Directory Structure

- `timelines/`: Timeline specification files
  - `ethereum.toml`: Ethereum timeline specification
  - `solana.toml`: Solana timeline specification
  - `ethereum_mainnet.toml`: Ethereum mainnet configuration
  - `solana_mainnet.toml`: Solana mainnet configuration

- `resources/`: Sample resource definitions used in tests and examples

## Usage

These fixtures are used by both the test suite and the example scenarios. They provide consistent, reusable configurations that can be referenced from multiple places in the codebase.

### In Tests

```haskell
-- Load a timeline descriptor from a fixture
descriptor <- loadDescriptor "fixtures/timelines/ethereum.toml"
```

### In Examples

Example scenarios can reference these fixtures:

```toml
# In an example scenario
timeline = "ethereum_mainnet"
timeline_config = "fixtures/timelines/ethereum_mainnet.toml"
``` 