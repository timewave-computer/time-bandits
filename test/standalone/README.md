# Standalone Component Tests

This directory contains standalone implementations of key components from the Time Bandits system. These standalone versions are isolated from the main codebase and can be built and tested independently.

## Purpose

1. **Isolation**: Each component is implemented in isolation, without dependencies on problematic modules in the main codebase
2. **Verification**: The standalone tests verify that the core logic of each component works correctly
3. **Reference Implementation**: These implementations can serve as references when fixing issues in the main codebase

## Available Components

### CLI Deployment

Location: `cli-deployment/`

A standalone implementation of the CLI.Deployment module, which handles scenario configurations for deployments.

```bash
cd cli-deployment
cabal build
cabal run
```

### Fact Observation

Location: `fact-observation/`

A standalone implementation of the Fact Observation component, which defines rules for extracting and validating facts from blockchain data.

```bash
cd fact-observation
cabal build
cabal run
```

### TimeMap

Location: `time-map/`

A standalone implementation of the TimeMap component, which tracks relationships between timelines and provides a graph structure for navigating them.

```bash
cd time-map
cabal build
cabal run
```

## Adding New Components

When adding a new standalone component:

1. Create a new directory under `test/standalone/`
2. Set up a minimal cabal project with the necessary dependencies
3. Implement the core functionality without depending on problematic modules
4. Create a simple test program to demonstrate the component's functionality
5. Document how to build and run the test 