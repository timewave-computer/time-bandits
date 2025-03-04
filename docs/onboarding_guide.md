# Time-Bandits Onboarding Guide

Welcome to the Time-Bandits project! This guide will help you get started with the codebase.

## Setting Up the Project

### Prerequisites

- [Nix](https://nixos.org/download.html) package manager
- Git

### Cloning the Repository

```bash
git clone https://github.com/timewave-computer/time-bandits.git
cd time-bandits
```

### Building with Nix

Time-Bandits uses a Nix flake for managing dependencies. To set up the development environment:

```bash
# Enable flakes if you haven't already
echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf

# Enter the development shell
nix develop

# Alternatively, if you use direnv
echo "use flake" > .envrc
direnv allow
```

### Building the Project

```bash
# Using cabal
cabal build

# Build and run tests
cabal test
```

## Running Simulations

### In-Memory Simulation

```bash
cabal run time-bandits -- sim in-memory --scenario basic
```

### Multi-Process Local Simulation

```bash
cabal run time-bandits -- sim local --scenario double-spend --traveler-count 3
```

### Running Individual Actors

```bash
# Run a Time Traveler
cabal run time-bandits -- traveler --id traveler1 --timeline ethereum

# Run a Time Keeper
cabal run time-bandits -- keeper --id keeper1 --timeline ethereum

# Run a Time Bandit
cabal run time-bandits -- bandit --id bandit1 --timeline ethereum
```

## Learning the Codebase

### Recommended Reading Order

1. Start with the `docs/codebase_overview.md` to understand the high-level architecture
2. Read the `core/README.md` to understand the core abstractions
3. Look at `docs/glossary.md` to familiarize yourself with the terminology
4. Examine a simple scenario in the `scenarios/` directory
5. Study the key modules in this order:
   - `Core.hs` - Core abstractions
   - `Timeline.hs` - Timeline operations
   - `Program.hs` - Program structure
   - `Effect.hs` - Effect system
   - `TimeTraveler.hs` - Main actor implementation

### Key Concepts

- **Timeline**: A sequence of states that resources can move through
- **Time Traveler**: An actor that can navigate and execute programs across timelines
- **Time Keeper**: An actor that maintains timeline consistency
- **Time Bandit**: An actor that attempts to exploit vulnerabilities in timelines
- **Effect**: An operation that can be performed on a timeline
- **Program**: A set of effects to be executed in sequence

## Getting Help

If you have questions or need help, please:

1. Check the documentation in the `docs/` directory
2. Look for examples in the `examples/` directory
3. Reach out to the team via the project's communication channels 