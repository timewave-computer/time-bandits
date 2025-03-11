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

Time-Bandits supports three primary simulation modes, each with progressively more realistic deployment conditions. The current implementation uses a scenario file-based approach.

### Running a Scenario

To run a simulation with a scenario file:

```bash
cabal run time-bandits -- run scenarios/basic_scenario.toml
```

You can enable verbose output with:

```bash
cabal run time-bandits -- --verbose run scenarios/basic_scenario.toml
# or
cabal run time-bandits -- -v run scenarios/basic_scenario.toml
```

> Note: The simulation system is continuously evolving. The command structure may change as development progresses. If the commands don't work, check the latest documentation or run `cabal run time-bandits -- help` for current usage.

### Simulation Modes

The Time-Bandits system supports these execution modes:

1. **In-Memory Mode**: All actors run in a single process, with direct function calls. This is ideal for development and initial testing.

2. **Local Multi-Process Mode**: Each actor runs in its own separate process, started by the simulation controller. This better simulates real-world conditions while still running locally.

3. **Geo-Distributed Mode**: Actors run on remote machines, started via SSH, and orchestrated by a central controller. This is the most realistic deployment mode for production testing.

The simulation mode is specified in the scenario configuration file.

### Creating Custom Scenarios

Scenarios are defined in TOML files in the `scenarios/` directory. A scenario defines:

```toml
[scenario]
name = "CrossChainArb"
mode = "LocalProcesses"  # Can be InMemory, LocalProcesses, or Distributed

[[actors]]
id = "trader1"
type = "Trader"

[[actors]]
id = "keeper_eth"
type = "TimeKeeper"
timeline = "Ethereum"

[[initialFacts]]
timeline = "Ethereum"
fact = { balance = { asset = "USDC", amount = 100 } }
```

To create a new scenario, copy an existing one and modify it for your needs.

## Working with Content-Addressable Code

Time-Bandits implements a content-addressable code storage system, which identifies code by content hash rather than by name. This approach provides significant advantages when working on the codebase:

### Basic Operations

To interact with the content-addressable code system, use the `content-addressable-tool` utility:

```bash
# Store a code file in the repository
cabal run content-addressable-tool -- store path/to/file.hs

# Look up code by name or hash
cabal run content-addressable-tool -- lookup nameOrHash

# Execute code by name or hash
cabal run content-addressable-tool -- execute nameOrHash
```

### Key Benefits for Developers

1. **Dependency Precision**: When referring to code, you can specify the exact version by its content hash.
2. **Safe Refactoring**: Renaming functions or reorganizing modules won't break existing code references.
3. **Multiple Versions**: You can maintain multiple versions of the same function and use them simultaneously.
4. **Immutable History**: Code definitions are never overwritten, preserving the entire development history.

### Programming Model

When developing new modules:

1. Each function or module gets a unique content hash based on its implementation.
2. Names are lightweight metadata pointers to the underlying code.
3. References between modules use content hashes for stability.
4. The content-addressable executor handles resolving and executing code by hash.

### Example Workflow

```haskell
-- Define a function (will be assigned a content hash)
let functionName = "calculateTotal"
let functionBody = "values.reduce((a, b) => a + b, 0)"
let functionHash = hashFunction functionName functionBody

-- Store with a name
registerName repo functionName functionHash

-- Later, you can "rename" without changing the implementation
registerName repo "computeSum" functionHash  -- Both names point to the same code
```

## Running Tests

### Running the Test Suite

To run the complete test suite:

```bash
cabal test
```

For more detailed test output and reports:

```bash
./run-test-report.sh
```

This generates a comprehensive test report in the `test-report-out` directory, with the latest report always available at `test-report-out/current_report.md`.

### Test Report Structure

The test report includes:

- **Summary**: Overall test statistics and status
- **Module Status**: Status of major system components
- **All Tests and Their Status**: Complete list of tests with modification dates
- **Test Mode Scenario Tests**: Details about tests in different execution modes

### Test Modes

Time-Bandits tests run in different modes to ensure functionality across various deployment scenarios:

1. **In-Memory Mode**: Tests with all actors in a single process
2. **Local Multi-Process Mode**: Tests with actors in separate local processes
3. **Geo-Distributed Mode**: Tests environment setup for distributed deployment

The test report provides detailed information about test execution in each mode and shows the most recently modified tests at the top.

### Creating New Tests

When adding new tests, follow these steps:

1. Create a test file in the appropriate directory under `test/`
2. Add your test to the main test suite in `test/TestMain.hs`
3. Update the `Last Modified` date in the test report to ensure they appear at the top of the report
4. Run the test report generation script:
   ```bash
   ./run-test-report.sh
   ```
5. Verify your test appears in the report and passes

For testing across multiple simulation modes, use the `TestModeScenarioTest.hs` as a reference.

## Learning the Codebase

### Recommended Reading Order

1. Start with the `docs/codebase_overview.md` to understand the high-level architecture
2. Read the `core/README.md` to understand the core abstractions
3. Look at `docs/glossary.md` to familiarize yourself with the terminology
4. Examine the different simulation modes in `docs/adr_009_simulation_system.md`
5. Examine a simple scenario in the `scenarios/` directory
6. Study the key modules in this order:
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
- **Scenario**: A complete test configuration that defines actors, timelines, and initial states
- **Simulation Mode**: The execution environment for the scenario (in-memory, local, geo-distributed)
- **Test Report**: A comprehensive record of all test executions and their results

## Getting Help

If you have questions or need help, please:

1. Check the documentation in the `docs/` directory
2. Look for examples in the `examples/` directory
3. Check the test reports for examples of functionality
4. Reach out to the team via the project's communication channels 