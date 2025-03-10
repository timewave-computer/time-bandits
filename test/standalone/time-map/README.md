# TimeMap Standalone Test

This is a standalone implementation of the TimeMap component from the Time Bandits system.

## Overview

The TimeMap is a central data structure in Time Bandits that tracks relationships between timelines. It represents timelines as nodes in a graph and connections between timelines as edges.

This standalone implementation includes:

- Core TimeMap data structures
- Timeline definition
- Operations for adding, linking, and querying timelines
- Utilities for rendering and validating the time map

## Features

- **Timeline Management**: Create and track timelines with unique IDs
- **Relationship Tracking**: Define relationships between timelines (fork, continue, merge)
- **Query Operations**: Find ancestors, descendants, roots, and leaf timelines
- **Graph Visualization**: Render the time map as a text-based graph

## Running the Test

Build and run the test with:

```bash
cabal build
cabal run
```

## Example Output

The test demonstrates a simple development workflow:

1. Creating multiple timelines (main, feature branches, release, hotfix)
2. Establishing relationships between them
3. Querying the resulting timeline graph

The output shows:
- The evolution of the TimeMap as timelines are connected
- Results of queries like finding ancestors, descendants, and common ancestors
- Visualization of the timeline relationships

## Integration with Time Bandits

This standalone implementation can be integrated back into the main Time Bandits system once the remaining issues are fixed. The core data structures and algorithms are compatible with the main codebase.

## Dependencies

- containers (for Map and Set)
- text (for text manipulation)
- cereal (for serialization)
- bytestring (for binary data) 