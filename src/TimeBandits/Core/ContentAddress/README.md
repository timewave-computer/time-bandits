# Content Address Module

## Overview

The Content Address module provides a system for content-addressable code storage and retrieval in the Time Bandits system. Content-addressable storage is a method where data is stored and retrieved based on its content rather than its location, using cryptographic hash functions to generate unique identifiers for each piece of content.

This approach offers several benefits:
- **Deduplication**: Identical code is stored only once, reducing redundancy
- **Immutability**: Stored code cannot be modified without changing its address
- **Referential integrity**: References to code are always valid as long as the content exists
- **Distribution**: Content-addressed data is easily distributed across systems

## Components

The Content Address module consists of the following components:

### Types.hs

Defines the core types used throughout the content-addressable system:
- `CodeHash`: A unique identifier for a piece of code
- `CodeDefinition`: Represents a stored code entity with its hash, source, and type
- `DefType`: Classifies code as modules, functions, types, classes, or instances
- `CodeRepository`: The storage system for code definitions

### Hash.hs

Provides functions for generating content hashes:
- Hash generation for functions, modules, and other code entities
- Consistent hash computation algorithms

### Repository.hs

Implements the storage and retrieval mechanisms:
- Creating new repositories
- Storing code definitions
- Looking up code by hash or name
- Registering names for hashes

### Util.hs

Utility functions for working with content-addressable code:
- Traversing codebases to extract and process code
- Parsing code into appropriate structures
- Populating repositories from existing code

## Usage Examples

```haskell
-- Create a new repository
repo <- newCodeRepository

-- Store a function definition
let functionName = "add"
    functionBody = "add :: Int -> Int -> Int\nadd x y = x + y"
hash <- addFunctionToRepository repo functionName functionBody

-- Look up a definition by hash
definition <- lookupByHash repo hash

-- Look up a definition by name
definitionByName <- lookupByName repo "add"

-- Populate a repository from a directory
repo <- populateRepositoryFromDirectory "./src/MyProject"
```

## Integration

The Content Address module integrates with other components of the Time Bandits system:
- **Core.Common**: Reuses common types and serialization
- **Execution**: Enables execution of content-addressed code
- **TimeMachine**: Supports versioning and history tracking

## Future Enhancements

Planned improvements for the Content Address module:
- Integration with the Temporal Effect Language (TEL)
- Distributed repository synchronization
- Enhanced search capabilities for code discovery
- Dependency tracking between code entities 