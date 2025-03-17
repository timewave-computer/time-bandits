# TimeMap

## Overview

The TimeMap module provides the core functionality for managing and tracking timelines and their relationships within the Time Bandits system. This module is central to implementing the time-travel mechanics, ensuring consistent state across timelines, and maintaining the integrity of the timeline structure.

## Key Concepts

- **Timeline**: A sequence of states and events that represent a particular history.
- **TimelineId**: A unique identifier for each timeline, typically a hash of its content.
- **TimeMap**: A data structure maintaining relationships between timelines.
- **TimeNode**: A representation of a timeline within the TimeMap, including its connections.
- **TimeEdge**: A connection between timelines representing their relationships (fork, merge, continue).
- **TimeBranch**: A sequence of connected timelines forming a logical branch.

## Module Structure

The TimeMap module is organized into the following components:

### Types.hs

Defines all core type definitions for working with the TimeMap:
- TimeMap and its constituent parts
- TimeNode and TimeEdge representations
- TimeBranch for track continuous branches
- EdgeType for specifying relationship types

### Operations.hs

Provides the public API for working with the TimeMap:
- Creation operations (empty, singleton, fromList)
- Modification operations (addTimeline, mergeTimelines, linkTimelines)
- Query operations (getTimeline, getAncestors, getDescendants)
- Validation functions (validateTimeline, validateTimeMapIntegrity)
- Utility functions (renderTimeMap, timeMapToGraph)

## Usage Examples

### Creating a TimeMap

```haskell
-- Create an empty TimeMap
let emptyMap = empty

-- Create a TimeMap with a single timeline
let singleMap = singleton timeline

-- Create a TimeMap from a list of timelines
let multiMap = fromList [timeline1, timeline2, timeline3]
```

### Adding and Linking Timelines

```haskell
-- Add a timeline to an existing TimeMap
let updatedMap = addTimeline newTimeline timeMap

-- Link two timelines with a 'fork' relationship
let linkedMap = linkTimelines parentId childId EdgeTypeFork timeMap
```

### Querying Timeline Relationships

```haskell
-- Get all ancestors of a timeline
let ancestors = getAncestors timelineId timeMap

-- Find the common ancestor of two timelines
let commonAncestor = findCommonAncestor timeline1Id timeline2Id timeMap

-- Get all leaf (latest) timelines
let latestTimelines = getLatestTimelines timeMap
```

## Integrity and Validation

The TimeMap module enforces several rules to maintain timeline integrity:

1. **Acyclicity**: The timeline graph must not contain cycles.
2. **Consistency**: Child timelines must be consistent with their parents' state.
3. **Uniqueness**: Each timeline has a unique identifier.
4. **Connectivity**: The timeline graph should maintain proper connectivity.

## Design Goals

- **Efficient queries**: Fast access to timeline relationships.
- **Integrity maintenance**: Ensuring the timeline structure remains valid.
- **Flexible relationships**: Supporting various timeline relationships (fork, merge, continue).
- **Visualization support**: Tools for rendering the timeline structure.

## Integration with Other Modules

The TimeMap module integrates with:
- Core.Timeline for the basic timeline definitions
- TimeBandits.Core.Resource for tracking resources across timelines 