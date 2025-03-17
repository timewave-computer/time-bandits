# Resource Management

## Overview

The Resource Management module provides a comprehensive system for creating, tracking, and transferring ownership of resources within the Time Bandits system. Resources are any items of value that can be owned, transferred, or consumed by entities within the system.

## Key Concepts

- **Resource**: Any item of value that can be owned and transferred. Examples include token balances, escrow receipts, and contract witnesses.
- **ResourceId**: A unique identifier for each resource.
- **OwnerId**: An identifier for an entity that can own resources.
- **Ownership**: The relationship between a resource and its owner, tracked in the ResourceLedger.
- **Escrow**: A mechanism for temporarily holding resources with conditions for their release.

## Module Structure

The Resource module is organized into the following components:

### Types.hs

Defines all core type definitions for resources:
- Resource data types (TokenBalanceResource, EscrowReceiptResource, etc.)
- ResourceLedger and its supporting types
- Ownership records and history tracking
- Escrow and claim conditions

### Operations.hs

Provides the public API for working with resources:
- Resource creation, transfer, and consumption
- Resource ownership verification
- Escrow operations (escrow, claim, release)
- Resource querying and error handling

### Ledger.hs

Implements the resource ledger functionality:
- Low-level ledger operations
- Ownership tracking and management
- Resource state management
- Locking and concurrency control

## Usage Examples

### Creating a Resource

```haskell
createResource ownerId tokenResource
```

### Transferring a Resource

```haskell
transferResource resourceId currentOwner newOwner
```

### Placing a Resource in Escrow

```haskell
escrowResource resourceId owner beneficiary claimCondition
```

## Error Handling

The module uses specialized error types to handle various failure scenarios:
- Resource not found
- Insufficient permissions
- Resource already consumed
- Invalid escrow claims
- Concurrency errors

## Design Goals

- **Single-owner invariant**: Each resource has exactly one owner at any point in time.
- **Atomic transfers**: Ownership changes happen atomically to prevent race conditions.
- **Comprehensive history**: All ownership changes are recorded for auditability.
- **Flexible resource types**: The system can handle various resource types through its extensible design.
- **Conditional transfers**: The escrow system allows for conditional transfers based on specified criteria.

## Integration with Other Modules

The Resource Management module integrates with:
- TimeBandits.Common.ContentAddress for resource identification
- TimeBandits.Core.TimeMap for timeline-aware resource operations 