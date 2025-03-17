# TimeBandits.Core.Common

This module provides fundamental types and utilities used throughout the Time Bandits system. It serves as the foundation for the type system and should have minimal dependencies.

## Components

### Types (`TimeBandits.Core.Common.Types`)

Core types used across the codebase:
- Hash types (Hash, EntityHash, etc.)
- Cryptographic types (PubKey, PrivKey, Signature)
- Time related types (LamportTime)
- Actor types (Actor)
- Error types (AppError, NetworkError, etc.)

### Serialization (`TimeBandits.Core.Common.Serialize`)

Centralized serialization functionality:
- Serialization/deserialization functions
- Base type instances
- Conversion utilities for different formats (Text, ByteString, Base64)

### Utilities (`TimeBandits.Core.Common.Utils`)

General-purpose utility functions:
- String utilities (camelToSnake, snakeToCamel, etc.)
- List utilities (groupByKey, uniqueByKey, etc.)
- IO utilities (file operations, directory management)
- Time utilities (timestamp formatting, conversion)
- Error handling utilities (exception catching) 