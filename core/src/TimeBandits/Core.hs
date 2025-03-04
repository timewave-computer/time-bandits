{- |
Module: TimeBandits.Core
Description: Core functionality and primitives for the Time-Bandits system.

This module re-exports all components from the TimeBandits.Core.Core module.
It provides the fundamental primitives and abstractions for the Time-Bandits system,
including type classes, cryptographic functions, and error handling utilities.

Key components:
  * Basic type classes: 'Event' and 'Message' for the event-based architecture
  * Cryptographic functions for hashing, message verification, and signing
  * P2P networking helpers for node selection and rendezvous hashing
  * Core error handling utilities

The Core module serves as the foundation for the entire Time-Bandits architecture,
providing the essential building blocks used by all other components of the system.
-}
module TimeBandits.Core
  ( module TimeBandits.Core.Core
  ) where

import TimeBandits.Core.Core 