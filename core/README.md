# Core Module

This module contains shared types, data structures, and foundational logic that is used across the entire system. These components form the foundation of the time-bandits system.

## Components:

- **Effect.hs**: Defines the core effect system that enables time-travel operations
- **Resource.hs**: Defines the resources that can be manipulated across timelines
- **Timeline.hs**: Core timeline abstractions and operations
- **TimeMap.hs**: Manages mappings between timeline states and resources

All other modules in the system depend on these core abstractions. The code here should be kept stable and well-tested as changes can affect the entire system. 