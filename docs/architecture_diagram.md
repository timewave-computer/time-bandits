# Time-Bandits Architecture Diagram

This diagram illustrates the key components and relationships of the Time-Bandits system.

```mermaid
graph TB
  %% Define subgraphs for each major component
  subgraph TimeBanditsSystem
    direction TB

    %% Time Keepers
    subgraph TimeKeepers
      direction TB
      TK1[Time Keeper 1]
      TK2[Time Keeper 2]
      TK3[Time Keeper 3]
    end

    %% Bandits
    subgraph Bandits
      direction TB
      B1[Bandit 1]
      B2[Bandit 2]
      B3[Bandit 3]
    end

    %% Travelers
    subgraph Travelers
      direction TB
      T1[Traveler 1]
      T2[Traveler 2]
    end

    %% External Timelines
    subgraph ExternalTimelines
      direction TB
      ET1[Blockchain A]
      ET2[Blockchain B]
    end

    %% Unified Log
    UL[Unified Log]
  end

  %% Define relationships between components
  %% Time Keepers observe external timelines
  ET1 -- Observes --> TK1
  ET2 -- Observes --> TK2

  %% Time Keepers synchronize time maps
  TK1 -- Syncs Time Map --> TK2
  TK2 -- Syncs Time Map --> TK3

  %% Bandits receive time maps from Time Keepers
  TK1 -- Provides Time Map --> B1
  TK2 -- Provides Time Map --> B2
  TK3 -- Provides Time Map --> B3

  %% Travelers propose effects to Bandits
  T1 -- Proposes Effect --> B1
  T2 -- Proposes Effect --> B2

  %% Bandits apply effects and update the unified log
  B1 -- Applies Effect --> UL
  B2 -- Applies Effect --> UL
  B3 -- Applies Effect --> UL

  %% Bandits synchronize effects among themselves
  B1 -- Syncs Effect --> B2
  B2 -- Syncs Effect --> B3
  B3 -- Syncs Effect --> B1

  %% Travelers query the unified log
  T1 -- Queries --> UL
  T2 -- Queries --> UL
```

Key Objects:

- Time Keepers: Nodes responsible for observing external timelines (e.g., blockchains) and maintaining synchronized time maps.​
- Bandits: Executor nodes that apply effects proposed by travelers, update the unified log, and synchronize effects among themselves.​
- Travelers: Users or entities that propose effects (actions or transactions) to be executed within the Time Bandits system.​
- External Timelines: External systems or blockchains that Time Keepers observe to maintain accurate time maps.​
- Unified Log: A centralized log that records all applied effects, ensuring consistency and traceability within the system.​

Key Interactions:

- Time Keepers observe external timelines to gather data and synchronize time maps among themselves.​
- Bandits receive these synchronized time maps from Time Keepers to ensure they operate based on the latest external state.​
- Travelers propose effects to Bandits, which are then applied and recorded in the unified log.
- Bandits synchronize applied effects among themselves to maintain system-wide consistency.​
- Travelers can query the unified log to verify the status and history of applied effects.​