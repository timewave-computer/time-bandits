# Celestia Adapter Implementation Plan

This document outlines the plan for implementing the `CelestiaAdapter` module for Time Bandits.

## Overview

The `CelestiaAdapter` will implement the `TimelineAdapter` typeclass to provide integration with Celestia's data availability layer. Unlike traditional blockchains like Ethereum, Celestia is primarily focused on data availability and consensus, making this adapter somewhat different in its implementation approach.

## Implementation Requirements

1. The adapter should follow the `TimelineAdapter` interface defined in `src/Adapters/TimelineAdapter.hs`
2. Maintain clear separation between Celestia-specific code and the core Time Bandits logic
3. Support both testnet and mainnet environments
4. Provide robust error handling and retry mechanisms
5. Include proper logging of all operations
6. Handle Celestia's unique data availability and blob-focused architecture

## Module Structure

```haskell
module Adapters.CelestiaAdapter
  ( createCelestiaAdapter
  -- Re-exports from TimelineAdapter for convenience
  , TimelineAdapter(..)
  , AdapterConfig(..)
  , AdapterState(..)
  , AdapterError(..)
  -- Celestia-specific operations
  , CelestiaConfig(..)
  , CelestiaState(..)
  , submitBlob
  , queryNamespace
  , getLatestHeight
  ) where
```

## Core Components

### Configuration

```haskell
data CelestiaConfig = CelestiaConfig
  { rpcUrl :: String                  -- Celestia RPC URL
  , apiToken :: Maybe String          -- Optional API token for authentication
  , privateKeys :: [String]           -- Private keys for controlled accounts
  , defaultNamespace :: ByteString    -- Default namespace for blob submissions
  , confirmationBlocks :: Int         -- Number of blocks for finality
  , pollInterval :: Int               -- Milliseconds between state polls
  , feeStrategy :: FeeStrategy        -- Strategy for fee determination
  }

data FeeStrategy
  = StaticFee Integer
  | DynamicFee Double                -- Multiplier of recommended fee (1.0 = recommended)
```

### State Management

```haskell
data CelestiaState = CelestiaState
  { celestiaClient :: CelestiaClient  -- Connection to Celestia node
  , managedAccounts :: [Address]      -- Accounts we control
  , pendingSubmissions :: Map TxHash PendingSubmission -- Submissions awaiting confirmation
  , nonces :: Map Address Integer     -- Track nonces for each account
  , recentBlocks :: [BlockInfo]       -- Cache of recent blocks
  , currentFeeRate :: Integer         -- Current network fee rate
  }

data PendingSubmission = PendingSubmission
  { blobId :: TxHash
  , namespace :: ByteString
  , sentAt :: UTCTime
  , confirmedInBlock :: Maybe BlockHeight
  , retryCount :: Int
  , originalData :: ByteString
  }
```

## Key Functions

1. **Adapter Creation**
   ```haskell
   createCelestiaAdapter :: CelestiaConfig -> IO TimelineAdapter
   ```

2. **Core TimelineAdapter Implementation**
   ```haskell
   -- Implement required TimelineAdapter methods:
   -- - getBalance (adapted for Celestia token balances)
   -- - getTransactions (adapted for blob submissions)
   -- - getLatestBlockHeight
   -- - submitTransaction (adapted for blob submission)
   -- - verifyTransaction (verify blob inclusion)
   -- - watchForTransaction (watch for blob inclusion)
   ```

3. **Celestia-specific Functions**
   ```haskell
   -- Functions unique to Celestia
   submitBlob :: CelestiaState -> ByteString -> ByteString -> IO (Either AdapterError TxHash)
   queryNamespace :: CelestiaState -> ByteString -> BlockHeight -> IO (Either AdapterError [BlobInfo])
   getNamespaceProof :: CelestiaState -> ByteString -> TxHash -> IO (Either AdapterError InclusionProof)
   estimateFee :: CelestiaState -> ByteString -> IO (Either AdapterError Integer)
   ```

4. **State Management Functions**
   ```haskell
   updateNonce :: Address -> Integer -> CelestiaState -> CelestiaState
   trackPendingSubmission :: TxHash -> ByteString -> ByteString -> CelestiaState -> CelestiaState
   confirmSubmission :: TxHash -> BlockHeight -> CelestiaState -> CelestiaState
   ```

## Celestia API Integration

The adapter will need to interact with several Celestia-specific endpoints:
1. Blob submission
2. Namespace querying
3. Balance checking
4. Block height retrieval
5. Proof verification

This will require mapping between the Time Bandits abstract concepts and Celestia's specific architecture:

| Time Bandits Concept | Celestia Equivalent |
|--------------------|-------------------|
| Transaction | Blob Submission |
| Block | Celestia Block |
| Address | Celestia Account Address |
| Transaction Receipt | Blob Inclusion Proof |
| Balance | Token Balance |

## Dependencies

- `celestia-api` - Haskell library for Celestia RPC calls (may need to be created)
- `cryptonite` - For cryptographic operations
- `text`, `bytestring` - For data handling
- Other Time Bandits core modules for integration

## Testing Strategy

1. **Unit Tests**
   - Mock RPC responses for deterministic testing
   - Test each component function in isolation

2. **Integration Tests**
   - Connect to Celestia testnets (Mocha, Arabica)
   - Perform real blob submissions with test data

3. **Simulation Tests**
   - Test within the Time Bandits simulation environment
   - Verify interaction with other components

## Implementation Steps

1. Research and understand Celestia's unique architecture and API
2. Set up the basic module structure
3. Implement the connection and configuration handling
4. Build core Celestia interaction functions (blob submission, namespace queries)
5. Implement the TimelineAdapter typeclass methods
6. Add error handling and retry logic
7. Implement namespace monitoring features
8. Add comprehensive testing
9. Document usage patterns and examples

## Challenges and Considerations

1. Celestia's data availability focus makes it different from traditional blockchains
2. The adapter may need to handle namespace management more explicitly
3. The mapping between TimelineAdapter concepts and Celestia concepts will require careful design
4. Depending on Celestia's API maturity, we may need to adapt to changes during development 