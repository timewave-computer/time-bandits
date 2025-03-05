# Ethereum Adapter Implementation Plan

This document outlines the plan for implementing the `EthereumAdapter` module for Time Bandits.

## Overview

The `EthereumAdapter` will implement the `TimelineAdapter` typeclass to provide integration with Ethereum-based blockchains. It will handle:

- Reading blockchain state (balances, transactions, blocks)
- Submitting transactions to the Ethereum network
- Verifying transaction inclusions/receipts
- Managing account/wallet interactions
- Tracking blockchain events

## Implementation Requirements

1. The adapter should follow the `TimelineAdapter` interface defined in `src/Adapters/TimelineAdapter.hs`
2. Maintain clear separation between Ethereum-specific code and the core Time Bandits logic
3. Support both testnet and mainnet environments
4. Provide robust error handling and retry mechanisms
5. Include proper logging of all operations

## Module Structure

```haskell
module Adapters.EthereumAdapter
  ( createEthereumAdapter
  -- Re-exports from TimelineAdapter for convenience
  , TimelineAdapter(..)
  , AdapterConfig(..)
  , AdapterState(..)
  , AdapterError(..)
  -- Ethereum-specific operations
  , EthereumConfig(..)
  , EthereumState(..)
  , submitEthTransaction
  , queryEthBalance
  , getLatestBlockInfo
  ) where
```

## Core Components

### Configuration

```haskell
data EthereumConfig = EthereumConfig
  { rpcUrl :: String                  -- Ethereum RPC URL
  , chainId :: Integer                -- Chain ID for the network
  , privateKeys :: [String]           -- Private keys for controlled accounts
  , gasLimit :: Maybe Integer         -- Optional gas limit override
  , gasPriceStrategy :: GasStrategy   -- Strategy for gas price determination
  , confirmationBlocks :: Int         -- Number of blocks for finality
  , pollInterval :: Int               -- Milliseconds between state polls
  }

data GasStrategy
  = StaticGasPrice Integer
  | DynamicGasPrice Double           -- Multiplier of current gas price (1.0 = current price)
  | EIP1559GasPrice                  -- Use EIP1559 pricing with auto-adjustment
```

### State Management

```haskell
data EthereumState = EthereumState
  { web3Provider :: Web3Provider     -- Connection to Ethereum node
  , managedAccounts :: [Address]     -- Accounts we control
  , pendingTxs :: Map TxHash PendingTx -- Transactions awaiting confirmation
  , nonces :: Map Address Integer    -- Track nonces for each account
  , recentBlocks :: [BlockInfo]      -- Cache of recent blocks
  , currentGasPrice :: Integer       -- Current network gas price
  }

data PendingTx = PendingTx
  { txHash :: TxHash
  , sentAt :: UTCTime
  , confirmedInBlock :: Maybe BlockNumber
  , retryCount :: Int
  , originalRequest :: TxRequest
  }
```

## Key Functions

1. **Adapter Creation**
   ```haskell
   createEthereumAdapter :: EthereumConfig -> IO TimelineAdapter
   ```

2. **Core TimelineAdapter Implementation**
   ```haskell
   -- Implement required TimelineAdapter methods:
   -- - getBalance
   -- - getTransactions
   -- - getLatestBlockHeight
   -- - submitTransaction
   -- - verifyTransaction
   -- - watchForTransaction
   ```

3. **Ethereum-specific Functions**
   ```haskell
   -- Functions unique to Ethereum
   submitEthTransaction :: EthereumState -> TransactionRequest -> IO (Either AdapterError TxHash)
   queryEthBalance :: EthereumState -> Address -> IO (Either AdapterError Integer)
   getLatestBlockInfo :: EthereumState -> IO (Either AdapterError BlockInfo)
   estimateGasCost :: EthereumState -> TransactionRequest -> IO (Either AdapterError Integer)
   ```

4. **State Management Functions**
   ```haskell
   updateNonce :: Address -> Integer -> EthereumState -> EthereumState
   trackPendingTransaction :: TxHash -> TxRequest -> EthereumState -> EthereumState
   confirmTransaction :: TxHash -> BlockNumber -> EthereumState -> EthereumState
   ```

## Dependencies

- `web3` - Haskell Ethereum library for RPC calls
- `cryptonite` - For cryptographic operations
- `text`, `bytestring` - For data handling
- Other Time Bandits core modules for integration

## Testing Strategy

1. **Unit Tests**
   - Mock RPC responses for deterministic testing
   - Test each component function in isolation

2. **Integration Tests**
   - Connect to Ethereum testnets (Sepolia, Goerli)
   - Perform real transactions with minimal values

3. **Simulation Tests**
   - Test within the Time Bandits simulation environment
   - Verify interaction with other components

## Implementation Steps

1. Set up the basic module structure
2. Implement the connection and configuration handling
3. Build core Ethereum interaction functions (queries, transactions)
4. Implement the TimelineAdapter typeclass methods
5. Add error handling and retry logic
6. Implement event monitoring and subscription features
7. Add comprehensive testing
8. Document usage patterns and examples
