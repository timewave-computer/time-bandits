# ADR 015: Blockchain Adapter

## Status

Proposed

## Context

Time Bandits needs to interact with various blockchain networks to observe and modify state across multiple chains. These interactions are fundamentally different from peer-to-peer communication handled by the Network Adapter (ADR-014), as they involve external systems with their own consensus mechanisms, transaction formats, and state models.

Currently, our system implements blockchain interactions in an ad-hoc manner, which makes it difficult to:
1. Add support for new blockchains
2. Maintain consistent error handling and retry logic
3. Test blockchain interactions in isolation
4. Mock blockchain interactions for development and testing

We need a standardized approach for blockchain interactions that provides a consistent interface while accommodating the unique characteristics of different blockchains.

Key challenges that must be addressed include:
1. Supporting diverse blockchain architectures (UTXO, account-based, etc.)
2. Handling different transaction formats and fee models
3. Managing private keys and signing securely
4. Providing consistent error handling and retry strategies
5. Supporting both synchronous and asynchronous operations
6. Monitoring transaction status and confirmations
7. Efficiently querying blockchain state
8. Maintaining compatibility with evolving blockchain protocols

## Decision

We will implement a Blockchain Adapter system that provides a standardized interface for all blockchain interactions in the Time Bandits system. The adapter will abstract away the complexities of specific blockchain implementations while providing a consistent API for the rest of the system.

The Blockchain Adapter will follow a similar architectural pattern to the Network Adapter (ADR-014) to ensure consistency across the codebase, but the two systems will be loosely coupled to maintain separation of concerns.

### Core Components

#### Configuration

Each blockchain adapter will be configured according to its specific requirements:

```haskell
data BlockchainAdapterConfig = BlockchainAdapterConfig
  { chainType :: ChainType          -- Type of blockchain (Ethereum, Bitcoin, etc.)
  , networkType :: NetworkType      -- Mainnet, testnet, etc.
  , rpcEndpoints :: [String]        -- List of RPC endpoints to use (with fallbacks)
  , connectionTimeout :: Int        -- Connection timeout in milliseconds
  , maxRetries :: Int               -- Maximum number of retries for failed operations
  , retryDelay :: Int               -- Delay between retries in milliseconds
  , requiredConfirmations :: Int    -- Number of confirmations required for finality
  , credentials :: Maybe Credentials -- Optional credentials for authentication
  , keystorePath :: Maybe FilePath  -- Path to keystore for signing transactions
  }

data ChainType 
  = Ethereum
  | Bitcoin
  | Celestia
  | Solana
  | Cosmos
  | Custom String
  deriving (Show, Eq)

data NetworkType 
  = Mainnet
  | Testnet String
  | LocalNetwork
  deriving (Show, Eq)
```

#### State Management

The adapter will maintain state about blockchain connections and operations:

```haskell
data BlockchainState = BlockchainState
  { activeConnections :: Map ChainType Connection  -- Active connections to blockchains
  , pendingTransactions :: Map TxHash TxInfo       -- Transactions waiting for confirmation
  , chainStats :: Map ChainType ChainStats         -- Statistics about blockchain activity
  , asyncJobs :: [Async ()]                       -- Background jobs for monitoring
  }

data TxInfo = TxInfo
  { txHash :: TxHash                    -- Transaction hash
  , txChain :: ChainType                -- Chain the transaction was submitted to
  , txTimestamp :: UTCTime              -- Time the transaction was submitted
  , txStatus :: TxStatus                -- Current status of the transaction
  , txConfirmations :: Int              -- Number of confirmations received
  , txData :: ByteString                -- Raw transaction data
  , txMetadata :: Map String String     -- Additional metadata
  }

data TxStatus
  = Pending
  | Confirmed Int  -- Number of confirmations
  | Failed String  -- Error message
  | Dropped        -- Dropped from mempool
  deriving (Show, Eq)
```

#### Transaction Model

A standardized transaction model will provide a consistent interface across different blockchains:

```haskell
data Transaction = Transaction
  { txType :: TxType                    -- Type of transaction
  , txChain :: ChainType                -- Target blockchain
  , txSender :: Address                 -- Sender address
  , txRecipient :: Maybe Address        -- Optional recipient address
  , txValue :: Maybe TokenAmount        -- Optional value to transfer
  , txData :: Maybe ByteString          -- Optional transaction data
  , txGasLimit :: Maybe Integer         -- Optional gas limit (for applicable chains)
  , txGasPrice :: Maybe Integer         -- Optional gas price (for applicable chains)
  , txNonce :: Maybe Integer            -- Optional nonce (for applicable chains)
  , txSignature :: Maybe Signature      -- Optional signature
  }

data TxType
  = Transfer                   -- Simple value transfer
  | ContractCall               -- Call to a smart contract
  | ContractDeployment         -- Deploy a smart contract
  | DataSubmission             -- Submit data to the blockchain
  | Custom String              -- Custom transaction type
  deriving (Show, Eq)
```

### Core API

The Blockchain Adapter will expose the following core functions:

1. **Adapter Creation and Management**
   ```haskell
   createBlockchainAdapter :: BlockchainAdapterConfig -> IO BlockchainAdapter
   
   -- Get the current state of a blockchain adapter
   getAdapterState :: BlockchainAdapter -> IO BlockchainState
   ```

2. **Transaction Operations**
   ```haskell
   -- Create a new unsigned transaction
   createTransaction :: BlockchainAdapter -> TxType -> Address -> Maybe Address -> Maybe TokenAmount -> Maybe ByteString -> IO Transaction
   
   -- Sign a transaction with the provided key
   signTransaction :: BlockchainAdapter -> Transaction -> PrivateKey -> IO Transaction
   
   -- Submit a signed transaction to the blockchain
   submitTransaction :: BlockchainAdapter -> Transaction -> IO (Either BlockchainError TxHash)
   
   -- Wait for a transaction to be confirmed
   waitForConfirmation :: BlockchainAdapter -> TxHash -> Int -> IO (Either BlockchainError TxInfo)
   
   -- Get the status of a transaction
   getTransactionStatus :: BlockchainAdapter -> TxHash -> IO (Either BlockchainError TxStatus)
   ```

3. **State Queries**
   ```haskell
   -- Get the balance of an address
   getBalance :: BlockchainAdapter -> Address -> Maybe TokenAddress -> IO (Either BlockchainError TokenAmount)
   
   -- Call a read-only contract method
   callContract :: BlockchainAdapter -> Address -> ByteString -> IO (Either BlockchainError ByteString)
   
   -- Get the current block number
   getBlockNumber :: BlockchainAdapter -> IO (Either BlockchainError BlockNumber)
   
   -- Get block information
   getBlock :: BlockchainAdapter -> BlockNumber -> IO (Either BlockchainError Block)
   ```

4. **Event Monitoring**
   ```haskell
   -- Subscribe to blockchain events
   subscribeToEvents :: BlockchainAdapter -> EventFilter -> (Event -> IO ()) -> IO Subscription
   
   -- Unsubscribe from blockchain events
   unsubscribeFromEvents :: BlockchainAdapter -> Subscription -> IO ()
   ```

### Integration with Network Adapter

While the Blockchain Adapter and Network Adapter (ADR-014) are designed to be loosely coupled, they will share some common interfaces to ensure consistency:

1. **Common Error Types**: Both adapters will use a common base error type to ensure consistent error handling.

2. **Message Passing**: The Network Adapter can be used to distribute blockchain transactions and events to other Time Bandits nodes.

3. **Resource Management**: Both adapters will follow similar patterns for resource allocation and cleanup.

4. **Configuration Management**: Similar configuration management patterns will be used for both adapters.

The loose coupling will be maintained through:

1. **No Direct Dependencies**: The Blockchain Adapter will not directly depend on the Network Adapter or vice versa.

2. **Message-Based Communication**: Any communication between the two systems will be through well-defined message formats.

3. **Interface Consistency**: Common interfaces will be defined in a shared module to ensure consistency without creating tight coupling.

### Security Measures

The Blockchain Adapter will implement several security features:

1. **Secure Key Management**: Private keys will be securely stored and accessed only when needed for signing.

2. **Transaction Validation**: All transactions will be validated before submission to prevent errors.

3. **Connection Security**: Secure connections will be used for all blockchain interactions.

4. **Rate Limiting**: Requests will be rate-limited to prevent abuse of blockchain nodes.

5. **Error Handling**: Comprehensive error handling will prevent security issues from failed operations.

## Consequences

### Positive

1. **Simplified Blockchain Interactions**: The rest of the system can use a consistent, high-level API for blockchain operations without worrying about the underlying details.

2. **Enhanced Maintainability**: Adding support for new blockchains becomes easier with a standardized adapter interface.

3. **Better Testing**: Blockchain interactions can be mocked for testing without modifying application code.

4. **Improved Reliability**: Consistent error handling and retry logic improves the reliability of blockchain operations.

5. **Separation of Concerns**: Clear separation between peer-to-peer communication (Network Adapter) and blockchain interactions (Blockchain Adapter).

### Negative

1. **Additional Complexity**: Introducing another layer adds some complexity to the system.

2. **Performance Overhead**: The abstraction may introduce some overhead compared to direct blockchain interactions.

3. **API Limitations**: The standardized interface may not expose all features of specific blockchains, requiring custom extensions.

### Neutral

1. **Dependencies**: The implementation will require various blockchain-specific libraries and tools.

2. **Evolving Standards**: Blockchain technologies are constantly evolving, requiring ongoing maintenance of the adapter implementations.

## Implementation Plan

1. Define the core interfaces and data types
2. Implement the Ethereum adapter as the first reference implementation
3. Add support for Celestia as a second implementation to validate the abstraction
4. Implement transaction signing and submission
5. Add state query functionality
6. Implement event monitoring
7. Add comprehensive testing with mocks
8. Create documentation and examples
9. Implement additional blockchain adapters as needed 