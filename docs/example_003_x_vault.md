# Example 003: Implementing Cross-Chain Vaults in Time Bandits

This document outlines how a developer would implement a cross-chain vault system using Time Bandits, inspired by the Valence cross-chain vault architecture. In this system, a vault contract on one chain (source chain) manages assets that are actually deployed to a different vault on another chain (destination chain) to generate yield.

## Architecture Overview

The cross-chain vault system consists of:

1. **Source Chain Vault Contract**: Deployed on the source chain (e.g., Ethereum), this contract:
   - Accepts user deposits in a specific token (e.g., USDC)
   - Issues vault shares to depositors
   - Tracks the total assets under management
   - Communicates with the Time Bandits system for cross-chain operations

2. **Destination Chain Vault**: An externally-owned vault on the destination chain (e.g., Celestia) that:
   - Receives assets from the source chain via bridges
   - Generates yield from these assets
   - Can return assets to the source chain when withdrawals are requested

3. **Time Bandits Program**: A program that:
   - Observes deposit/withdrawal events on the source chain
   - Executes cross-chain transfers between source and destination vaults
   - Monitors yield generation on the destination chain
   - Updates the source chain vault with current value information

## Step 1: Define the Smart Contract Architecture

First, let's outline the required smart contracts and their interfaces.

### Source Chain Vault (Ethereum)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "@openzeppelin/contracts/token/ERC20/extensions/ERC4626.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

contract CrossChainVault is ERC4626, Ownable {
    // Address of the Time Bandits bridge controller
    address public timeBanditsController;
    
    // Information about the destination vault
    string public destinationChain;
    string public destinationVault;
    
    // Tracking values
    uint256 public lastKnownTotalAssetsRemote;
    uint256 public lastUpdateTimestamp;
    
    // Events for Time Bandits to monitor
    event DepositForBridge(address indexed sender, uint256 assets);
    event WithdrawRequestForBridge(address indexed receiver, uint256 assets);
    event RemoteAssetsUpdated(uint256 newTotalAssets, uint256 timestamp);
    
    constructor(
        IERC20 _asset,
        string memory _name,
        string memory _symbol,
        address _controller,
        string memory _destChain,
        string memory _destVault
    ) ERC4626(_asset) ERC20(_name, _symbol) Ownable(msg.sender) {
        timeBanditsController = _controller;
        destinationChain = _destChain;
        destinationVault = _destVault;
    }
    
    // Override totalAssets to include remote assets
    function totalAssets() public view override returns (uint256) {
        return lastKnownTotalAssetsRemote;
    }
    
    // Called by Time Bandits to update the total assets value
    function updateRemoteAssets(uint256 _newTotalAssets) external onlyController {
        lastKnownTotalAssetsRemote = _newTotalAssets;
        lastUpdateTimestamp = block.timestamp;
        emit RemoteAssetsUpdated(_newTotalAssets, block.timestamp);
    }
    
    // Override deposit to emit our bridge event
    function deposit(uint256 assets, address receiver) public override returns (uint256) {
        uint256 shares = super.deposit(assets, receiver);
        emit DepositForBridge(receiver, assets);
        return shares;
    }
    
    // Override withdraw to emit our bridge event
    function withdraw(uint256 assets, address receiver, address owner) public override returns (uint256) {
        uint256 shares = super.withdraw(assets, receiver, owner);
        emit WithdrawRequestForBridge(receiver, assets);
        return shares;
    }
    
    // Modifier to restrict calls to the Time Bandits controller
    modifier onlyController() {
        require(msg.sender == timeBanditsController, "Only controller can call");
        _;
    }
}
```

## Step 2: Define Time Bandits Effects and Fact Observation Rules

### Cross-Chain Vault Effects

```haskell
-- Cross-chain vault deposit effect
type CrossChainVaultDeposit = {
  sourceTimeline :: Timeline,
  destTimeline :: Timeline,
  sourceVaultAddress :: Address,
  destVaultAddress :: Address,
  assetAddress :: Address,  -- The underlying asset (e.g., USDC)
  amount :: Amount,
  bridgeParams :: ExternalBridge  -- Uses the external bridge effect from previous examples
}

-- Cross-chain vault withdrawal effect
type CrossChainVaultWithdraw = {
  sourceTimeline :: Timeline,
  destTimeline :: Timeline,
  sourceVaultAddress :: Address,
  destVaultAddress :: Address,
  assetAddress :: Address,
  amount :: Amount,
  recipient :: Address,
  bridgeParams :: ExternalBridge
}

-- Value update effect
type CrossChainVaultUpdate = {
  sourceTimeline :: Timeline,
  destTimeline :: Timeline,
  sourceVaultAddress :: Address,
  destVaultAddress :: Address,
  newTotalAssets :: Amount
}

-- Result types
type CrossChainVaultResult = 
  | Success { txHash :: Hash }
  | Failed { reason :: Text }
  | Partial { status :: Text, details :: Text }
```

### Fact Observation Rules

```toml
# Rule for observing source vault deposits
[[rules]]
rule_id = "cross-chain-vault-deposit-observation"
fact_type = "VaultDepositForBridgeObservation"
proof = "InclusionProof"
enabled = true
description = "Observes deposits in source vault that need bridging"

path.source = "${sourceTimeline}"
path.selector = "contract.event"

[path.parameters]
contract_address = "${sourceVaultAddress}"
event_name = "DepositForBridge"

# Rule for observing source vault withdrawal requests
[[rules]]
rule_id = "cross-chain-vault-withdraw-observation"
fact_type = "VaultWithdrawRequestObservation"
proof = "InclusionProof"
enabled = true
description = "Observes withdrawal requests in source vault that need cross-chain handling"

path.source = "${sourceTimeline}"
path.selector = "contract.event"

[path.parameters]
contract_address = "${sourceVaultAddress}"
event_name = "WithdrawRequestForBridge"

# Rule for observing destination vault value changes
[[rules]]
rule_id = "destination-vault-value-observation"
fact_type = "DestVaultValueObservation"
proof = "StateProof"
enabled = true
description = "Observes value changes in the destination vault"

path.source = "${destTimeline}"
path.selector = "contract.call"

[path.parameters]
contract_address = "${destVaultAddress}"
method = "totalAssets"
```

## Step 3: Implement the Cross-Chain Vault Program in TEL

```haskell
module CrossChainVault where

import TimeBandits.Core.TEL
import ExternalBridges  -- For bridge functionality
import Erc4626Vault     -- For vault interactions

-- Function to initialize a new cross-chain vault monitoring program
initCrossChainVaultMonitor :: 
  Timeline ->           -- Source timeline (e.g., Ethereum)
  Timeline ->           -- Destination timeline (e.g., Celestia)
  Address ->            -- Source vault address
  Address ->            -- Destination vault address
  Address ->            -- Asset address (e.g., USDC)
  Int ->                -- Update interval in seconds
  Effect ()
initCrossChainVaultMonitor sourceChain destChain sourceVault destVault assetAddress updateInterval = do
  -- Start the background processes
  fork $ processDeposits sourceChain destChain sourceVault destVault assetAddress
  fork $ processWithdrawals sourceChain destChain sourceVault destVault assetAddress
  fork $ updateValuePeriodically sourceChain destChain sourceVault destVault updateInterval
  
  emit $ "Cross-chain vault monitor initialized for " ++ 
          show sourceVault ++ " on " ++ sourceChain ++ " -> " ++
          show destVault ++ " on " ++ destChain

-- Process deposits from source chain to destination chain
processDeposits :: Timeline -> Timeline -> Address -> Address -> Address -> Effect ()
processDeposits sourceChain destChain sourceVault destVault assetAddress = do
  -- Watch for deposit events
  while True $ do
    depositFact <- observe (VaultDepositForBridgeObservation sourceChain sourceVault) on sourceChain
    
    -- Extract deposit parameters
    let depositor = extractDepositor depositFact
        amount = extractAmount depositFact
        
    emit $ "Observed deposit of " ++ show amount ++ " from " ++ show depositor
    
    -- Determine which bridge to use
    bridgeParams <- getBridgeParameters sourceChain destChain assetAddress
    
    -- Create the cross-chain vault deposit effect
    let depositParams = CrossChainVaultDeposit {
      sourceTimeline = sourceChain,
      destTimeline = destChain,
      sourceVaultAddress = sourceVault,
      destVaultAddress = destVault,
      assetAddress = assetAddress,
      amount = amount,
      bridgeParams = bridgeParams
    }
    
    -- Execute the deposit process
    result <- executeDepositProcess depositParams
    
    case result of
      Success txHash -> 
        emit $ "Successfully deposited to destination vault: " ++ show txHash
      Failed reason -> 
        emit $ "Failed to deposit to destination vault: " ++ reason
      Partial status details ->
        emit $ "Partial deposit status: " ++ status ++ " - " ++ details

-- Execute the full deposit process
executeDepositProcess :: CrossChainVaultDeposit -> Effect CrossChainVaultResult
executeDepositProcess params = do
  -- Step 1: Obtain the tokens from the source vault
  tokenResult <- getTokensFromSourceVault params.sourceTimeline params.sourceVaultAddress params.amount
  
  case tokenResult of
    Left err -> return $ Failed $ "Failed to get tokens: " ++ err
    Right _ -> do
      -- Step 2: Bridge the tokens to the destination chain
      bridgeResult <- ExternalBridges.bridgeToken 
                        params.assetAddress params.assetAddress 
                        params.sourceTimeline params.destTimeline 
                        params.amount
      
      case bridgeResult of
        BridgeSuccess txHash -> do
          -- Step 3: Deposit to the destination vault
          destDepositResult <- Erc4626Vault.depositToVault 
                                params.destTimeline params.destVaultAddress 
                                params.assetAddress params.amount 0.01
          
          case destDepositResult of
            DepositSuccess _ finalTxHash -> do
              -- Step 4: Update the source vault with new total assets value
              _ <- updateSourceVaultValue params.sourceTimeline params.destTimeline 
                    params.sourceVaultAddress params.destVaultAddress
              
              return $ Success finalTxHash
              
            _ -> return $ Partial "BridgeSuccess" "Destination deposit failed"
          
        BridgeRefunded refundTxHash -> do
          -- Handle refund case - return tokens to source vault
          _ <- returnTokensToSourceVault params.sourceTimeline 
                params.sourceVaultAddress params.assetAddress params.amount
          
          return $ Failed $ "Bridge was refunded: " ++ show refundTxHash
          
        _ -> return $ Failed "Bridge failed"

-- Process withdrawals from destination chain back to source chain
processWithdrawals :: Timeline -> Timeline -> Address -> Address -> Address -> Effect ()
processWithdrawals sourceChain destChain sourceVault destVault assetAddress = do
  -- Watch for withdrawal request events
  while True $ do
    withdrawFact <- observe (VaultWithdrawRequestObservation sourceChain sourceVault) on sourceChain
    
    -- Extract withdrawal parameters
    let receiver = extractReceiver withdrawFact
        amount = extractAmount withdrawFact
        
    emit $ "Observed withdrawal request of " ++ show amount ++ " for " ++ show receiver
    
    -- Determine which bridge to use
    bridgeParams <- getBridgeParameters destChain sourceChain assetAddress
    
    -- Create the cross-chain vault withdrawal effect
    let withdrawParams = CrossChainVaultWithdraw {
      sourceTimeline = sourceChain,
      destTimeline = destChain,
      sourceVaultAddress = sourceVault,
      destVaultAddress = destVault,
      assetAddress = assetAddress,
      amount = amount,
      recipient = receiver,
      bridgeParams = bridgeParams
    }
    
    -- Execute the withdrawal process
    result <- executeWithdrawProcess withdrawParams
    
    case result of
      Success txHash -> 
        emit $ "Successfully processed withdrawal: " ++ show txHash
      Failed reason -> 
        emit $ "Failed to process withdrawal: " ++ reason
      Partial status details ->
        emit $ "Partial withdrawal status: " ++ status ++ " - " ++ details

-- Execute the full withdrawal process
executeWithdrawProcess :: CrossChainVaultWithdraw -> Effect CrossChainVaultResult
executeWithdrawProcess params = do
  -- Step 1: Convert assets to shares in the destination vault
  sharesToWithdraw <- Erc4626Vault.convertToShares 
                       params.destTimeline params.destVaultAddress params.amount
  
  -- Step 2: Withdraw from the destination vault
  withdrawResult <- Erc4626Vault.withdrawFromVault
                     params.destTimeline params.destVaultAddress 
                     params.assetAddress sharesToWithdraw 0.01
  
  case withdrawResult of
    WithdrawSuccess receivedAssets _ -> do
      -- Step 3: Bridge the assets back to the source chain
      bridgeResult <- ExternalBridges.bridgeToken
                        params.assetAddress params.assetAddress
                        params.destTimeline params.sourceTimeline
                        receivedAssets
      
      case bridgeResult of
        BridgeSuccess txHash -> do
          -- Step 4: Transfer assets to the recipient on source chain
          transferResult <- transferToRecipient 
                             params.sourceTimeline params.assetAddress 
                             params.recipient receivedAssets
          
          case transferResult of
            Right finalTxHash -> do
              -- Step 5: Update the source vault with new total assets value
              _ <- updateSourceVaultValue params.sourceTimeline params.destTimeline
                    params.sourceVaultAddress params.destVaultAddress
              
              return $ Success finalTxHash
              
            Left err -> return $ Partial "BridgeSuccess" $ "Transfer failed: " ++ err
          
        _ -> return $ Failed "Bridge back to source chain failed"
      
    _ -> return $ Failed "Withdrawal from destination vault failed"

-- Periodically update the source vault with the current value from destination
updateValuePeriodically :: Timeline -> Timeline -> Address -> Address -> Int -> Effect ()
updateValuePeriodically sourceChain destChain sourceVault destVault updateInterval = do
  while True $ do
    -- Wait for the update interval
    wait updateInterval seconds
    
    -- Update the value
    result <- updateSourceVaultValue sourceChain destChain sourceVault destVault
    
    case result of
      Right txHash -> 
        emit $ "Updated source vault value successfully: " ++ show txHash
      Left err -> 
        emit $ "Failed to update source vault value: " ++ err
    
    -- Continue the loop

-- Update the source vault with the current value from destination
updateSourceVaultValue :: Timeline -> Timeline -> Address -> Address -> Effect (Either Text Hash)
updateSourceVaultValue sourceChain destChain sourceVault destVault = do
  -- Step 1: Get current total assets in destination vault
  totalAssetsResult <- try $ Erc4626Vault.getTotalAssets destChain destVault
  
  case totalAssetsResult of
    Left err -> return $ Left $ "Failed to get destination assets: " ++ err
    Right totalAssets -> do
      -- Step 2: Update the source vault contract
      updateResult <- try $ callContract sourceChain sourceVault "updateRemoteAssets" [totalAssets]
      
      case updateResult of
        Left err -> return $ Left $ "Failed to update source vault: " ++ err
        Right txHash -> do
          emit $ "Updated source vault total assets to " ++ show totalAssets
          return $ Right txHash

-- Helper functions (implementation details omitted for brevity)
getTokensFromSourceVault :: Timeline -> Address -> Amount -> Effect (Either Text ())
returnTokensToSourceVault :: Timeline -> Address -> Address -> Amount -> Effect (Either Text ())
transferToRecipient :: Timeline -> Address -> Address -> Amount -> Effect (Either Text Hash)
getBridgeParameters :: Timeline -> Timeline -> Address -> Effect ExternalBridge
extractDepositor :: Fact -> Address
extractReceiver :: Fact -> Address
extractAmount :: Fact -> Amount
```

## Step 4: Value Calculation and Yield Tracking

Enhance the program with value calculation and yield tracking capabilities:

```haskell
-- Track the performance of the cross-chain vault
trackVaultPerformance :: 
  Timeline ->           -- Source timeline 
  Timeline ->           -- Destination timeline
  Address ->            -- Source vault address
  Address ->            -- Destination vault address
  Int ->                -- Tracking interval in seconds
  Effect ()
trackVaultPerformance sourceChain destChain sourceVault destVault trackingInterval = do
  -- Initialize tracking data
  initialTotalAssets <- Erc4626Vault.getTotalAssets destChain destVault
  initialTimestamp <- getCurrentTimestamp
  
  -- Create initial checkpoint
  let initialCheckpoint = VaultCheckpoint {
    timestamp = initialTimestamp,
    totalAssets = initialTotalAssets,
    sharePrice = await calculateSharePrice()
  }
  
  -- Store the checkpoint
  storeCheckpoint sourceVault initialCheckpoint
  
  -- Start performance tracking loop
  while True $ do
    -- Wait for tracking interval
    wait trackingInterval seconds
    
    -- Get current values
    currentTotalAssets <- Erc4626Vault.getTotalAssets destChain destVault
    currentTimestamp <- getCurrentTimestamp
    currentSharePrice <- calculateSharePrice()
    
    -- Create new checkpoint
    let newCheckpoint = VaultCheckpoint {
      timestamp = currentTimestamp,
      totalAssets = currentTotalAssets,
      sharePrice = currentSharePrice
    }
    
    -- Store the checkpoint
    storeCheckpoint sourceVault newCheckpoint
    
    -- Calculate performance metrics
    lastCheckpoint <- getLastCheckpoint sourceVault
    dailyYield <- calculateDailyYield lastCheckpoint newCheckpoint
    annualizedYield <- calculateAnnualizedYield lastCheckpoint newCheckpoint
    
    -- Log performance
    emit $ "Cross-chain vault performance:"
    emit $ "- Total Assets: " ++ show currentTotalAssets
    emit $ "- Share Price: " ++ show currentSharePrice
    emit $ "- Daily Yield: " ++ show dailyYield ++ "%"
    emit $ "- Annualized Yield: " ++ show annualizedYield ++ "%"
    
    -- Update the source vault
    _ <- updateSourceVaultValue sourceChain destChain sourceVault destVault

-- Calculate the current share price
calculateSharePrice :: Effect Decimal
calculateSharePrice = do
  -- Implementation would get total assets and total supply, then divide
  totalAssets <- getTotalAssets()
  totalSupply <- getTotalSupply()
  return $ if totalSupply == 0 then 1.0 else totalAssets / totalSupply

-- Calculate yield between checkpoints
calculateDailyYield :: VaultCheckpoint -> VaultCheckpoint -> Effect Decimal
calculateDailyYield oldCp newCp = do
  -- Calculate percentage change in share price
  let priceDiff = newCp.sharePrice - oldCp.sharePrice
      priceChange = priceDiff / oldCp.sharePrice
      
      -- Calculate time difference in days
      timeDiffSeconds = newCp.timestamp - oldCp.timestamp
      timeDiffDays = timeDiffSeconds / (24 * 3600)
      
      -- Calculate daily yield rate
      dailyYield = (priceChange / timeDiffDays) * 100
      
  return dailyYield

-- Calculate annualized yield between checkpoints
calculateAnnualizedYield :: VaultCheckpoint -> VaultCheckpoint -> Effect Decimal
calculateAnnualizedYield oldCp newCp = do
  dailyYield <- calculateDailyYield oldCp newCp
  return $ dailyYield * 365
```

## Step 5: Use Case - Cross-Chain USDC Yield Strategy

Here's a concrete example of a USDC yield strategy using vaults across Ethereum and Celestia:

```haskell
-- Deploy and initialize a cross-chain USDC yield strategy
deployUsdcYieldStrategy :: Amount -> Effect ()
deployUsdcYieldStrategy initialDeposit = do
  -- Constants
  let ethereumChain = "ethereum"
      celestiaChain = "celestia"
      usdcAddress = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"  -- USDC on Ethereum
      sourceVaultAddress = "0x1234...5678"  -- Our source vault on Ethereum
      destVaultAddress = "0xabcd...efgh"    -- High-yield vault on Celestia
      updateInterval = 3600                 -- Update value hourly
      trackingInterval = 86400              -- Track performance daily
  
  -- Initialize the cross-chain vault monitor
  initCrossChainVaultMonitor 
    ethereumChain celestiaChain sourceVaultAddress destVaultAddress usdcAddress updateInterval
  
  -- Start performance tracking
  fork $ trackVaultPerformance 
    ethereumChain celestiaChain sourceVaultAddress destVaultAddress trackingInterval
  
  -- Make initial deposit if provided
  when (initialDeposit > 0) $ do
    emit $ "Making initial deposit of " ++ show initialDeposit ++ " USDC"
    
    -- Approve the source vault to spend USDC
    _ <- approve ethereumChain usdcAddress sourceVaultAddress initialDeposit
    
    -- Deposit to the source vault
    depositResult <- callContract ethereumChain sourceVaultAddress "deposit" [initialDeposit, getAddress()]
    
    case depositResult of
      Left err -> 
        emit $ "Initial deposit failed: " ++ err
      Right txHash -> 
        emit $ "Initial deposit successful: " ++ show txHash
  
  emit "Cross-chain USDC yield strategy deployed and initialized"
```

## Step 6: Risk Management and Monitoring

Add risk management and monitoring capabilities:

```haskell
-- Monitor cross-chain vault for risks and anomalies
monitorVaultRisks :: 
  Timeline ->           -- Source timeline 
  Timeline ->           -- Destination timeline
  Address ->            -- Source vault address
  Address ->            -- Destination vault address
  Decimal ->            -- Maximum slippage allowed (as percentage)
  Decimal ->            -- Maximum value deviation allowed (as percentage)
  Effect ()
monitorVaultRisks sourceChain destChain sourceVault destVault maxSlippage maxDeviation = do
  -- Start monitoring loops
  fork $ monitorSlippage sourceChain destChain sourceVault destVault maxSlippage
  fork $ monitorValueDeviation sourceChain destChain sourceVault destVault maxDeviation
  fork $ monitorBridgeHealth sourceChain destChain
  
  emit "Risk monitoring initialized"

-- Monitor for excessive slippage
monitorSlippage :: Timeline -> Timeline -> Address -> Address -> Decimal -> Effect ()
monitorSlippage sourceChain destChain sourceVault destVault maxSlippage = do
  while True $ do
    -- Check source chain price
    sourcePrice <- Erc4626Vault.getExchangeRate sourceChain sourceVault
    
    -- Check destination chain price 
    destPrice <- Erc4626Vault.getExchangeRate destChain destVault
    
    -- Calculate slippage
    let expectedPrice = sourcePrice  -- Ideally these should be very close
        actualSlippage = abs((destPrice - expectedPrice) / expectedPrice) * 100
    
    -- Alert if slippage exceeds threshold
    when (actualSlippage > maxSlippage) $ do
      emit $ "ALERT: Excessive slippage detected!"
      emit $ "Source price: " ++ show sourcePrice
      emit $ "Destination price: " ++ show destPrice
      emit $ "Slippage: " ++ show actualSlippage ++ "% (threshold: " ++ show maxSlippage ++ "%)"
      
      -- Notify administrators
      notifyAdministrators "Excessive slippage detected in cross-chain vault"
    
    -- Check every hour
    wait 3600 seconds

-- Monitor for unexpected value deviations
monitorValueDeviation :: Timeline -> Timeline -> Address -> Address -> Decimal -> Effect ()
monitorValueDeviation sourceChain destChain sourceVault destVault maxDeviation = do
  while True $ do
    -- Get source vault's view of total assets
    sourceTotalAssets <- callContract sourceChain sourceVault "totalAssets" []
    
    -- Get actual destination vault total assets
    destTotalAssets <- Erc4626Vault.getTotalAssets destChain destVault
    
    -- Calculate deviation
    let deviation = abs((destTotalAssets - sourceTotalAssets) / sourceTotalAssets) * 100
    
    -- Alert if deviation exceeds threshold
    when (deviation > maxDeviation) $ do
      emit $ "ALERT: Value deviation detected!"
      emit $ "Source vault reports: " ++ show sourceTotalAssets
      emit $ "Actual destination value: " ++ show destTotalAssets
      emit $ "Deviation: " ++ show deviation ++ "% (threshold: " ++ show maxDeviation ++ "%)"
      
      -- Update the source vault
      _ <- updateSourceVaultValue sourceChain destChain sourceVault destVault
      
      -- Notify administrators
      notifyAdministrators "Value deviation detected in cross-chain vault"
    
    -- Check every 4 hours
    wait 14400 seconds

-- Monitor bridge health
monitorBridgeHealth :: Timeline -> Timeline -> Effect ()
monitorBridgeHealth sourceChain destChain = do
  while True $ do
    -- Perform a small test bridge transaction
    testResult <- testBridgeHealth sourceChain destChain
    
    case testResult of
      BridgeHealthy stats -> 
        emit $ "Bridge health check passed: " ++ stats
      
      BridgeIssue description -> do
        emit $ "ALERT: Bridge health issue detected: " ++ description
        notifyAdministrators $ "Bridge health issue: " ++ description
    
    -- Check daily
    wait 86400 seconds
```
