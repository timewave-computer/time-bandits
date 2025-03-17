# Adding an ERC-4626 Vault Effect to Time Bandits

This document outlines how a third-party developer would implement an ERC-4626 compliant vault effect in Time Bandits. The effect will handle deposits of USDC in exchange for vault shares and redemptions of shares for USDC at the current exchange rate.

## Step 1: Ensure Required Timeline Adapters Exist

First, check if the required timeline adapter exists for the chain where the ERC-4626 vault is deployed:

```bash
# Check available timeline adapters
time-bandits adapters list
```

If the adapter doesn't exist, implement it following the Time Bandits adapter interface:

```haskell
module TimeBandits.Adapters.EthereumAdapter where

import TimeBandits.Adapters.TimelineAdapter

data EthereumAdapter = EthereumAdapter {
  rpcEndpoint :: Text,
  networkId :: Int
}

instance TimelineAdapter EthereumAdapter where
  applyEffect effect accountState timeline = 
    case effect of
      -- Handle basic effects
      Deposit params -> handleDeposit params accountState timeline
      Withdraw params -> handleWithdraw params accountState timeline
      
      -- Add handler for our custom vault effect
      CustomEffect "VaultDeposit" params -> handleVaultDeposit params accountState timeline
      CustomEffect "VaultWithdraw" params -> handleVaultWithdraw params accountState timeline
      
      -- Other effects
      _ -> defaultEffectHandler effect accountState timeline
      
  validateProof = defaultProofValidator
  observeFact = defaultFactObserver
```

## Step 2: Define the ERC-4626 Vault Effects in TEL

Define the vault deposit and withdraw effects:

```haskell
-- Vault Deposit Effect
type VaultDeposit = {
  timeline :: Timeline,
  vaultAddress :: Address,
  assetAddress :: Address,  -- The underlying asset (e.g., USDC)
  depositAmount :: Amount,
  minSharesOut :: Amount,   -- Slippage protection
  recipient :: Address      -- Recipient of the shares
}

-- Vault Withdrawal Effect
type VaultWithdraw = {
  timeline :: Timeline,
  vaultAddress :: Address,
  assetAddress :: Address,  -- The underlying asset (e.g., USDC)
  shareAmount :: Amount,    -- Amount of shares to redeem
  minAssetsOut :: Amount,   -- Slippage protection
  recipient :: Address      -- Recipient of the assets
}

-- Define result types
type VaultDepositResult = 
  | DepositSuccess { sharesReceived :: Amount, txHash :: Hash }
  | DepositFailed { reason :: Text }
  | SlippageExceeded { sharesOffered :: Amount, minSharesExpected :: Amount }

type VaultWithdrawResult = 
  | WithdrawSuccess { assetsReceived :: Amount, txHash :: Hash }
  | WithdrawFailed { reason :: Text }
  | SlippageExceeded { assetsOffered :: Amount, minAssetsExpected :: Amount }

-- Define the vault functions
vaultDeposit :: VaultDeposit -> Effect VaultDepositResult
vaultDeposit params = emit (CustomEffect "VaultDeposit" params)

vaultWithdraw :: VaultWithdraw -> Effect VaultWithdrawResult
vaultWithdraw params = emit (CustomEffect "VaultWithdraw" params)
```

## Step 3: Implement Fact Observation Rules

Create fact observation rules for vault events:

```toml
# Rule for observing deposit events
[[rules]]
rule_id = "vault-deposit-observation"
fact_type = "VaultDepositObservation"
proof = "InclusionProof"
enabled = true
description = "Observes deposits into ERC-4626 vaults"

path.source = "${timeline}"
path.selector = "contract.event"

[path.parameters]
contract_address = "${vaultAddress}"
event_name = "Deposit"

[[conditions]]
field = "sender"
operator = "=="
value = "${sender}"

[[conditions]]
field = "receiver"
operator = "=="
value = "${recipient}"

# Rule for observing withdrawal events
[[rules]]
rule_id = "vault-withdraw-observation"
fact_type = "VaultWithdrawObservation"
proof = "InclusionProof"
enabled = true
description = "Observes withdrawals from ERC-4626 vaults"

path.source = "${timeline}"
path.selector = "contract.event"

[path.parameters]
contract_address = "${vaultAddress}"
event_name = "Withdraw"

[[conditions]]
field = "sender"
operator = "=="
value = "${sender}"

[[conditions]]
field = "receiver"
operator = "=="
value = "${recipient}"

# Rule for observing exchange rate updates
[[rules]]
rule_id = "vault-exchange-rate-observation"
fact_type = "VaultExchangeRateObservation"
proof = "StateProof"
enabled = true
description = "Observes exchange rate changes in ERC-4626 vaults"

path.source = "${timeline}"
path.selector = "contract.call"

[path.parameters]
contract_address = "${vaultAddress}"
method = "convertToShares"
```

## Step 4: Implement the Effect Handlers

Next, implement the effect handlers in the adapter:

```haskell
-- Handler for vault deposit
handleVaultDeposit :: VaultDeposit -> AccountProgramState -> ExternalTimeline -> IO (Either AdapterError Receipt)
handleVaultDeposit params accountState timeline = do
  -- 1. Verify sufficient balance in account program
  when (insufficientBalance accountState params.assetAddress params.depositAmount) $
    return $ Left InsufficientBalance
  
  -- 2. Get current exchange rate from the vault
  exchangeRateResult <- callContract timeline params.vaultAddress "previewDeposit" [params.depositAmount]
  
  case exchangeRateResult of
    Left err -> return $ Left $ AdapterError $ "Failed to get exchange rate: " ++ show err
    Right expectedShares -> do
      -- 3. Check if expected shares meet slippage requirement
      if expectedShares < params.minSharesOut
        then return $ Left $ AdapterError $ "Slippage exceeded: expected " ++ show expectedShares ++ 
                                           " shares, minimum " ++ show params.minSharesOut
        else do
          -- 4. Build and submit the deposit transaction
          depositTx <- buildDepositTransaction params
          depositResult <- submitTransaction timeline depositTx
          
          case depositResult of
            Left err -> return $ Left $ AdapterError $ "Transaction failed: " ++ show err
            Right txHash -> do
              -- 5. Wait for the deposit event
              depositEvent <- waitForEvent timeline params.vaultAddress "Deposit" 
                [("receiver", params.recipient), ("assets", params.depositAmount)]
              
              case depositEvent of
                Left err -> return $ Left $ AdapterError $ "Failed to find deposit event: " ++ show err
                Right event -> do
                  -- 6. Extract actual shares received
                  let sharesReceived = extractShares event
                  
                  -- 7. Return success receipt
                  return $ Right $ Receipt
                    { effectType = "VaultDeposit"
                    , effectResult = DepositSuccess { sharesReceived = sharesReceived, txHash = txHash }
                    , proofs = [inclusionProof txHash]
                    }

-- Handler for vault withdrawal
handleVaultWithdraw :: VaultWithdraw -> AccountProgramState -> ExternalTimeline -> IO (Either AdapterError Receipt)
handleVaultWithdraw params accountState timeline = do
  -- 1. Verify sufficient share balance in account program
  when (insufficientBalance accountState (vaultShareAsset params.vaultAddress) params.shareAmount) $
    return $ Left InsufficientBalance
  
  -- 2. Get current exchange rate from the vault
  exchangeRateResult <- callContract timeline params.vaultAddress "previewRedeem" [params.shareAmount]
  
  case exchangeRateResult of
    Left err -> return $ Left $ AdapterError $ "Failed to get exchange rate: " ++ show err
    Right expectedAssets -> do
      -- 3. Check if expected assets meet slippage requirement
      if expectedAssets < params.minAssetsOut
        then return $ Left $ AdapterError $ "Slippage exceeded: expected " ++ show expectedAssets ++ 
                                           " assets, minimum " ++ show params.minAssetsOut
        else do
          -- 4. Build and submit the withdraw transaction
          withdrawTx <- buildWithdrawTransaction params
          withdrawResult <- submitTransaction timeline withdrawTx
          
          case withdrawResult of
            Left err -> return $ Left $ AdapterError $ "Transaction failed: " ++ show err
            Right txHash -> do
              -- 5. Wait for the withdraw event
              withdrawEvent <- waitForEvent timeline params.vaultAddress "Withdraw" 
                [("receiver", params.recipient), ("shares", params.shareAmount)]
              
              case withdrawEvent of
                Left err -> return $ Left $ AdapterError $ "Failed to find withdraw event: " ++ show err
                Right event -> do
                  -- 6. Extract actual assets received
                  let assetsReceived = extractAssets event
                  
                  -- 7. Return success receipt
                  return $ Right $ Receipt
                    { effectType = "VaultWithdraw"
                    , effectResult = WithdrawSuccess { assetsReceived = assetsReceived, txHash = txHash }
                    , proofs = [inclusionProof txHash]
                    }

-- Helper function to build asset identifier for vault shares
vaultShareAsset :: Address -> Asset
vaultShareAsset vaultAddress = "vault-share:" ++ vaultAddress
```

## Step 5: Create a Vault Module using TEL

Create a reusable TEL module for interacting with ERC-4626 vaults:

```haskell
module Erc4626Vault where

import TimeBandits.Core.TEL

-- Get the current total assets in the vault
getTotalAssets :: Timeline -> Address -> Effect Amount
getTotalAssets timeline vaultAddress = do
  -- Call the totalAssets function on the vault
  result <- observeContractCall timeline vaultAddress "totalAssets" []
  return $ parseAmount result

-- Get the current total supply of shares
getTotalSupply :: Timeline -> Address -> Effect Amount
getTotalSupply timeline vaultAddress = do
  -- Call the totalSupply function on the vault
  result <- observeContractCall timeline vaultAddress "totalSupply" []
  return $ parseAmount result

-- Get the current exchange rate (assets per share)
getExchangeRate :: Timeline -> Address -> Effect Decimal
getExchangeRate timeline vaultAddress = do
  -- Get total assets and supply
  totalAssets <- getTotalAssets timeline vaultAddress
  totalSupply <- getTotalSupply timeline vaultAddress
  
  -- Handle division by zero case
  if totalSupply == 0
    then return 1.0  -- Initial exchange rate
    else return $ totalAssets / totalSupply

-- Convert asset amount to share amount at current rate
convertToShares :: Timeline -> Address -> Amount -> Effect Amount
convertToShares timeline vaultAddress assetAmount = do
  -- Call the convertToShares function on the vault
  result <- observeContractCall timeline vaultAddress "convertToShares" [assetAmount]
  return $ parseAmount result

-- Convert share amount to asset amount at current rate
convertToAssets :: Timeline -> Address -> Amount -> Effect Amount
convertToAssets timeline vaultAddress shareAmount = do
  -- Call the convertToAssets function on the vault
  result <- observeContractCall timeline vaultAddress "convertToAssets" [shareAmount]
  return $ parseAmount result

-- Deposit assets and receive shares
depositToVault :: Timeline -> Address -> Address -> Amount -> Decimal -> Effect VaultDepositResult
depositToVault timeline vaultAddress assetAddress depositAmount slippageTolerance = do
  -- 1. Get expected shares from current exchange rate
  expectedShares <- convertToShares timeline vaultAddress depositAmount
  
  -- 2. Calculate minimum acceptable shares with slippage
  let minSharesOut = floor $ expectedShares * (1.0 - slippageTolerance)
  
  -- 3. Withdraw the assets from the account program
  withdrawResult <- withdraw depositAmount assetAddress timeline
  
  case withdrawResult of
    Left err -> return $ DepositFailed { reason = "Withdrawal failed: " ++ show err }
    Right _ -> do
      -- 4. Set up the vault deposit parameters
      let depositParams = VaultDeposit {
        timeline = timeline,
        vaultAddress = vaultAddress,
        assetAddress = assetAddress,
        depositAmount = depositAmount,
        minSharesOut = minSharesOut,
        recipient = getTravelerAddress()  -- Get the current traveler's address
      }
      
      -- 5. Execute the vault deposit effect
      depositResult <- vaultDeposit depositParams
      
      case depositResult of
        DepositSuccess sharesReceived txHash -> do
          -- 6. Deposit the shares to the account program
          deposit sharesReceived (vaultShareAsset vaultAddress) timeline
          return depositResult
        
        other -> return other

-- Withdraw assets by redeeming shares
withdrawFromVault :: Timeline -> Address -> Address -> Amount -> Decimal -> Effect VaultWithdrawResult
withdrawFromVault timeline vaultAddress assetAddress shareAmount slippageTolerance = do
  -- 1. Get expected assets from current exchange rate
  expectedAssets <- convertToAssets timeline vaultAddress shareAmount
  
  -- 2. Calculate minimum acceptable assets with slippage
  let minAssetsOut = floor $ expectedAssets * (1.0 - slippageTolerance)
  
  -- 3. Withdraw the shares from the account program
  withdrawResult <- withdraw shareAmount (vaultShareAsset vaultAddress) timeline
  
  case withdrawResult of
    Left err -> return $ WithdrawFailed { reason = "Withdrawal failed: " ++ show err }
    Right _ -> do
      -- 4. Set up the vault withdraw parameters
      let withdrawParams = VaultWithdraw {
        timeline = timeline,
        vaultAddress = vaultAddress,
        assetAddress = assetAddress,
        shareAmount = shareAmount,
        minAssetsOut = minAssetsOut,
        recipient = getTravelerAddress()  -- Get the current traveler's address
      }
      
      -- 5. Execute the vault withdraw effect
      withdrawResult <- vaultWithdraw withdrawParams
      
      case withdrawResult of
        WithdrawSuccess assetsReceived txHash -> do
          -- 6. Deposit the assets to the account program
          deposit assetsReceived assetAddress timeline
          return withdrawResult
        
        other -> return other

-- Helper to get the traveler's address
getTravelerAddress :: Effect Address
getTravelerAddress = do
  travelerInfo <- getAccountInfo
  return travelerInfo.address

-- Helper for vault share asset identifier
vaultShareAsset :: Address -> Asset
vaultShareAsset vaultAddress = "vault-share:" ++ show vaultAddress

-- Helper to parse amount from contract result
parseAmount :: ContractResult -> Amount
parseAmount result = read $ show result
```

## Step 6: Add Advanced Yield Strategies

Extend the module with advanced yield strategies:

```haskell
-- Compound yield by reinvesting strategy
compoundYield :: Timeline -> Address -> Address -> Amount -> Int -> Effect Amount
compoundYield timeline vaultAddress assetAddress initialAmount compoundPeriods = do
  -- 1. Initial deposit
  depositResult <- depositToVault timeline vaultAddress assetAddress initialAmount 0.01
  
  case depositResult of
    DepositSuccess initialShares _ -> do
      -- 2. Track shares over time
      finalShares <- foldM 
        (\currentShares period -> do
          -- Wait for the compound period to elapse
          wait (period * 86400) seconds  -- Compound every N days
          
          -- Get current value in assets
          currentAssetValue <- convertToAssets timeline vaultAddress currentShares
          
          -- Withdraw all shares
          withdrawResult <- withdrawFromVault timeline vaultAddress assetAddress currentShares 0.01
          
          case withdrawResult of
            WithdrawSuccess receivedAssets _ -> do
              -- Redeposit for compounding
              reDepositResult <- depositToVault timeline vaultAddress assetAddress receivedAssets 0.01
              
              case reDepositResult of
                DepositSuccess newShares _ -> return newShares
                _ -> return currentShares  -- On failure, keep current shares
                
            _ -> return currentShares  -- On failure, keep current shares
        )
        initialShares
        [1..compoundPeriods]
      
      -- 3. Convert final shares to assets
      finalAssetValue <- convertToAssets timeline vaultAddress finalShares
      return finalAssetValue
      
    _ -> return initialAmount  -- Return initial amount on deposit failure

-- Dollar-cost averaging strategy
dollarCostAverage :: Timeline -> Address -> Address -> Amount -> Int -> Effect Amount
dollarCostAverage timeline vaultAddress assetAddress totalAmount periods = do
  -- Calculate amount per period
  let amountPerPeriod = totalAmount / toInteger periods
  
  -- Track total shares
  totalShares <- foldM
    (\accumulatedShares period -> do
      -- Wait for next period
      wait (period * 86400) seconds  -- DCA every N days
      
      -- Deposit this period's amount
      depositResult <- depositToVault timeline vaultAddress assetAddress amountPerPeriod 0.01
      
      case depositResult of
        DepositSuccess newShares _ -> return $ accumulatedShares + newShares
        _ -> return accumulatedShares  -- On failure, keep current shares
    )
    0  -- Start with zero shares
    [1..periods]
  
  -- Convert final shares to assets
  finalAssetValue <- convertToAssets timeline vaultAddress totalShares
  return finalAssetValue
```

## Step 7: Register and Publish the Vault Module

Register the module in the content-addressable code system:

```haskell
import Core.CodeAddress

-- Store the vault module
storeAndRegisterVaultModule :: CodeRepository -> IO CodeHash
storeAndRegisterVaultModule repo = do
  let moduleName = "Erc4626Vault"
      moduleSource = "-- Full module source code here..."
  
  moduleHash <- hashModule moduleName [] moduleSource
  moduleDef <- CodeDefinition moduleHash moduleSource ModuleDef
  
  _ <- storeDefinition repo moduleDef
  registerName repo moduleName moduleHash
  
  putStrLn $ "Vault module published with hash: " ++ show moduleHash
  
  return moduleHash
```

## Step 8: Use the Vault in Programs

Other developers can now use the vault module:

```haskell
-- Import the vault module by name or content hash
import Erc4626Vault
-- Or import by content hash: import @a1b2c3d4... as Erc4626Vault

-- Example program using the vault
optimizeYield :: Amount -> Effect Amount
optimizeYield initialAmount = do
  -- Constants
  let usdcAddress = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"  -- USDC on Ethereum
      vaultAddress = "0x1234567890123456789012345678901234567890"  -- Example vault address
      timeline = "ethereum"
      investmentPeriod = 30  -- 30 days
  
  -- Get current exchange rate for tracking
  initialRate <- Erc4626Vault.getExchangeRate timeline vaultAddress
  
  -- Deposit into vault with 0.5% slippage tolerance
  depositResult <- Erc4626Vault.depositToVault timeline vaultAddress usdcAddress initialAmount 0.005
  
  case depositResult of
    DepositSuccess sharesReceived _ -> do
      -- Wait for investment period
      wait (investmentPeriod * 86400) seconds
      
      -- Get updated exchange rate
      finalRate <- Erc4626Vault.getExchangeRate timeline vaultAddress
      
      -- Calculate yield
      let yieldPercentage = ((finalRate / initialRate) - 1.0) * 100
      
      -- Log yield information
      emit $ "Earned " ++ show yieldPercentage ++ "% yield over " ++ 
              show investmentPeriod ++ " days"
      
      -- Withdraw all shares
      withdrawResult <- Erc4626Vault.withdrawFromVault 
                          timeline vaultAddress usdcAddress sharesReceived 0.005
      
      case withdrawResult of
        WithdrawSuccess assetsReceived _ -> do
          -- Return the final amount
          return assetsReceived
        
        _ -> do
          emit "Withdrawal failed"
          return 0
      
    _ -> do
      emit "Deposit failed"
      return 0
```

## Real-World Example: USDC Yield Vault

Here's a concrete example of using a real USDC vault:

```haskell
-- Define a specific USDC vault strategy
investInUSDCVault :: Amount -> Effect (Amount, Decimal)
investInUSDCVault amount = do
  -- Specific vault constants
  let usdcAddress = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"  -- USDC on Ethereum
      aaveUsdcVault = "0x3a91e48cd9c1c94a45f4982315393684127b2430"  -- Aave USDC vault
      timeline = "ethereum"
      investmentPeriod = 90  -- 90 days
      slippageTolerance = 0.003  -- 0.3%
  
  -- Get initial data
  initialTotalAssets <- Erc4626Vault.getTotalAssets timeline aaveUsdcVault
  initialTotalSupply <- Erc4626Vault.getTotalSupply timeline aaveUsdcVault
  
  -- Deposit USDC into vault
  depositResult <- Erc4626Vault.depositToVault timeline aaveUsdcVault usdcAddress amount slippageTolerance
  
  case depositResult of
    DepositSuccess sharesReceived _ -> do
      -- Wait for investment period
      wait (investmentPeriod * 86400) seconds
      
      -- Get final asset value
      finalAssetValue <- Erc4626Vault.convertToAssets timeline aaveUsdcVault sharesReceived
      
      -- Calculate APY
      let rawYield = finalAssetValue - amount
          yieldPercentage = (finalAssetValue / amount - 1.0) * 100
          annualizedYield = yieldPercentage * (365.0 / investmentPeriod)
      
      -- Log yield information
      emit $ "Investment results:"
      emit $ "- Initial investment: " ++ show amount ++ " USDC"
      emit $ "- Final value: " ++ show finalAssetValue ++ " USDC"
      emit $ "- Total yield: " ++ show rawYield ++ " USDC (" ++ show yieldPercentage ++ "%)"
      emit $ "- Estimated APY: " ++ show annualizedYield ++ "%"
      
      -- Withdraw everything
      _ <- Erc4626Vault.withdrawFromVault timeline aaveUsdcVault usdcAddress sharesReceived slippageTolerance
      
      return (finalAssetValue, annualizedYield)
      
    _ -> do
      emit "Deposit failed"
      return (amount, 0.0)
```

## How the ERC-4626 Vault Effect Works

The ERC-4626 vault implementation maintains all the key principles of Time Bandits:

1. **Asset Separation**: The vault shares are tracked as distinct assets within account programs
2. **Exchange Rate Awareness**: The implementation handles varying redemption rates at deposit and withdrawal
3. **Slippage Protection**: Minimum shares/assets parameters protect users from exchange rate manipulation
4. **Event-Based Tracking**: Fact observation rules capture deposit and withdraw events
5. **Yield Strategies**: Advanced strategies like compounding and DCA are built on the basic vault operations

This allows developers to safely interact with any ERC-4626 compliant vault, tracking both underlying assets and vault shares correctly within the Time Bandits system.

## Integration with Other Effects

The vault implementation can be combined with other effects like bridges for cross-chain yield strategies:

```haskell
-- Cross-chain yield farming strategy
crossChainYieldFarming :: Amount -> Effect Profit
crossChainYieldFarming amount = do
  -- Import modules
  import ExternalBridges
  import Erc4626Vault
  
  -- Define assets and vaults
  let ethereumUsdcAddress = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
      ethereumVaultAddress = "0x1234...5678"  -- Ethereum USDC vault
      celestiaUsdcAddress = "0xabcd...efgh"   -- USDC on Celestia
      celestiaVaultAddress = "0x9876...5432"  -- Celestia USDC vault
  
  -- Compare APYs
  ethereumApy <- getVaultAPY "ethereum" ethereumVaultAddress
  celestiaApy <- getVaultAPY "celestia" celestiaVaultAddress
  
  if celestiaApy > ethereumApy + 2.0 then  -- Only bridge if Celestia APY is at least 2% higher
    -- Bridge USDC to Celestia
    bridgeResult <- ExternalBridges.bridgeToken 
                     "USDC" "USDC" "ethereum" "celestia" amount
    
    case bridgeResult of
      BridgeSuccess _ -> do
        -- Deposit in Celestia vault
        depositResult <- Erc4626Vault.depositToVault 
                           "celestia" celestiaVaultAddress celestiaUsdcAddress amount 0.01
        -- Rest of the strategy...
        
      _ -> do
        -- Bridge failed, use Ethereum vault instead
        depositToEthereumVault amount
        
  else
    -- Use Ethereum vault directly
    depositToEthereumVault amount
  
  -- Helper function for Ethereum vault deposit
  where
    depositToEthereumVault amt = do
      Erc4626Vault.depositToVault 
        "ethereum" ethereumVaultAddress ethereumUsdcAddress amt 0.01
      -- Rest of the strategy...
```

By implementing the ERC-4626 vault effect, developers can interact with any compliant vault across supported blockchains, creating sophisticated yield strategies while maintaining the benefits of Time Bandits' time-aware, causally consistent programming model.