# Adding an ERC-4626 Vault Effect to Time Bandits

This document outlines how a third-party developer would implement an ERC-4626 compliant vault effect in Time Bandits. The effect will handle deposits of USDC in exchange for vault shares and redemptions of shares for USDC at the current exchange rate, utilizing the formalized resource model.

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
import TimeBandits.Resources.Formalized
import TimeBandits.Controllers.Interface

data EthereumAdapter = EthereumAdapter {
  rpcEndpoint :: Text,
  networkId :: Int,
  controllerType :: ControllerType  -- Safe, Live, or Byzantine
}

instance Controller EthereumAdapter where
  getControllerID adapter = ControllerID $ "ethereum:" <> show (adapter.networkId)
  getControllerType = controllerType
  createControllerLabel adapter = ControllerLabel {
    creatingController = getControllerID adapter,
    terminalController = getControllerID adapter,
    affectingControllers = [getControllerID adapter],
    backupControllers = []
  }

instance TimelineAdapter EthereumAdapter where
  applyEffect effect accountState timeline = 
    case effect of
      -- Handle basic effects with formalized resources
      Deposit params -> handleDeposit params accountState timeline
      Withdraw params -> handleWithdraw params accountState timeline
      
      -- Add handler for our custom vault effect with resource formalization
      CustomEffect "VaultDeposit" params -> handleVaultDeposit params accountState timeline
      CustomEffect "VaultWithdraw" params -> handleVaultWithdraw params accountState timeline
      
      -- Other effects
      _ -> defaultEffectHandler effect accountState timeline
      
  validateProof = defaultProofValidator
  observeFact = defaultFactObserver
  
  -- Resource validation
  validateResourceConservation inputs outputs = do
    -- Group resources by fungibility domain
    let inputSum = sumResourcesByDomain inputs
        outputSum = sumResourcesByDomain outputs
    
    -- Verify conservation for each domain
    validateSums inputSum outputSum
  
  -- Dual validation for cross-chain operations
  validateCrossChainOperation source target resource timeMap controllerLabel = do
    -- Temporal validation
    temporalResult <- validateTemporalConsistency source target timeMap
    
    -- Ancestral validation
    ancestralResult <- validateControllerPath 
      controllerLabel.creatingController
      controllerLabel.terminalController
      controllerLabel.affectingControllers
    
    pure ValidationResult {
      temporal = temporalResult,
      ancestral = ancestralResult
    }
```

## Step 2: Define the ERC-4626 Vault Effects in TEL

Define the vault deposit and withdraw effects using formalized resources:

```haskell
-- Define formalized resources for vault tokens
resource USDCToken(quantity: Decimal) {
  resourceLogic = TokenLogic
  fungibilityDomain = "USDC"
  ephemeral = false
  metadata = { 
    "decimals" = 6,
    "chainId" = 1  -- Ethereum mainnet
  }
}

resource VaultShare(quantity: Decimal) {
  resourceLogic = TokenLogic
  fungibilityDomain = "VaultShare"
  ephemeral = false
  metadata = {
    "vaultAddress" = "0xabcdef...",
    "decimals" = 18
  }
}

-- Vault Deposit Effect
type VaultDeposit = {
  timeline :: Timeline,
  vaultAddress :: Address,
  assetAddress :: Address,  -- The underlying asset (e.g., USDC)
  resource :: Resource,     -- Formalized resource representation
  minSharesOut :: Amount,   -- Slippage protection
  recipient :: Address,     -- Recipient of the shares
  controllerLabel :: ControllerLabel  -- Track resource provenance
}

-- Vault Withdrawal Effect
type VaultWithdraw = {
  timeline :: Timeline,
  vaultAddress :: Address,
  assetAddress :: Address,  -- The underlying asset (e.g., USDC)
  resource :: Resource,     -- Formalized resource representation of shares
  minAssetsOut :: Amount,   -- Slippage protection
  recipient :: Address,     -- Recipient of the assets
  controllerLabel :: ControllerLabel  -- Track resource provenance
}

-- Define result types
type VaultDepositResult = 
  | DepositSuccess { sharesReceived :: Resource, txHash :: Hash, commitments :: [Commitment] }
  | DepositFailed { reason :: Text }
  | SlippageExceeded { sharesOffered :: Resource, minSharesExpected :: Amount }
  | ValidationFailed { error :: ValidationError }

type VaultWithdrawResult = 
  | WithdrawSuccess { assetsReceived :: Resource, txHash :: Hash, commitments :: [Commitment] }
  | WithdrawFailed { reason :: Text }
  | SlippageExceeded { assetsOffered :: Resource, minAssetsExpected :: Amount }
  | ValidationFailed { error :: ValidationError }

-- Define the vault functions with resource conservation
vaultDeposit :: VaultDeposit -> Effect VaultDepositResult
vaultDeposit params = do
  -- Get current controller
  controller <- getController params.timeline
  
  -- Create appropriate resource commitments
  let inputCommitment = hashCommitment params.resource
  
  -- Track resource delta for validation
  recordResourceDelta (Delta (-params.resource.quantity)) "USDC"
  
  -- Emit the effect with all required resource info
  result <- emit (CustomEffect "VaultDeposit" params)
  
  -- Add shares to resources with positive delta
  case result of
    DepositSuccess res -> do
      recordResourceDelta (Delta res.sharesReceived.quantity) "VaultShare"
      pure result
    _ -> pure result

vaultWithdraw :: VaultWithdraw -> Effect VaultWithdrawResult
vaultWithdraw params = do
  -- Create nullifier to prevent double-withdrawal
  let nullifier = hashNullifier params.resource.nullifierPubKey params.resource
  
  -- Record nullifier to mark resource as consumed
  recordNullifier nullifier
  
  -- Track resource delta for validation
  recordResourceDelta (Delta (-params.resource.quantity)) "VaultShare"
  
  -- Emit the effect
  result <- emit (CustomEffect "VaultWithdraw" params)
  
  -- Add returned assets to resources with positive delta
  case result of
    WithdrawSuccess res -> do
      recordResourceDelta (Delta res.assetsReceived.quantity) "USDC"
      pure result
    _ -> pure result
```

## Step 3: Implement Fact Observation Rules

Create fact observation rules for vault events with resource tracking:

```toml
# Rule for observing deposit events
[[rules]]
rule_id = "vault-deposit-observation"
fact_type = "VaultDepositObservation"
proof = "InclusionProof"
enabled = true
description = "Observes deposits into ERC-4626 vaults"

[rules.extract_pattern]
contract_address = "{{ params.vaultAddress }}"
event_signature = "Deposit(address,address,uint256,uint256)"
indexed_topics = ["*", "{{ params.recipient }}"]

[rules.extracted_fields]
vault_address = "topic[0]"
depositor = "topic[1]"
asset_amount = "data[0]"
shares_received = "data[1]"
resource_commitment = "{{ hashCommitment(resource) }}"
controller_label = "{{ params.controllerLabel }}"

# Rule for observing withdrawal events
[[rules]]
rule_id = "vault-withdraw-observation"
fact_type = "VaultWithdrawObservation"
proof = "InclusionProof"
enabled = true
description = "Observes withdrawals from ERC-4626 vaults"

[rules.extract_pattern]
contract_address = "{{ params.vaultAddress }}"
event_signature = "Withdraw(address,address,address,uint256,uint256)"
indexed_topics = ["*", "{{ params.recipient }}"]

[rules.extracted_fields]
vault_address = "topic[0]"
withdrawer = "topic[1]" 
receiver = "topic[2]"
asset_amount = "data[0]"
shares_burned = "data[1]"
resource_nullifier = "{{ hashNullifier(resource.nullifierPubKey, resource) }}"
controller_label = "{{ params.controllerLabel }}"
```

## Step 4: Implement the Effect Handler

Implement the custom effect handler for vault operations with resource validation:

```haskell
-- Handle vault deposit
handleVaultDeposit :: VaultDepositParams -> AccountState -> Timeline -> IO (Either Error VaultDepositResult)
handleVaultDeposit params accountState timeline = do
  -- Validate the input resource
  validResource <- validateResource params.resource
  unless validResource $ 
    return $ Left $ ValidationError "Invalid resource format"
  
  -- Create appropriate controller label
  controllerLabel <- createControllerLabel timeline

  -- Create transaction to interact with the ERC-4626 vault
  let calldata = encodeABI "deposit(uint256,address)" [
                  params.resource.quantity,
                  params.recipient
                ]
                
  -- Execute transaction
  txResult <- executeTransaction Transaction {
    to = params.vaultAddress,
    data = calldata,
    value = 0,
    from = params.recipient
  }
  
  case txResult of
    Left err -> 
      return $ Left $ ExecutionError err
      
    Right txHash -> do
      -- Wait for transaction to be mined and observed
      receipt <- waitForTransaction txHash
      
      -- Extract events to find shares received
      depositEvent <- findEvent receipt "Deposit(address,address,uint256,uint256)"
      
      case depositEvent of
        Just event -> do
          let sharesReceived = extractUint256 event 3
          
          -- Check for slippage
          if sharesReceived < params.minSharesOut
            then return $ Right $ SlippageExceeded {
              sharesOffered = calculateVaultShares params.resource.quantity params.vaultAddress,
              minSharesExpected = params.minSharesOut
            }
            else do
              -- Create formalized resource for shares
              let shareResource = Resource {
                resourceLogic = TokenLogic,
                fungibilityDomain = "VaultShare:" <> toText params.vaultAddress,
                quantity = sharesReceived,
                metadata = encodeMetadata params.vaultAddress,
                ephemeral = False,
                nonce = generateNonce,
                nullifierPubKey = deriveNullifierKey params.recipient,
                randomnessSeed = generateSeed
              }
              
              -- Calculate resource commitment
              let commitment = hashCommitment shareResource
              
              -- Return success with resource info
              return $ Right $ DepositSuccess {
                sharesReceived = shareResource,
                txHash = txHash,
                commitments = [commitment]
              }
              
        Nothing ->
          return $ Left $ ObservationError "No deposit event found"

-- Handle vault withdrawal
handleVaultWithdraw :: VaultWithdrawParams -> AccountState -> Timeline -> IO (Either Error VaultWithdrawResult)
handleVaultWithdraw params accountState timeline = do
  -- Validate the input resource (shares)
  validResource <- validateResource params.resource
  unless validResource $ 
    return $ Left $ ValidationError "Invalid resource format"
  
  -- Verify nullifier to prevent double-spending
  nullifierExists <- checkNullifier (hashNullifier params.resource.nullifierPubKey params.resource)
  when nullifierExists $
    return $ Left $ ValidationError "Resource already consumed"

  -- Create transaction to interact with the ERC-4626 vault
  let calldata = encodeABI "redeem(uint256,address,address)" [
                  params.resource.quantity,
                  params.recipient,
                  params.recipient
                ]
                
  -- Execute transaction
  txResult <- executeTransaction Transaction {
    to = params.vaultAddress,
    data = calldata,
    value = 0,
    from = params.recipient
  }
  
  case txResult of
    Left err -> 
      return $ Left $ ExecutionError err
      
    Right txHash -> do
      -- Wait for transaction to be mined and observed
      receipt <- waitForTransaction txHash
      
      -- Extract events to find assets received
      withdrawEvent <- findEvent receipt "Withdraw(address,address,address,uint256,uint256)"
      
      case withdrawEvent of
        Just event -> do
          let assetsReceived = extractUint256 event 3
          
          -- Check for slippage
          if assetsReceived < params.minAssetsOut
            then return $ Right $ SlippageExceeded {
              assetsOffered = calculateVaultAssets params.resource.quantity params.vaultAddress,
              minAssetsExpected = params.minAssetsOut
            }
            else do
              -- Create formalized resource for the returned assets
              let assetResource = Resource {
                resourceLogic = TokenLogic,
                fungibilityDomain = "USDC",
                quantity = assetsReceived,
                metadata = encodeMetadata params.assetAddress,
                ephemeral = False,
                nonce = generateNonce,
                nullifierPubKey = deriveNullifierKey params.recipient,
                randomnessSeed = generateSeed
              }
              
              -- Calculate resource commitment
              let commitment = hashCommitment assetResource
              
              -- Record nullifier for the shares consumed
              recordNullifier (hashNullifier params.resource.nullifierPubKey params.resource)
              
              -- Return success with resource info
              return $ Right $ WithdrawSuccess {
                assetsReceived = assetResource,
                txHash = txHash,
                commitments = [commitment]
              }
              
        Nothing ->
          return $ Left $ ObservationError "No withdraw event found"
```

## Step 5: Create Time Bandits Program

Create a program that uses the vault effect:

```haskell
module VaultProgram where

import TimeBandits.Core.TEL
import TimeBandits.Resources.Formalized
import TimeBandits.Controllers.Interface

-- Program to deposit USDC into a vault
depositToVault :: Amount -> Address -> Address -> Address -> Effect ()
depositToVault amount assetAddress vaultAddress recipient = do
  -- Create formalized resource for USDC
  let usdcResource = createResource TokenLogic "USDC" amount (encodeMetadata assetAddress) recipient
  
  -- Get controller for Ethereum
  controller <- getController "ethereum"
  let controllerLabel = controller.createControllerLabel
  
  -- Execute vault deposit
  result <- vaultDeposit VaultDeposit {
    timeline = "ethereum",
    vaultAddress = vaultAddress,
    assetAddress = assetAddress,
    resource = usdcResource,
    minSharesOut = estimateMinShares amount vaultAddress,
    recipient = recipient,
    controllerLabel = controllerLabel
  }
  
  case result of
    DepositSuccess res -> do
      -- Log success with resource info
      logInfo $ "Deposited " <> show amount <> " USDC, received " <> 
                show res.sharesReceived.quantity <> " vault shares"
      -- Store commitment for future reference
      storeCommitment res.commitments
      
    SlippageExceeded details ->
      logError $ "Slippage exceeded: offered " <> show details.sharesOffered.quantity <> 
                ", but minimum expected was " <> show details.minSharesExpected
                
    ValidationFailed err ->
      logError $ "Resource validation failed: " <> show err
      
    DepositFailed reason ->
      logError $ "Deposit failed: " <> reason

-- Program to withdraw from vault
withdrawFromVault :: Amount -> Address -> Address -> Address -> Effect ()
withdrawFromVault shareAmount assetAddress vaultAddress recipient = do
  -- Create formalized resource for shares
  let sharesResource = createResource TokenLogic 
                       ("VaultShare:" <> toText vaultAddress) 
                       shareAmount 
                       (encodeMetadata vaultAddress) 
                       recipient
  
  -- Get controller for Ethereum
  controller <- getController "ethereum"
  let controllerLabel = controller.createControllerLabel
  
  -- Execute vault withdrawal
  result <- vaultWithdraw VaultWithdraw {
    timeline = "ethereum",
    vaultAddress = vaultAddress,
    assetAddress = assetAddress,
    resource = sharesResource,
    minAssetsOut = estimateMinAssets shareAmount vaultAddress,
    recipient = recipient,
    controllerLabel = controllerLabel
  }
  
  case result of
    WithdrawSuccess res -> do
      -- Log success with resource info
      logInfo $ "Withdrawn " <> show shareAmount <> " vault shares, received " <> 
                show res.assetsReceived.quantity <> " USDC"
      -- Store commitment for future reference
      storeCommitment res.commitments
      
    SlippageExceeded details ->
      logError $ "Slippage exceeded: offered " <> show details.assetsOffered.quantity <> 
                ", but minimum expected was " <> show details.minAssetsExpected
                
    ValidationFailed err ->
      logError $ "Resource validation failed: " <> show err
      
    WithdrawFailed reason ->
      logError $ "Withdrawal failed: " <> reason

-- Helper function to create resource
createResource :: ResourceLogic -> Text -> Quantity -> Value -> Address -> Resource
createResource logic domain quantity metadata owner = Resource {
  resourceLogic = logic,
  fungibilityDomain = domain,
  quantity = quantity,
  metadata = metadata,
  ephemeral = False,
  nonce = generateNonce,
  nullifierPubKey = deriveNullifierKey owner,
  randomnessSeed = generateSeed
}

-- Utility function to estimate minimum shares expected
estimateMinShares :: Amount -> Address -> Amount
estimateMinShares amount vaultAddress = do
  -- Get current exchange rate from vault
  rate <- getVaultExchangeRate vaultAddress
  -- Calculate expected shares with 2% slippage tolerance
  floor $ (fromIntegral amount / rate) * 0.98

-- Utility function to estimate minimum assets expected
estimateMinAssets :: Amount -> Address -> Amount
estimateMinAssets shareAmount vaultAddress = do
  -- Get current exchange rate from vault
  rate <- getVaultExchangeRate vaultAddress
  -- Calculate expected assets with 2% slippage tolerance
  floor $ (fromIntegral shareAmount * rate) * 0.98
```

## Using the Vault Implementation

A time traveler can now interact with the vault using the program:

```haskell
main :: Effect ()
main = do
  -- Set up parameters
  let usdcAddress = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"  -- USDC on Ethereum
      vaultAddress = "0xabcdef1234567890..."  -- Example vault address
      userAddress = "0x123456..."  -- User's address
  
  -- Deposit to vault
  depositToVault 1000_000000 usdcAddress vaultAddress userAddress  -- 1000 USDC with 6 decimals
  
  -- After some time...
  
  -- Withdraw from vault
  withdrawFromVault 950_000000000000000000 usdcAddress vaultAddress userAddress  -- 950 shares with 18 decimals
  
  -- Ensure resource conservation
  validateResourceConservation
```

This example demonstrates how to implement an ERC-4626 vault effect using the formalized resource model, with proper resource tracking, controller labels, and validation mechanisms. The implementation ensures that resources are properly created, consumed, and validated throughout the process, with strong guarantees about conservation and security.