# Example 006: Cross-chain Messaging with Async Convergence

This document outlines the implementation of a two-phase commit pattern spanning multiple chains, with registers serving as coordination points.

## Register Architecture for Cross-Chain Communication

```haskell
-- Model of registers on multiple chains
data CrossChainRegister = CrossChainRegister
  { registerId :: RegisterID
  , chainId :: ChainID  -- Specifies the chain where this register exists
  , contents :: RegisterContents
  , messageQueue :: Queue Message  -- Queue for cross-chain messages
  , bridgeAdapter :: BridgeAdapter  -- Manages cross-chain communication
  , verificationKeys :: [VerificationKey]
  , controllerLabel :: ControllerLabel  -- Added per ADR_018
  }

-- Bridge adapter for chain-specific bridge operations
data BridgeAdapter = BridgeAdapter
  { bridgeContract :: Address  -- Address of the bridge contract
  , sendMessage :: Message -> IO (Either BridgeError TxHash)
  , receiveMessage :: TxHash -> IO (Either BridgeError Message)
  , verifyMessage :: Message -> Proof -> Bool
  }

-- Structure for cross-chain messages
data Message = Message
  { messageId :: MessageID
  , sourceChain :: ChainID
  , targetChain :: ChainID
  , sourceRegister :: RegisterID
  , targetRegister :: RegisterID
  , payload :: MessagePayload
  , signature :: Signature
  , nonce :: Nonce
  , timeMapSnapshot :: TimeMap  -- Added for temporal validation
  , controllerLabels :: Map ResourceID ControllerLabel  -- Added for ancestral validation
  }

-- Payload variants for different message types
data MessagePayload
  = TokenTransfer TokenType Amount
  | DataPacket ByteString
  | TokenWithData TokenType Amount ByteString
```

## Formalized Resource Definition

As per ADR_018, we define the resources that can be transferred across chains:

```haskell
-- Formalized resource definition
data Resource = Resource
  { resourceLogic      :: Logic         -- Predicate controlling resource consumption
  , fungibilityDomain  :: Label         -- Determines equivalence classes
  , quantity           :: Quantity      -- Numerical representation of amount
  , metadata           :: Value         -- Associated resource data
  , ephemeral          :: Bool          -- Whether existence must be verified
  , nonce              :: Nonce         -- Uniqueness identifier
  , nullifierPubKey    :: NullifierPK   -- For verifying consumption
  , randomnessSeed     :: Seed          -- For deriving randomness
  }

-- Resource commitment (hash-based proof of existence)
commitment :: Resource -> Commitment
commitment r = hashCommitment r

-- Resource nullifier (unique value marking consumption)
nullifier :: NullifierKey -> Resource -> Nullifier  
nullifier nk r = hashNullifier nk r

-- Controller label for cross-chain resources
data ControllerLabel = ControllerLabel
  { creatingController  :: ControllerID
  , terminalController  :: ControllerID
  , affectingControllers :: [ControllerID]  -- DAG of controllers that affected the resource
  , backupControllers   :: [ControllerID]   -- In case terminal fails
  }
```

## Execution Flow Implementation

```haskell
-- Chain A: Initiating a cross-chain transfer
sendTokensWithData :: RegisterID -> TokenType -> Amount -> ByteString -> ChainID -> RegisterID -> IO (Either SendError MessageID)
sendTokensWithData sourceReg tokenType amount data targetChain targetReg = do
  -- 1. Retrieve the source register
  register <- getRegister sourceReg
  
  -- 2. Verify authorization (ZK proof, token ownership, etc.)
  authorized <- checkAuthorization register
  unless authorized $ throwError Unauthorized
  
  -- 3. Create formalized resource for transfer
  let resource = Resource
        { resourceLogic = TokenLogic
        , fungibilityDomain = tokenType
        , quantity = amount
        , metadata = encodeToValue data
        , ephemeral = False
        , nonce = generateNonce
        , nullifierPubKey = register.nullifierKey
        , randomnessSeed = generateSeed
        }
  
  -- 4. Calculate resource commitment and nullifier
  let resourceCommitment = commitment resource
  let resourceNullifier = nullifier register.nullifierKey resource
  
  -- 3. Create cross-chain message
  let payload = TokenWithData tokenType amount data
  
  -- 4. Create controller label for the resource
  let controllerLabel = ControllerLabel
        { creatingController = register.chainId
        , terminalController = targetChain
        , affectingControllers = [register.chainId]
        , backupControllers = []
        }
  
  -- 5. Get current time map for temporal validation
  timeMap <- getCurrentTimeMap
  
  -- 6. Create complete message with resource information
  let message = Message
        { messageId = generateMessageId
        , sourceChain = register.chainId
        , targetChain = targetChain
        , sourceRegister = sourceReg
        , targetRegister = targetReg
        , payload = payload
        , signature = sign register.verificationKeys message
        , nonce = generateNonce
        , timeMapSnapshot = timeMap
        , controllerLabels = Map.singleton resourceCommitment controllerLabel
        }
  
  -- 7. Transmit via bridge adapter
  result <- register.bridgeAdapter.sendMessage message
  
  -- 8. Create nullifier to mark resource as consumed
  recordNullifier resourceNullifier
  
  -- 9. Update register state to reflect pending outbound transfer with delta tracking
  -- Delta of -amount to reflect outgoing resource
  updateRegisterState sourceReg (LockTokens tokenType amount) (Delta (-amount))
  
  -- 10. Return message ID for tracking
  pure $ Right message.messageId

-- Chain B: Processing received message
receiveTokensWithData :: MessageID -> Proof -> IO (Either ReceiveError RegisterID)
receiveTokensWithData messageId bridgeProof = do
  -- 1. Verify the cross-chain message using the bridge proof
  message <- verifyBridgeMessage messageId bridgeProof
  
  -- 2. Retrieve the target register
  register <- getRegister message.targetRegister
  
  -- 3. Perform dual validation (temporal and ancestral)
  validationResult <- performDualValidation message register
  
  case validationResult of
    Left validationError -> throwError (ValidationFailed validationError)
    Right _ -> do
      -- 4. Extract resource information
      let (resourceCommitment, controllerLabel) = case Map.toList message.controllerLabels of
            [(commitment, label)] -> (commitment, label)
            _ -> throwError InvalidResourceMapping
      
      -- 5. Extract payload data
      (tokenType, amount, data) <- case message.payload of
        TokenWithData t a d -> pure (t, a, d)
        _ -> throwError InvalidPayload
      
      -- 6. Create new resource on this chain
      let resource = Resource
            { resourceLogic = TokenLogic
            , fungibilityDomain = tokenType
            , quantity = amount
            , metadata = encodeToValue data
            , ephemeral = False
            , nonce = generateNonce
            , nullifierPubKey = register.nullifierKey
            , randomnessSeed = generateSeed
            }
      
      -- 7. Update controller label to reflect receipt
      let updatedLabel = controllerLabel
            { terminalController = register.chainId
            , affectingControllers = register.chainId : controllerLabel.affectingControllers
            }
      
      -- 8. Update register with received tokens and data (delta of +amount)
      updateRegisterContents register.registerId resource updatedLabel (Delta amount)
      
      -- 9. Generate ZK proof confirming message receipt and processing
      receiptProof <- generateReceiptProof register message
      
      -- 10. Send receipt back to chain A (optional)
      sendReceipt message.sourceChain message.sourceRegister messageId receiptProof
      
      -- 11. Return register ID for further processing
      pure $ Right register.registerId

-- Perform dual validation of cross-chain message
performDualValidation :: Message -> CrossChainRegister -> IO (Either ValidationError ())
performDualValidation message register = do
  -- 1. Temporal validation - check time map consistency
  temporalResult <- validateTemporalConsistency message.timeMapSnapshot message.sourceChain
  
  -- 2. Ancestral validation - check controller history
  let resourceLabels = message.controllerLabels
  ancestralResult <- validateControllerAncestry register.chainId resourceLabels
  
  -- 3. Both validations must pass
  case (temporalResult, ancestralResult) of
    (Right _, Right _) -> pure $ Right ()
    (Left temporalError, _) -> pure $ Left temporalError
    (_, Left ancestralError) -> pure $ Left ancestralError

-- Chain B: Secondary process utilizing the received tokens and data
processReceivedTokens :: RegisterID -> TokenType -> Address -> IO (Either ProcessError TxHash)
processReceivedTokens registerId moneyToken userAddress = do
  -- 1. Retrieve the register with TIME tokens and data
  register <- getRegister registerId
  
  -- 2. Retrieve the user's MONEY token register
  moneyRegister <- findUserTokenRegister userAddress moneyToken
  
  -- 3. Extract tokens and data from the received register
  (timeToken, amount, data) <- extractTokensAndData register
  moneyAmount <- getTokenBalance moneyRegister moneyToken
  
  -- 4. Evaluate if the data meets required conditions
  let condition = evaluateCondition data
  
  -- 5. If condition is met, create an execution sequence
  if condition then do
    -- Define nodes for the execution graph
    let nodes = Map.fromList
          [ (NodeID "read_time", ExecutionNode
              { nodeId = NodeID "read_time"
              , nodeType = ActionNode
              , operation = readRegister registerId
              , registerDependencies = [registerId]
              , completionProof = Nothing
              , metadata = Map.empty
              })
          , (NodeID "read_money", ExecutionNode
              { nodeId = NodeID "read_money"
              , nodeType = ActionNode
              , operation = readRegister moneyRegister
              , registerDependencies = [moneyRegister]
              , completionProof = Nothing
              , metadata = Map.empty
              })
          , (NodeID "combine_tokens", ExecutionNode
              { nodeId = NodeID "combine_tokens" 
              , nodeType = ActionNode
              , operation = combineTokens registerId moneyRegister
              , registerDependencies = [registerId, moneyRegister]
              , completionProof = Nothing
              , metadata = Map.empty
              })
          , (NodeID "execute_action", ExecutionNode
              { nodeId = NodeID "execute_action"
              , nodeType = ActionNode
              , operation = executeBasedOnData data
              , registerDependencies = [registerId, moneyRegister]
              , completionProof = Nothing
              , metadata = Map.empty
              })
          ]
          
    -- Define edges between nodes
    let edges =
          [ Edge (NodeID "read_time") (NodeID "combine_tokens") Nothing 0
          , Edge (NodeID "read_money") (NodeID "combine_tokens") Nothing 0
          , Edge (NodeID "combine_tokens") (NodeID "execute_action") Nothing 0
          ]
          
    -- Create and execute the sequence
    sequenceId <- createExecutionSequence (Map.elems nodes) edges
    executionResult <- executeSequence sequenceId
    
    -- Ensure resource conservation across the entire execution
    validateResourceConservation sequenceId
    
    pure $ Right executionResult
  else
    pure $ Left ConditionNotMet
```

## Process Flow Analysis

The implementation follows this sequence:

1. On Chain A, a smart contract initiates a cross-chain transfer containing both TIME tokens and a data bundle. The register on Chain A locks those tokens and records an "in-flight" state.

2. The message traverses the bridge, which implements its own security and validation mechanisms.

3. On Chain B, the message arrives and undergoes dual validation:
   - Temporal validation ensures causal consistency using time maps
   - Ancestral validation verifies the controller history of the resource

4. The target register updates to reflect the received tokens and data.

5. Subsequently, a secondary process on Chain B combines these TIME tokens with MONEY tokens and executes logic conditional on the data bundle.

6. An execution sequence is created that:
   - Reads both registers (TIME and MONEY)
   - Combines the tokens
   - Executes conditional logic based on the data
   - Validates resource conservation (delta = 0)

## Key Architectural Elements

Several important aspects of this design warrant emphasis:

1. **Register Continuity**: The register functions as a persistent storage mechanism across chains. It maintains custody of assets until they are explicitly utilized in subsequent operations.

2. **Encapsulated Authorization**: Authorization logic is encapsulated at each processing stage. Cross-chain messages utilize their own authorization (bridge proofs), while subsequent operations leverage register-based authorization.

3. **Execution Sequences**: Execution sequences model complex dependencies between registers, ensuring operations execute in the correct order.

4. **Formalized Resources**: Resources are defined using the formal model from ADR_018, providing strong guarantees about conservation and integrity.

5. **Dual Validation**: All cross-chain operations undergo both temporal and ancestral validation to ensure integrity.

6. **Controller Labels**: Resources track their controller history explicitly, making trust assumptions visible.

This model effectively addresses challenges typically encountered in traditional cross-chain applications. Rather than implementing fragile state synchronization between separate systems, the register abstraction creates a unified view across chains.

## Error Handling and Recovery

Implementing robust error handling is essential for cross-chain operations:

```haskell
-- Timeout and recovery mechanism for unresolved cross-chain messages
recoverStuckMessage :: MessageID -> IO (Either RecoveryError RegisterID)
recoverStuckMessage messageId = do
  -- Verify if message exceeds timeout threshold
  messageStatus <- getMessageStatus messageId
  currentHeight <- getCurrentBlockHeight
  
  if currentHeight - messageStatus.lastUpdate > timeoutThreshold then
    -- Generate a timeout proof
    timeoutProof <- generateTimeoutProof messageId currentHeight
    
    -- Generate controller status verification
    controllerStatus <- verifyControllerStatus messageStatus.controllerLabels
    
    -- If controller is classified as Byzantine or halted, use backup controller
    backupController <- case controllerStatus of
      ControllerHalted label -> findBackupController label.backupControllers
      ControllerByzantine label -> findBackupController label.backupControllers
      ControllerOperational _ -> throwError NoBackupNeeded
    
    -- Unlock original tokens on source chain with appropriate delta
    unlockSourceTokens messageId timeoutProof (Delta (messageStatus.amount))
  else
    throwError MessageNotTimedOut
```

It is important to note that the bridge itself presents the primary challenge in this architecture. The cross-chain messaging layer typically operates as a separate system with its own trust assumptions and security model. Our register system interacts with this layer but does not replace it.

## Program Account Interface for Time Travelers

Program accounts remain the optimal entry point for time travelers and external programs. This approach provides a clean abstraction layer while maintaining backward compatibility.

The following code illustrates the structure of inbound messages to program accounts:

```haskell
-- Messages from time travelers to program accounts
data ProgramAccountMessage
  = DepositAsset Asset Amount ChainID (Maybe RegisterHint)
  | WithdrawAsset Asset Amount Address ChainID
  | ExecuteProgram ProgramID [Parameter] (Maybe ExecutionConstraint)
  | InvokeRegister RegisterID OperationType [Parameter]
  | CreateRegister RegisterType InitialContents [AuthMethod]
  | ManageAuthMethod RegisterID AuthMethodOp
  | QueryState QueryType
  deriving (Show)

-- Hints assist program accounts with register routing
data RegisterHint
  = UseExistingRegister RegisterID
  | CreateNewRegister RegisterType
  | RoutingTag Text
  | PreferredAuth AuthMethod
  | ControllerPreference ControllerLabel  -- Added for ADR_018
  deriving (Show)

-- Authorization method operations
data AuthMethodOp
  = AddAuth AuthMethod
  | RemoveAuth AuthMethod
  | UpdateAuth AuthMethod AuthMethod
  deriving (Show)
```

In practice, the implementation flow follows this pattern:

```haskell
-- Example: Time traveler deposits ETH with ZK circuit routing
messageToProgramAccount :: ProgramAccountMessage
messageToProgramAccount = DepositAsset 
    (Asset "ETH") 
    (Amount 5.0) 
    (ChainID "ethereum") 
    (Just $ PreferredAuth $ ZKProofAuthorization zkCircuitKey Nothing)

-- Program account message processing
processMessage :: ProgramAccount -> ProgramAccountMessage -> IO (Either ProcessError ResponseMessage)
processMessage account msg = case msg of
  DepositAsset asset amount chainId hint -> do
    -- Locate or create appropriate register based on hint
    registerId <- findOrCreateRegister account asset chainId hint
    
    -- Create controller label for the deposit
    controllerLabel <- createControllerLabel chainId account
    
    -- Generate deposit address/instructions
    depositInfo <- generateDepositInstructions registerId asset chainId controllerLabel
    
    -- Configure observation for the deposit
    setupDepositObservation registerId asset amount chainId controllerLabel
    
    -- Return deposit instructions to time traveler
    pure $ Right $ DepositInstructions depositInfo
    
  -- Handle additional message types...
```

The underlying mechanism operates efficiently. Program accounts actively create appropriate register structures based on requests, configure authorization methods, and establish observations to detect deposit arrivals.

The `RegisterHint` pattern offers significant advantages. Users often have specific requirements for deposit handling - some prefer dedicated registers for isolation, while others favor reusing existing registers for batching. Providing control through hints rather than imposing a single pattern has proven effective in similar systems.

For the cross-chain scenario described earlier, a composed flow would be implemented as follows:

```haskell
-- Time traveler configuration for cross-chain operation
sequence = do
  -- 1. Create register on source chain
  sourceRegId <- sendMessage account $ CreateRegister 
    TokenRegisterType 
    (InitialTokenBalance "TIME" 100) 
    [OwnershipAuth, ZKProofAuth zkKey1]
    
  -- 2. Create register on destination chain with controller label
  let controllerLabel = ControllerLabel
        { creatingController = "ethereum"
        , terminalController = "celestia"
        , affectingControllers = ["ethereum", "celestia"]
        , backupControllers = ["ethereum"]
        }
        
  destRegId <- sendMessage account $ CreateRegister
    CompositeRegisterType
    EmptyContents
    [ZKProofAuth zkKey2, ConditionAuth conditionPredicate]
    
  -- 3. Deposit TIME tokens into source register
  depositResult <- sendMessage account $ DepositAsset 
    (Asset "TIME") 
    (Amount 100.0)
    (ChainID "ethereum")
    (Just $ UseExistingRegister sourceRegId)
    
  -- 4. Configure cross-chain transfer with resource formalization
  transferResult <- sendMessage account $ InvokeRegister
    sourceRegId
    CrossChainTransferOp
    [ ParamRegisterID destRegId
    , ParamData applicationData
    , ParamBridge "LayerZero"
    , ParamControllerLabel controllerLabel  -- Added for ADR_018
    ]
    
  -- 5. Configure conditional execution on destination chain
  executeResult <- sendMessage account $ ExecuteProgram
    combinerProgramId
    [ ParamRegisterID destRegId  -- TIME tokens + data
    , ParamAsset "MONEY" (Amount 50.0)  -- Additional MONEY tokens
    ]
    (Just $ ExecuteAfter $ BlockHeight 1000)
```

From the time traveler's perspective, this process involves sending a sequence of messages to their program account. The underlying mechanisms of register creation, cross-chain messaging, and ZK proof generation are managed transparently.

An important benefit of this design is that the complexity of the register system becomes an implementation detail. Time travelers need not comprehend the intricacies of authorization tables or execution sequences - they interact with a high-level API that maintains compatibility with the original Time Bandits system.

Effective error propagation is crucial in systems handling asynchronous cross-chain operations. Each message should return not only success or failure status but sufficient context for diagnosis and potential recovery:

```haskell
data ResponseMessage
  = Success OperationID (Maybe RegisterID)
  | Pending OperationID StatusInfo
  | Failure OperationID ErrorDetails
  deriving (Show)
  
data ErrorDetails = ErrorDetails
  { errorCode :: ErrorCode
  , errorMessage :: Text
  , errorLocation :: ErrorLocation
  , recoveryOptions :: [RecoveryAction]
  , controllerStatus :: Maybe ControllerStatus  -- Added for ADR_018
  }

-- Controller status information for error handling
data ControllerStatus
  = ControllerOperational ControllerID
  | ControllerHalted ControllerLabel
  | ControllerByzantine ControllerLabel
  deriving (Show)
```