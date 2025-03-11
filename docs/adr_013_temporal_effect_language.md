# ADR 013: Temporal Effect Language (TEL)

## Status

Proposed

## Context

Time Bandits needs a specialized language for describing cross-timeline programs that can leverage our content-addressable code system. The existing TECL (Temporal Effect Combinator Language) implementation provides a good starting point, but we need to formalize it with a complete grammar that is:

1. **Homoiconic**: The code represents its own data structure, making it easy to manipulate and transform programmatically.
2. **Combinator-based**: Built on composable operators that combine smaller effects into more complex workflows.
3. **Human-readable**: Maintains clear, concise syntax that is easy to read and write.
4. **Easily composable**: Allows effects to be combined in intuitive ways with minimal boilerplate.
5. **Content-addressable**: Leverages our content-addressable code system for immutable, hash-based identification.

A well-designed temporal effect language will enable developers to express complex cross-timeline workflows while benefiting from the guarantees provided by our content-addressable code system.

## Decision

We will implement a formal Temporal Effect Language (TEL) with the following characteristics:

### 1. Core Design Principles

- **Expression-oriented**: Everything in TEL is an expression that evaluates to a value.
- **Immutable by default**: All values and code definitions are immutable.
- **Hash-addressed**: All TEL expressions are identified by their content hash.
- **Explicit side effects**: Effects that interact with external systems are explicitly marked.
- **Temporal awareness**: First-class support for time relationships, delays, and timeouts.
- **Type safety**: Strong typing with inference where possible.

### 2. Syntax and Grammar

The formal grammar for TEL is as follows:

```ebnf
# Top level constructs
Program         ::= Definition*

# Definitions
Definition      ::= TypeSignature? FunctionDef
TypeSignature   ::= Identifier "::" TypeExpr
FunctionDef     ::= Identifier Pattern* "=" Expression
                  | Identifier Pattern* 
                    GuardedExpr+

# Pattern matching
Pattern         ::= VarPattern | LiteralPattern | ConstructorPattern | WildcardPattern
VarPattern      ::= Identifier
LiteralPattern  ::= LiteralExpr
ConstructorPattern ::= Identifier Pattern*
WildcardPattern ::= "_"

# Guards
GuardedExpr     ::= "|" Expression "=" Expression
                  | "where" Declaration+
Declaration     ::= Identifier "=" Expression

# Types
TypeExpr        ::= BasicType | ListType | TupleType | FunctionType | EffectType
BasicType       ::= "Int" | "Double" | "Text" | "Bool" | "Hash" | "Time"
ListType        ::= "[" TypeExpr "]"
TupleType       ::= "(" TypeExpr ("," TypeExpr)+ ")"
FunctionType    ::= TypeExpr "->" TypeExpr
EffectType      ::= "Effect" TypeExpr
                  | "Timeline" TypeExpr

# Expressions
Expression      ::= LiteralExpr
                  | VariableExpr
                  | ApplicationExpr
                  | LambdaExpr
                  | EffectExpr
                  | LetExpr
                  | IfExpr
                  | CaseExpr
                  | TimeExpr
                  | InfixExpr
                  | HashRefExpr
                  | DoExpr

# Literals
LiteralExpr     ::= IntLiteral | DoubleLiteral | TextLiteral | BoolLiteral
                  | ListLiteral | TupleLiteral
IntLiteral      ::= Digit+
DoubleLiteral   ::= Digit+ "." Digit+
TextLiteral     ::= '"' [^"]* '"'
BoolLiteral     ::= "True" | "False"
ListLiteral     ::= "[" [Expression ("," Expression)*] "]"
TupleLiteral    ::= "(" [Expression ("," Expression)+] ")"

# Variables and function application
VariableExpr    ::= Identifier
ApplicationExpr ::= Expression Expression+
LambdaExpr      ::= "\" Pattern+ "->" Expression
LetExpr         ::= "let" Declaration+ "in" Expression

# Control flow
IfExpr          ::= "if" Expression 
                    "then" Expression 
                    "else" Expression
CaseExpr        ::= "case" Expression "of"
                    (Pattern "->" Expression)+

# Effect syntax using do-notation
DoExpr          ::= "do" DoStatement+
DoStatement     ::= Identifier "<-" Expression
                  | "let" Declaration+
                  | Expression

# Time expressions
TimeExpr        ::= "after" Expression TimeUnit Expression
                  | "within" Expression TimeUnit Expression
                  | "at" Expression Expression
TimeUnit        ::= "seconds" | "minutes" | "hours" | "days"

# Effect expressions
EffectExpr      ::= DepositExpr | WithdrawExpr | TransferExpr | ObserveExpr
                  | EmitExpr | InvokeExpr
DepositExpr     ::= "deposit" Expression "to" Expression "on" Expression
WithdrawExpr    ::= "withdraw" Expression "from" Expression "on" Expression
TransferExpr    ::= "transfer" Expression "from" Expression "to" Expression "on" Expression
ObserveExpr     ::= "observe" Expression "on" Expression
EmitExpr        ::= "emit" Expression
InvokeExpr      ::= "invoke" Expression

# Infix operators and combinators
InfixExpr       ::= Expression Operator Expression
Operator        ::= SequenceOp | ParallelOp | ChoiceOp | OtherOp
SequenceOp      ::= ">>"
ParallelOp      ::= "<|>"
ChoiceOp        ::= "<|"
OtherOp         ::= "+" | "-" | "*" | "/" | "==" | "/=" | "<" | ">" | "<=" | ">=" | "&&" | "||"

# Hash reference expression (for content-addressable code)
HashRefExpr     ::= "@" HashLiteral
HashLiteral     ::= HexDigit{64}  # SHA-256 hash as hex

# Terminals
Identifier      ::= Alpha (Alpha | Digit | "_")*
Alpha           ::= "a"..."z" | "A"..."Z"
Digit           ::= "0"..."9"
HexDigit        ::= Digit | "a"..."f" | "A"..."F"
```

### 3. Key Features

#### Content-Addressable Integration

TEL integrates with our content-addressable code system through:

1. **Hash References**: Any definition can be referenced by its content hash using the `@` syntax:
   ```haskell
   invoke (@a1b2c3d4...) arg1 arg2
   ```

2. **Immutable Definitions**: All definitions are immutable and stored in the content-addressable repository:
   ```haskell
   -- Function type signature (optional but recommended)
   transferFunds :: Double -> Account -> Account -> Effect ()
   -- Function definition
   transferFunds amount source dest = 
     withdraw amount from source on "ethereum" >>
     deposit amount to dest on "optimism"
   ```

3. **Version-Exact Dependencies**: Dependencies are specified by hash, ensuring exact versions:
   ```haskell
   complexWorkflow :: Param -> Effect Result
   complexWorkflow param = do
     let utilityFn = @a1b2c3d4...  -- Reference to function by hash
     result <- utilityFn param
     doSomethingElse result
   ```

#### Temporal Combinators

TEL provides first-class combinators for working with time, using Haskell-like operators:

1. **Sequence** (`>>`): Execute effects in order:
   ```haskell
   depositEffect >> waitForConfirmation >> withdrawEffect
   ```

2. **Parallel** (`<|>`): Execute effects in parallel:
   ```haskell
   observeEthPrice <|> observeBtcPrice <|> observeSolPrice
   ```

3. **Choice** (`<|`): Try one effect, falling back to another if the first fails:
   ```haskell
   tryPrimaryRoute <| tryBackupRoute
   ```

4. **Timeout**: Execute with a time limit:
   ```haskell
   timeout 30 minutes $ do
     confirmation <- waitForConfirmation
     completeTransfer confirmation
   ```

5. **Race**: Execute two effects, using the result of whichever completes first:
   ```haskell
   race 
     (waitForResponse)
     (after 10 seconds $ fallbackAction)
   ```

#### Do-Notation for Effects

TEL uses do-notation for sequencing effects:

```haskell
transferWithConfirmation :: Amount -> Account -> Account -> Effect Bool
transferWithConfirmation amount sender receiver = do
  -- Withdraw from sender
  txId <- withdraw amount from sender on "ethereum"
  
  -- Wait for confirmation
  confirmed <- waitForConfirmation txId
  
  -- If confirmed, deposit to receiver
  if confirmed
    then do
      deposit amount to receiver on "arbitrum"
      return True
    else
      return False
```

#### Timeline Interaction

Effects that interact with specific timelines use explicit timeline parameters:

```haskell
-- Transfer between two chains
crossChainTransfer :: Amount -> Account -> Account -> Effect TransferId
crossChainTransfer amount sender receiver = do
  -- Withdraw from source chain
  withdraw amount from sender on "ethereum" >>
  -- Deposit to destination chain
  deposit amount to receiver on "arbitrum"

-- Observe a price on multiple timelines
getBestPrice :: Asset -> Effect Price
getBestPrice asset = do
  priceA <- observe (price asset) on "uniswap"
  priceB <- observe (price asset) on "sushiswap"
  return $ min priceA priceB
```

### 4. Homoiconicity

TEL is designed to be homoiconic, meaning the code is represented as a data structure that can be manipulated by the language itself. This enables:

1. **Programmatic Manipulation**: Programs can create, analyze, and transform other programs.
2. **Metaprogramming**: Writing code that writes code.
3. **First-class Patterns**: Workflow patterns can be defined as reusable templates.

For example, a workflow pattern can be created and reused:

```haskell
-- Retry a function multiple times, with fallback
retry :: Effect a -> Int -> Effect a
retry action 0 = action
retry action n = action <| retry action (n-1)

-- Usage:
retry (sendTransaction tx) 3
```

## Consequences

### Positive Consequences

1. **Code Precision**: Content-addressable references ensure exact code versions are used.
2. **Safe Refactoring**: Names are metadata, enabling safe renaming without breaking dependencies.
3. **Improved Composition**: Combinators simplify the creation of complex workflows from simpler parts.
4. **Visual Reasoning**: The grammar maps cleanly to flowchart-like diagrams.
5. **Strong Error Handling**: Explicit failure paths and timeout behaviors.
6. **Cross-Timeline Safety**: Type safety across timeline boundaries.
7. **Auditability**: Programs are fully replayable and their causal graph matches declared control flow.

### Negative Consequences

1. **Learning Curve**: New syntax and concepts require learning investment.
2. **Implementation Complexity**: Requires substantial work to create parser, interpreter, and integration with existing systems.
3. **Performance Overhead**: Content-addressable lookup adds some performance cost.
4. **Storage Growth**: Each version of each function creates new entries in the repository.

### Mitigation Strategies

1. **Gradual Introduction**: Start with core features, expand over time.
2. **Strong Documentation**: Clear examples, tutorials, and reference documentation.
3. **Visual Tools**: Develop visualization tools to show program flow.
4. **Performance Optimizations**: Caching of frequently used definitions, parallel execution where possible.

## Implementation Plan

1. **Phase 1**: Core language parser and interpreter
2. **Phase 2**: Content-addressable integration
3. **Phase 3**: Timeline adapters and effect execution
4. **Phase 4**: Development tools (debugging, visualization)
5. **Phase 5**: Performance optimization

## Examples

### Example 1: Basic Cross-Chain Swap

```haskell
-- Swap ETH for USDC with price check
swapEthForUsdc :: Amount -> Amount -> Account -> Effect Bool
swapEthForUsdc amount minReceived account = do
  -- Withdraw ETH from account
  withdraw amount of "ETH" from account on "ethereum"
  
  -- Observe current price
  price <- observe "ETH/USDC" on "chainlink"
  
  -- Check if price meets minimum requirements
  if price * amount >= minReceived
    then do
      -- Execute the swap
      let usdcAmount = price * amount
      deposit usdcAmount of "USDC" to account on "ethereum"
      return True
    else do
      -- Return funds if price is too low
      deposit amount of "ETH" to account on "ethereum"
      return False
```

### Example 2: Multi-Chain Arbitrage with Timeout

```haskell
-- Find and execute cross-chain arbitrage opportunity
arbitrageOpportunity :: Token -> Amount -> Amount -> Minutes -> Effect Profit
arbitrageOpportunity token amount minProfit timeoutMins = do
  -- Observe prices on multiple DEXes in parallel
  prices <- observeAll [
      observe (price token) on "uniswap",
      observe (price token) on "sushiswap",
      observe (price token) on "curve"
    ]
  
  -- Find best buy and sell opportunities
  let bestBuy = minimumBy comparePrice prices
      bestSell = maximumBy comparePrice prices
      
  -- Calculate potential profit
  let potentialProfit = (priceOf bestSell - priceOf bestBuy) * amount
  
  -- Execute arbitrage if profitable, with timeout protection
  if potentialProfit > minProfit
    then timeout timeoutMins minutes $ do
      withdraw amount from account on (dexOf bestBuy)
      swapResult <- swap amount token on (dexOf bestBuy)
      transfer token to (dexOf bestSell)
      finalAmount <- swap token (amount + potentialProfit) on (dexOf bestSell)
      return $ finalAmount - amount
    else do
      emit "No profitable arbitrage opportunity found"
      return 0
```

### Example 3: Content-Addressed Dependencies

```haskell
-- Define utility functions with content-addressed references
calculateOptimalAmount :: Balance -> GasPrice -> Amount
calculateOptimalAmount balance gasPrice =
  let gasCost = @a1b2c3d4...  -- Reference to estimateGasCost function by hash
      safetyMargin = @e5f6g7h8...  -- Reference to calculateMargin function by hash
  in balance - gasCost gasPrice - safetyMargin balance

-- Main workflow
withdrawOptimalAmount :: Account -> Account -> Effect TransactionId
withdrawOptimalAmount source destination = do
  -- Observe account state
  balance <- observe (balance source) on "ethereum"
  gasPrice <- observe gasPrice on "ethereum"
  
  -- Calculate optimal amount using hash-referenced functions
  let amount = calculateOptimalAmount balance gasPrice
  
  -- Execute withdrawal and deposit
  txId <- withdraw amount from source on "ethereum"
  deposit amount to destination on "optimism"
  return txId
```

## Conclusion

The proposed Temporal Effect Language (TEL) provides an expressive, safe way to write cross-timeline programs within the Time Bandits system. By leveraging our content-addressable code system and focusing on composition, the language enables developers to create complex workflows with strong guarantees about correctness and determinism. Syntax should be familiar to functional programmers while maintaining the benefits of homoiconicity and content-addressing.