feat(TEL): Fix operator pattern matching and hash type conversions

This PR addresses two important issues in the TEL Interpreter:

1. **Operator Pattern Matching**: Updated the `applyOperator` function to use proper `Operator` data constructors instead of string literals. This eliminates the "IsString Operator" type errors and provides better type safety.

2. **Hash Type Conversions**: Fixed mock implementations to use proper hash constructors:
   - Updated `mockResource` to use `EntityHash` and `computeHash` for resource IDs, timeline hashes, and actor hashes
   - Fixed ByteString encoding for metadata

3. **Effect Propagation**: Added `LogicalClock` context to all interpreter functions that required it, ensuring proper time handling across the interpreter.

The changes ensure type safety and proper pattern matching throughout the TEL interpreter, enabling better integration with the Core effect system. An extensive integration status document has been updated to track progress and remaining tasks.

Remaining issues that will be addressed in subsequent PRs:
- Export `InternalError` constructor for REPL functionality
- Clean up unused code (imports, bindings, pattern matches)
- Expand test coverage for the TEL interpreter pipeline

Resolves #XXX (replace with actual issue number) 