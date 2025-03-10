# Test Report System

This directory contains a test report generation system for Time Bandits that produces detailed, human-readable reports of test execution results in Markdown format.

## System Components

1. **TestReport Module** - Core data structures and rendering for the report format
2. **HspecReporter** - Custom formatter for Hspec test results 
3. **TastyReporter** - Custom reporter for Tasty test results
4. **GenerateTestReport** - Standalone executable that runs tests and generates reports
5. **run-test-report** - A unified script that works with both Cabal and Nix

## Current Status

The test report system is currently partially implemented. The infrastructure is in place, but there are several compilation errors that need to be fixed before it can be used.

## Implementation To-Do List

### 1. Fix Hspec Reporter Issues

The HspecReporter has some import errors that need to be fixed:

```haskell
Module 'Test.Hspec.Core.Formatters' does not export 'Path'
Module 'Test.Hspec.Core.Formatters' does not export 'specdescPath'
```

This can be fixed by reviewing the actual API of Test.Hspec.Core.Formatters and adjusting our imports and implementations to match.

### 2. Fix Tasty Reporter Issues

The TastyReporter has similar import errors:

```haskell
Module 'Test.Tasty.Runners' does not export 'ResultDetail(..)'
Module 'Test.Tasty.Runners' does not export 'fold'
Module 'Test.Tasty.Runners' does not export 'nutritionistIngredient'
```

This can be fixed by reviewing the Tasty API documentation and adjusting our implementation to use the available functions and types.

### 3. Fix RulesSpec Implementation

The RulesSpec module has several type mismatch errors that need to be fixed:

```haskell
Couldn't match expected type 'Rules.RuleSet' with actual type 'Map.Map Text Text -> Rules.RuleSet'
```

This likely indicates that the `createRuleSet` function has a different signature than expected. Review the actual implementation of `Core.FactObservation.Rules` and adjust the spec accordingly.

### 4. Fix Nix Integration

The Nix integration for the test report generator is partially implemented but needs some adjustments. The current issues are:

1. The `generate-test-report` executable is not being built correctly in the flake.nix
2. The `test-report-generator` script is not working due to path resolution issues

### 5. Test Suite Organization

Once the compilation issues are fixed, the test suite organization should be reviewed to ensure:

1. Tests are properly grouped by module/function
2. Test descriptions are clear and indicate what is being tested
3. Test suites have proper setup and teardown to ensure consistency

## Usage (When Fixed)

After fixes are implemented, the test reports can be generated with:

```bash
# Using the standalone script
./run-test-report [output-directory]

# Using just
just test-report [output-directory]

# Using cabal directly
cabal run generate-test-report -- [output-directory]

# Using nix directly
nix run .#test-report-generator -- [output-directory]
```

The generated reports will be available in the output directory (defaults to `test-reports`), with the most recent report always copied to `latest_report.md` for easy access and git tracking.

## Design Notes

The test report system is designed to:

1. Capture metadata about tests (name, module, description, duration)
2. Record test results in a structured format
3. Render reports in Markdown for easy viewing
4. Support both Hspec and Tasty testing frameworks
5. Work consistently across both Cabal and Nix builds
6. Be extensible for future test frameworks and report formats 