# 🧪 Time Bandits Test Results

🕐 **Generated on:** Wed Mar 12 07:29:32 EST 2025
📋 **Status:** ❌ BUILD FAILED

## 📊 Summary

| Metric | Count |
|--------|-------|
| Test Suites | Unknown |
| Test Cases | Unknown |
| Passed | 0  |
| Failed | 0 (build issues) ❌ |
| Pending | 0  |
| Stubbed | 0  |



## 📚 Module Status

No detailed module information available.






## 📝 All Tests and Their Status

| Module | Test | Purpose | Last Modified | Status |
|--------|------|---------|--------------|--------|
| Test Mode Scenarios | In-Memory Mode Resource Transfer Scenario | Tests resource transfer in In-Memory mode | 2025-03-10 | ✅ |
| Test Mode Scenarios | Local Multi-Process Mode Resource Transfer Scenario | Tests resource transfer in Local Multi-Process mode | 2025-03-10 | ✅ |
| Test Mode Scenarios | Geo-Distributed Mode Simulation | Tests environment setup for Geo-Distributed mode | 2025-03-10 | ✅ |
| Standalone Tests / Schema Evolution Tests | Rejected invalid schema modification | Verifies schema validation for incorrect changes | 2025-03-07 | ✅ |
| Standalone Tests / Schema Evolution Tests | Schema evolution tests | Validates Schema evolution tests functionality | 2025-03-07 | ✅ |
| Standalone Tests / Schema Evolution Tests | adding a nullable field to a schema | Tests adding a nullable field to a schema | 2025-03-07 | ✅ |
| Standalone Tests / Schema Evolution Tests | adding a required field (should fail) | Tests adding a required field (should fail) | 2025-03-07 | ✅ |
| Standalone Tests / TEL Parser Tests | Invalid type conversion | Verifies type conversion validation | 2025-03-05 | ✅ |
| Standalone Tests / TEL Parser Tests | TEL parsing | Tests TEL parsing | 2025-03-05 | ✅ |
| Standalone Tests / TEL Parser Tests | TEL parsing | Verifies successful TEL parsing | 2025-03-05 | ✅ |
| Standalone Tests / TEL Parser Tests | TEL tests | Validates TEL tests functionality | 2025-03-05 | ✅ |
| Standalone Tests / TEL Parser Tests | TEL type checking | Tests TEL type checking | 2025-03-05 | ✅ |
| Standalone Tests / TEL Parser Tests | TEL type checking | Verifies successful TEL type checking | 2025-03-05 | ✅ |
| Standalone Tests / TEL Parser Tests | TEL type conversion | Tests TEL type conversion | 2025-03-05 | ✅ |
| Standalone Tests / TEL Parser Tests | Type conversion - Int to string: 42 | Tests TEL type conversion | 2025-03-05 | ✅ |
| Standalone Tests / TEL Parser Tests | Type conversion - String to int: 123 | Tests TEL type conversion | 2025-03-05 | ✅ |
| TOML Parser / Rule parsing | should attempt to parse a rule from TOML text | Tests the parser's ability to handle TOML text input | 2025-03-03 | ✅ |
| TOML Parser / Rule parsing | should attempt to parse a rule from a file | Validates parsing rules from TOML files | 2025-03-03 | ✅ |
| TOML Parser / Rule parsing | should attempt to parse a rule set | Tests parsing complete rulesets from TOML | 2025-03-03 | ✅ |
| Fact Observation Integration / Configuration integration | correctly applies engine configuration | Ensures engine configuration options are properly applied | 2025-03-01 | ✅ |
| Fact Observation Integration / End-to-end rule processing | loads rules from TOML files and evaluates them | End-to-end test of loading rules from files and evaluating data | 2025-03-01 | ✅ |
| Fact Observation Integration / End-to-end rule processing | processes rules with multiple conditions | Tests rules with complex condition combinations | 2025-03-01 | ✅ |
| Fact Observation Engine / Engine initialization | creates a fact observation engine with default config | Verifies that the engine initializes properly with default configuration | 2025-02-28 | ✅ |
| Fact Observation Engine / Rule evaluation | evaluates rules against input data | Tests the engine's ability to evaluate rules against provided data | 2025-02-28 | ✅ |
| Fact Observation Engine / Rule evaluation | handles rule conditions correctly | Ensures conditions in rules are correctly evaluated | 2025-02-28 | ✅ |
| Fact Observation Engine / Rule loading and validation | loads rules from a directory | Tests loading multiple rule files from a directory | 2025-02-28 | ✅ |
| Fact Observation Engine / Rule loading and validation | validates rules during loading | Verifies rule validation during the loading process | 2025-02-28 | ✅ |
| Fact Observation CLI / CLI commands | loads rules from a directory | Tests loading multiple rule files from a directory | 2025-02-25 | ✅ |
| Fact Observation CLI / CLI commands | reports errors for invalid rules | Ensures proper error reporting for invalid rules | 2025-02-25 | ✅ |
| Fact Observation CLI / CLI commands | runs with help flag | Verifies CLI help functionality | 2025-02-25 | ✅ |
| Fact Observation CLI / CLI commands | validates rules through CLI | Tests rule validation through the CLI interface | 2025-02-25 | ✅ |
| Rules / Rule creation and management | adds rules to a ruleset | Confirms that rules can be successfully added to a ruleset | 2025-02-20 | ✅ |
| Rules / Rule creation and management | creates valid rules | Verifies that valid rules can be created with all required fields | 2025-02-20 | ✅ |
| Rules / Rule creation and management | detects duplicate rule IDs | Ensures that duplicate rule IDs are properly detected | 2025-02-20 | ✅ |
| Rules / Rule creation and management | successfully adds rules with unique IDs | Verifies that rules with unique IDs can be added to a ruleset | 2025-02-20 | ✅ |
| Rules Module | can create a rule | Tests basic rule creation functionality | 2025-02-18 | ✅ |
| Rules Module | can create an empty RuleSet | Verifies empty ruleset creation | 2025-02-18 | ✅ |
| Rules Module | enforces unique rule IDs within a RuleSet | Ensures rule ID uniqueness is enforced in rulesets | 2025-02-18 | ✅ |
| Additional Tests / MiniConsensus | MiniConsensusTest | Tests MiniConsensus functionality | 2025-02-15 | ✅ |
| Additional Tests / MiniFactObservation | MiniFactObservationTest | Tests MiniFactObservation functionality | 2025-02-15 | ✅ |
| Additional Tests / MiniLog | MiniLogTest | Tests MiniLog functionality | 2025-02-15 | ✅ |
| Additional Tests / MiniNetwork | MiniNetworkTest | Tests MiniNetwork functionality | 2025-02-15 | ✅ |
| Additional Tests / MiniSchema | MiniSchemaTest | Tests MiniSchema functionality | 2025-02-15 | ✅ |
| Additional Tests / MiniTEL | MiniTELTest | Tests MiniTEL functionality | 2025-02-15 | ✅ |
| Additional Tests / MiniTimeline | MiniTimelineTest | Tests MiniTimeline functionality | 2025-02-15 | ✅ |

## 📝 Test Output

```

```

For complete test output, see the test_output.log file.

## 📝 Test Mode Scenario Tests

The Test Mode Scenario Tests validate that the Time Bandits system can correctly function in all supported execution modes:

### In-Memory Mode
- All actors run in a single process, directly invoking each other's functions
- Tests resource transfer between timelines

### Local Multi-Process Mode  
- Each actor runs in its own separate process, started by the simulation controller
- Tests the same resource transfer scenario as in-memory mode

### Geo-Distributed Mode
- Setup for actors running on remote machines
- Tests environment configuration for distributed deployment

These tests ensure that the same business logic works correctly across all deployment modes, from development to production environments.
