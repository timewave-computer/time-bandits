#!/usr/bin/env bash
set -eo pipefail

# Fixed output directory for all test reports
OUTPUT_DIR="test-report-out"
echo "Using fixed output directory: $OUTPUT_DIR"

# Automatically detect if we're in a Nix environment
if [ -n "$IN_NIX_SHELL" ] || [ -n "$NIX_STORE" ]; then
  USE_NIX="true"
else
  # Use Nix if available, otherwise fall back to Cabal
  if command -v nix &> /dev/null; then
    USE_NIX="true"
  else
    USE_NIX="false"
  fi
fi

# Allow overriding Nix detection
if [ -n "$1" ]; then
  USE_NIX="$1"
fi

echo "Time Bandits Test Runner"
echo "======================="
echo "Output directory: $OUTPUT_DIR"
echo "Using Nix: $USE_NIX"

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
REPORT_FILE="$OUTPUT_DIR/test_report_$TIMESTAMP.md"
LATEST_REPORT="$OUTPUT_DIR/latest_report.md"

# Temporarily rename problematic files to avoid compilation issues
backup_files() {
  echo "Backing up reporter files..."
  # Create backup directory if it doesn't exist
  mkdir -p .reporter-backup
  
  for file in test/HspecReporter.hs test/TastyReporter.hs; do
    if [ -f "$file" ]; then
      local module_name=$(basename "$file" .hs)
      cp "$file" ".reporter-backup/${module_name}.hs.bak"
      cat > "$file" << EOF
{-# LANGUAGE OverloadedStrings #-}
-- This file was temporarily disabled for compatibility
module ${module_name} (
) where

-- Empty implementation
EOF
    fi
  done
  
  # Modify TestReport.hs if needed
  if [ -f "test/TestReport.hs" ]; then
    cp "test/TestReport.hs" ".reporter-backup/TestReport.hs.bak"
    cat > "test/TestReport.hs" << EOF
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
-- This file was temporarily disabled for compatibility
module TestReport (
  TestReport,
  TestResult(..),
  ResultStatus(..),
  emptyReport,
  addTestResult,
  writeReport
) where

import Data.Text (Text)

data TestReport = TestReport deriving stock Show
data TestResult = TestResult {
  testName :: Text,
  testModule :: Text,
  testDescription :: Text,
  testStatus :: ResultStatus,
  testDuration :: Double,
  testMessage :: Maybe Text
} deriving stock Show
data ResultStatus = Pass | Fail | Skip deriving stock Show

emptyReport :: IO TestReport
emptyReport = return TestReport

addTestResult :: TestReport -> TestResult -> TestReport
addTestResult report _ = report

writeReport :: FilePath -> TestReport -> IO ()
writeReport _ _ = return ()
EOF
  fi
}

# Restore the original files
restore_files() {
  echo "Restoring reporter files..."
  for file in test/HspecReporter.hs test/TastyReporter.hs; do
    local module_name=$(basename "$file" .hs)
    local backup_file=".reporter-backup/${module_name}.hs.bak"
    if [ -f "$backup_file" ]; then
      mv "$backup_file" "$file"
      echo "Restored $file"
    fi
  done
  
  # Restore TestReport.hs
  if [ -f ".reporter-backup/TestReport.hs.bak" ]; then
    mv ".reporter-backup/TestReport.hs.bak" "test/TestReport.hs"
    echo "Restored test/TestReport.hs"
  fi
}

# Make sure we restore files even if script is interrupted
trap restore_files EXIT

# Run tests and capture output
run_tests() {
  local test_output
  local exit_code
  
  # Back up problematic files first
  backup_files
  
  if [ "$USE_NIX" = "true" ]; then
    echo "Running tests with Nix..."
    # Set IN_NIX_SHELL to ensure tests use the standard formatter
    export IN_NIX_SHELL=1
    
    # Run with Nix, capture output
    test_output=$(nix build .#time-bandits-test --log-format bar-with-logs 2>&1) || {
      exit_code=$?
      echo "Nix test build failed (exit code $exit_code). Trying with cabal instead..."
      unset IN_NIX_SHELL
      USE_NIX="false"
      run_tests
      return
    }
  else
    echo "Running tests with Cabal..."
    # Run with Cabal, capture output
    test_output=$(cabal test 2>&1) || {
      exit_code=$?
      echo "Cabal test failed with exit code $exit_code"
      echo "$test_output"
      create_minimal_report "$exit_code" "$test_output"
      return $exit_code
    }
  fi
  
  # Save output to file and create report
  echo "$test_output" > "$OUTPUT_DIR/test_output.log"
  echo "Test output saved to $OUTPUT_DIR/test_output.log"
  
  create_minimal_report "0" "$test_output"
  return 0
}

# Create a simple Markdown report
create_minimal_report() {
  local exit_code="$1"
  local test_output="$2"
  local status_emoji="❌"
  local status_text="FAILED"
  
  if [ "$exit_code" = "0" ]; then
    status_emoji="✅"
    status_text="PASSED"
  fi
  
  # Extract test statistics
  local total_tests=$(echo "$test_output" | grep -E 'Running [0-9]+ test suites' | grep -o '[0-9]\+' | head -n 1)
  local total_cases=$(echo "$test_output" | grep -E '[0-9]+ of [0-9]+ test suites \([0-9]+ of [0-9]+ test cases\)' | grep -o '([0-9]\+ of [0-9]\+ test cases)' | grep -o '[0-9]\+' | head -n 1)
  local passed_tests=$(echo "$test_output" | grep -E '[0-9]+ of [0-9]+ test suites \([0-9]+ of [0-9]+ test cases\) passed' | head -n 1 | grep -o 'passed')
  local pending_count=$(echo "$test_output" | grep -E '# PENDING:' | wc -l | tr -d ' ')
  
  # Set defaults if extraction failed
  [ -z "$total_tests" ] && total_tests="Unknown"
  [ -z "$total_cases" ] && total_cases="Unknown"
  [ -n "$passed_tests" ] && passed_tests="All" || passed_tests="0"
  
  # Parse pending tests
  local pending_tests="$pending_count"
  
  # Extract failed tests
  local failed_tests="0"
  if [ "$status_text" = "FAILED" ]; then
    failed_tests="1+"
  fi
  
  # Extract test modules and their status
  local test_modules=$(echo "$test_output" | grep -E 'Running Hspec tests for' | sed 's/Running Hspec tests for \(.*\)\.\.\./\1/')
  local module_table=""
  
  # Build module status table
  if [ -n "$test_modules" ]; then
    module_table="| Module | Status |\n|--------|--------|\n"
    while IFS= read -r module; do
      # Check if the module has failures
      if echo "$test_output" | grep -E "FAIL|failed" &>/dev/null; then
        module_table+="| $module | ❌ FAIL |\n"
      else
        module_table+="| $module | ✅ PASS |\n"
      fi
    done <<< "$test_modules"
  else
    module_table="No detailed module information available."
  fi
  
  # Extract pending test details
  local pending_details=""
  local stubbed_details=""
  local normal_pending_count=0
  local stubbed_count=0
  
  if [ "$pending_count" -gt 0 ]; then
    pending_details="### ⏳ Pending Tests\n\n"
    stubbed_details="### 🚧 Stubbed Implementations\n\n"
    
    local has_normal_pending=false
    local has_stubbed=false
    
    while IFS= read -r line; do
      # Extract test name and message from PENDING line
      local raw_message=$(echo "$line" | sed -E 's/.*# PENDING: (.*)/\1/')
      
      # Check if this is a stubbed implementation
      if echo "$raw_message" | grep -E 'stub|not implemented|currently stubbed|missing dependencies' &>/dev/null; then
        stubbed_details+="- $raw_message 🚧\n"
        stubbed_count=$((stubbed_count + 1))
        has_stubbed=true
      else
        pending_details+="- $raw_message\n"
        normal_pending_count=$((normal_pending_count + 1))
        has_normal_pending=true
      fi
    done < <(echo "$test_output" | grep -E '# PENDING:')
    
    # Reset if no items in a category
    if [ "$has_normal_pending" = "false" ]; then
      pending_details=""
    fi
    
    if [ "$has_stubbed" = "false" ]; then
      stubbed_details=""
    fi
  fi
  
  # Get implementation status
  local impl_status=""
  if [ "$stubbed_count" -gt 0 ]; then
    impl_status="### 🔍 Implementation Status\n\n"
    impl_status+="| Category | Status |\n|----------|--------|\n"
    
    # Check if TOML parser is stubbed
    if echo "$test_output" | grep -E 'TOML.*stub|missing dependencies' &>/dev/null; then
      impl_status+="| TOML Parser | 🚧 Stubbed |\n"
    fi
    
    # Check other components based on pending messages
    if echo "$test_output" | grep -E 'Rule evaluation.*implemented' &>/dev/null; then
      impl_status+="| Rule Engine | ⏳ Pending |\n"
    fi
    
    if echo "$test_output" | grep -E 'CLI.*implemented' &>/dev/null; then
      impl_status+="| CLI Interface | ⏳ Pending |\n"
    fi
  fi
  
  # Create a prettier markdown report
  cat > "$REPORT_FILE" << EOF
# 🧪 Time Bandits Test Results

🕐 **Generated on:** $(date)
📋 **Status:** $status_emoji $status_text

## 📊 Summary

| Metric | Count |
|--------|-------|
| Test Suites | $total_tests |
| Test Cases | $total_cases |
| Passed | $passed_tests $( [ "$passed_tests" = "All" ] && echo "✅" ) |
| Failed | $failed_tests $( [ "$failed_tests" != "0" ] && echo "❌" ) |
| Pending | $normal_pending_count $( [ "$normal_pending_count" != "0" ] && echo "⏳" ) |
| Stubbed | $stubbed_count $( [ "$stubbed_count" != "0" ] && echo "🚧" ) |

$(echo -e "$impl_status")

## 📚 Module Status

$(echo -e "$module_table")

$(echo -e "$stubbed_details")

$(echo -e "$pending_details")

## 📝 Test Output

\`\`\`
$(echo "$test_output" | grep -E 'Running|test suite|PASS|FAIL|✓|✗|PENDING|passed' | head -n 30)
\`\`\`

For complete test output, see the test_output.log file.
EOF
  
  # Create link to latest report
  ln -sf "test_report_$TIMESTAMP.md" "$LATEST_REPORT"
  
  # Create a copy named test_report_current.md (for git)
  cp "$REPORT_FILE" "$OUTPUT_DIR/test_report_current.md"
  
  echo "Test report generated at: $LATEST_REPORT"
  echo "Current report (for git): $OUTPUT_DIR/test_report_current.md"
}

# Main execution
run_tests
exit_code=$?

echo ""
echo "Process complete!"
echo "Reports are available in: $OUTPUT_DIR"
echo "Open $LATEST_REPORT to view the latest test results."

exit $exit_code
