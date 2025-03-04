#!/usr/bin/env bash
# Script to run all property tests

set -euo pipefail

# Default values
VERBOSITY="normal"
PATTERN="*"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --verbosity)
      VERBOSITY="$2"
      shift 2
      ;;
    --pattern)
      PATTERN="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

echo "Running tests with pattern: $PATTERN"
echo "Verbosity: $VERBOSITY"

# Run the tests
cabal test --test-show-details=direct --test-option="--pattern=$PATTERN" --test-option="--verbosity=$VERBOSITY" 