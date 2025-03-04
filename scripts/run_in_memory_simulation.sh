#!/usr/bin/env bash
# Script to run an in-memory simulation

set -euo pipefail

# Default values
SCENARIO="basic"
TRAVELER_COUNT=1
KEEPER_COUNT=1
BANDIT_COUNT=0
LOG_LEVEL="info"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --scenario)
      SCENARIO="$2"
      shift 2
      ;;
    --travelers)
      TRAVELER_COUNT="$2"
      shift 2
      ;;
    --keepers)
      KEEPER_COUNT="$2"
      shift 2
      ;;
    --bandits)
      BANDIT_COUNT="$2"
      shift 2
      ;;
    --log-level)
      LOG_LEVEL="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

echo "Running in-memory simulation with scenario: $SCENARIO"
echo "Travelers: $TRAVELER_COUNT, Keepers: $KEEPER_COUNT, Bandits: $BANDIT_COUNT"
echo "Log level: $LOG_LEVEL"

# Run the simulation
cabal run time-bandits -- sim in-memory \
  --scenario "$SCENARIO" \
  --traveler-count "$TRAVELER_COUNT" \
  --keeper-count "$KEEPER_COUNT" \
  --bandit-count "$BANDIT_COUNT" \
  --log-level "$LOG_LEVEL" 