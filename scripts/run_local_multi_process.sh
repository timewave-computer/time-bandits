#!/usr/bin/env bash
# Script to run a local multi-process simulation

set -euo pipefail

# Default values
SCENARIO="basic"
TRAVELER_COUNT=2
KEEPER_COUNT=1
BANDIT_COUNT=0
LOG_LEVEL="info"
PORT_BASE=8000

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
    --port-base)
      PORT_BASE="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

echo "Running local multi-process simulation with scenario: $SCENARIO"
echo "Travelers: $TRAVELER_COUNT, Keepers: $KEEPER_COUNT, Bandits: $BANDIT_COUNT"
echo "Log level: $LOG_LEVEL, Port base: $PORT_BASE"

# Start the controller
cabal run time-bandits -- controller --port "$PORT_BASE" --scenario "$SCENARIO" &
CONTROLLER_PID=$!

# Wait for controller to initialize
sleep 2

# Start time keepers
for ((i=1; i<=KEEPER_COUNT; i++)); do
  PORT=$((PORT_BASE + i))
  cabal run time-bandits -- keeper --id "keeper$i" --port "$PORT" --controller-port "$PORT_BASE" &
  KEEPER_PIDS+=($!)
done

# Start time travelers
for ((i=1; i<=TRAVELER_COUNT; i++)); do
  PORT=$((PORT_BASE + KEEPER_COUNT + i))
  cabal run time-bandits -- traveler --id "traveler$i" --port "$PORT" --controller-port "$PORT_BASE" &
  TRAVELER_PIDS+=($!)
done

# Start time bandits if requested
for ((i=1; i<=BANDIT_COUNT; i++)); do
  PORT=$((PORT_BASE + KEEPER_COUNT + TRAVELER_COUNT + i))
  cabal run time-bandits -- bandit --id "bandit$i" --port "$PORT" --controller-port "$PORT_BASE" &
  BANDIT_PIDS+=($!)
done

# Wait for user to terminate
echo "Press Ctrl+C to terminate the simulation"
trap 'kill $CONTROLLER_PID ${KEEPER_PIDS[@]} ${TRAVELER_PIDS[@]} ${BANDIT_PIDS[@]} 2>/dev/null || true' EXIT
wait $CONTROLLER_PID 