#!/bin/bash

# Script to run the Template Haskell test executable

echo "Building and running Template Haskell test..."

# Build the executable
cabal build test-template-haskell

# Run the executable if built successfully
if [ $? -eq 0 ]; then
  cabal run test-template-haskell
else
  echo "Build failed. See errors above."
  exit 1
fi 