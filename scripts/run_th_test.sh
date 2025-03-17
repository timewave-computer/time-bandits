#!/bin/bash

# A simple script to run the Template Haskell test

echo "Compiling and running Template Haskell test..."
cd "$(dirname "$0")/.."

# Try to build just the test script
echo "Building with cabal..."
cabal build scripts/test_template_haskell.hs

# If build is successful, run the test
if [ $? -eq 0 ]; then
  echo "Build successful! Running the test..."
  cabal run scripts/test_template_haskell.hs
else
  echo "Build failed. See errors above."
  exit 1
fi 