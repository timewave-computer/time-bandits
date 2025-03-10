{ pkgs ? import <nixpkgs> {} }:

let
  # Get the time-bandits Haskell package from the current directory
  timeBandits = (import ../. {}).packages.${pkgs.system}.time-bandits;
  
  # Define the script as a derivation
  script = pkgs.writeShellScriptBin "generate-test-report" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Fixed output directory
    OUTPUT_DIR="test-report-out"
    
    # Get the project root directory
    PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo "$(dirname "$0")/..")"
    cd "$PROJECT_ROOT"
    
    echo "Project root: $PROJECT_ROOT"
    echo "Output directory: $OUTPUT_DIR"
    
    # Create output directory if it doesn't exist
    if [ ! -d "$OUTPUT_DIR" ]; then
      echo "Creating output directory: $OUTPUT_DIR"
      mkdir -p "$OUTPUT_DIR" || {
        echo "Error: Could not create directory $OUTPUT_DIR"
        echo "Using temporary directory instead"
        OUTPUT_DIR="$(mktemp -d)"
        echo "Temporary directory: $OUTPUT_DIR"
      }
    fi
    
    echo "Building time-bandits project with Nix..."
    nix build .#time-bandits
    
    echo "Running tests and generating report..."
    nix run .#generate-minimal-report -- "$OUTPUT_DIR"
    
    echo ""
    echo "Report generation complete!"
    echo "Reports are available in: $OUTPUT_DIR"
    echo "Open $OUTPUT_DIR/latest_report.md to view the latest test results."
  '';

in {
  # Return the script
  inherit script;
  
  # Define a derivation for the test report itself
  defaultPackage = pkgs.stdenv.mkDerivation {
    name = "time-bandits-test-report";
    src = ./.;
    
    buildInputs = [
      script
      timeBandits
    ];
    
    buildPhase = ''
      mkdir -p $out/test-report-out
      cd $out
      export HOME=$out
      export TMP=$out
      export TMPDIR=$out
      generate-test-report || echo "Report generation failed but continuing build"
    '';
    
    installPhase = ''
      # Copy reports if they exist
      if [ -d "test-report-out" ]; then
        cp -r test-report-out/* $out/test-report-out/ || true
      fi
      echo "Test reports available in $out/test-report-out"
    '';
  };
} 