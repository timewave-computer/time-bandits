{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.ghc963;
in
{
  # Test-related dependencies
  hspec = pkgs.haskell.lib.compose.dontCheck (
    pkgs.haskell.lib.compose.doJailbreak (
      haskellPackages.hspec
    )
  );
  hspec-discover = pkgs.haskell.lib.compose.dontCheck (
    pkgs.haskell.lib.compose.doJailbreak (
      haskellPackages.hspec-discover
    )
  );
  hspec-core = pkgs.haskell.lib.compose.dontCheck (
    pkgs.haskell.lib.compose.doJailbreak (
      haskellPackages.hspec-core
    )
  );
  QuickCheck = pkgs.haskell.lib.compose.dontCheck (
    pkgs.haskell.lib.compose.doJailbreak (
      haskellPackages.QuickCheck
    )
  );
  quickcheck-instances = haskellPackages.quickcheck-instances;
  
  # Project dependencies
  polysemy = haskellPackages.polysemy;
  polysemy-plugin = haskellPackages.polysemy-plugin;
  relude = haskellPackages.relude;
  cryptonite = haskellPackages.cryptonite;
  memory = haskellPackages.memory;
  aeson = haskellPackages.aeson;
  containers = haskellPackages.containers;
  bytestring = haskellPackages.bytestring;
  text = haskellPackages.text;
  time = haskellPackages.time;
  async = haskellPackages.async;
  mtl = haskellPackages.mtl;
} 