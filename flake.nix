{
  description = "Distributed time travel";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          config = {
            allowBroken = true;
            allowUnfree = true;
          };
          overlays = [ (final: prev: {
            haskell = prev.haskell // {
              packages = prev.haskell.packages // {
                ghc963 = prev.haskell.packages.ghc963.override {
                  overrides = self: super: {
                    polysemy = pkgs.haskell.lib.compose.dontCheck (
                      pkgs.haskell.lib.compose.doJailbreak (
                        super.callCabal2nixWithOptions "polysemy" (pkgs.fetchFromGitHub {
                          owner = "polysemy-research";
                          repo = "polysemy";
                          rev = "1.9.2.0";
                          sha256 = "0qp6g44hbyjgbw4awpw6aiysv8cjlr0dik94b77mjwvf74lnamj0";
                        }) "--flag=-test-doctests" {}
                      )
                    );
                    polysemy-plugin = pkgs.haskell.lib.compose.dontCheck (
                      pkgs.haskell.lib.compose.doJailbreak (
                        super.callHackage "polysemy-plugin" "0.4.5.0" {}
                      )
                    );
                    polysemy-log = pkgs.haskell.lib.compose.dontCheck (
                      pkgs.haskell.lib.compose.doJailbreak (
                        super.callHackage "polysemy-log" "0.9.0.0" {}
                      )
                    );
                  };
                };
              };
            };
          }) ];
        };

        packages.default = self'.packages.time-bandits;
        
        # Define the time-bandits packages
        packages.time-bandits = pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "time-bandits" ./. "--flag=-test-doctests" {
          polysemy = pkgs.haskell.lib.compose.dontCheck (
            pkgs.haskell.lib.compose.doJailbreak (
              pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "polysemy" (pkgs.fetchFromGitHub {
                owner = "polysemy-research";
                repo = "polysemy";
                rev = "1.9.2.0";
                sha256 = "0qp6g44hbyjgbw4awpw6aiysv8cjlr0dik94b77mjwvf74lnamj0";
              }) "--flag=-test-doctests" {}
            )
          );
          polysemy-plugin = pkgs.haskell.lib.compose.dontCheck (
            pkgs.haskell.lib.compose.doJailbreak (
              pkgs.haskell.packages.ghc963.polysemy-plugin
            )
          );
        };

        # Define subcomponents
        packages.time-bandits-core = pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "time-bandits-core" ./core "--flag=-test-doctests" {
          polysemy = pkgs.haskell.packages.ghc963.polysemy;
          polysemy-plugin = pkgs.haskell.packages.ghc963.polysemy-plugin;
        };

        packages.time-bandits-programs = pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "time-bandits-programs" ./programs "--flag=-test-doctests" {
          polysemy = pkgs.haskell.packages.ghc963.polysemy;
          polysemy-plugin = pkgs.haskell.packages.ghc963.polysemy-plugin;
          time-bandits-core = self'.packages.time-bandits-core;
        };

        packages.time-bandits-actors = pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "time-bandits-actors" ./actors "--flag=-test-doctests" {
          polysemy = pkgs.haskell.packages.ghc963.polysemy;
          polysemy-plugin = pkgs.haskell.packages.ghc963.polysemy-plugin;
          time-bandits-core = self'.packages.time-bandits-core;
        };

        packages.time-bandits-execution = pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "time-bandits-execution" ./execution "--flag=-test-doctests" {
          polysemy = pkgs.haskell.packages.ghc963.polysemy;
          polysemy-plugin = pkgs.haskell.packages.ghc963.polysemy-plugin;
          time-bandits-core = self'.packages.time-bandits-core;
          time-bandits-programs = self'.packages.time-bandits-programs;
        };

        packages.time-bandits-adapters = pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "time-bandits-adapters" ./adapters "--flag=-test-doctests" {
          polysemy = pkgs.haskell.packages.ghc963.polysemy;
          polysemy-plugin = pkgs.haskell.packages.ghc963.polysemy-plugin;
          time-bandits-core = self'.packages.time-bandits-core;
        };

        packages.time-bandits-proofs = pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "time-bandits-proofs" ./proofs "--flag=-test-doctests" {
          polysemy = pkgs.haskell.packages.ghc963.polysemy;
          polysemy-plugin = pkgs.haskell.packages.ghc963.polysemy-plugin;
          time-bandits-core = self'.packages.time-bandits-core;
        };

        packages.time-bandits-cli = pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "time-bandits-cli" ./cli "--flag=-test-doctests" {
          polysemy = pkgs.haskell.packages.ghc963.polysemy;
          polysemy-plugin = pkgs.haskell.packages.ghc963.polysemy-plugin;
          time-bandits-core = self'.packages.time-bandits-core;
          time-bandits-programs = self'.packages.time-bandits-programs;
          time-bandits-actors = self'.packages.time-bandits-actors;
          time-bandits-execution = self'.packages.time-bandits-execution;
          time-bandits-adapters = self'.packages.time-bandits-adapters;
          time-bandits-proofs = self'.packages.time-bandits-proofs;
        };
        
        # Add test suite
        packages.time-bandits-test = pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "time-bandits" ./. "--flag=-test-doctests" {
          polysemy = pkgs.haskell.lib.compose.dontCheck (
            pkgs.haskell.lib.compose.doJailbreak (
              pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "polysemy" (pkgs.fetchFromGitHub {
                owner = "polysemy-research";
                repo = "polysemy";
                rev = "1.9.2.0";
                sha256 = "0qp6g44hbyjgbw4awpw6aiysv8cjlr0dik94b77mjwvf74lnamj0";
              }) "--flag=-test-doctests" {}
            )
          );
          polysemy-plugin = pkgs.haskell.lib.compose.dontCheck (
            pkgs.haskell.lib.compose.doJailbreak (
              pkgs.haskell.packages.ghc963.polysemy-plugin
            )
          );
          hspec = pkgs.haskell.lib.compose.dontCheck (
            pkgs.haskell.lib.compose.doJailbreak (
              pkgs.haskell.packages.ghc963.hspec
            )
          );
          hspec-discover = pkgs.haskell.lib.compose.dontCheck (
            pkgs.haskell.lib.compose.doJailbreak (
              pkgs.haskell.packages.ghc963.hspec-discover
            )
          );
          hspec-core = pkgs.haskell.lib.compose.dontCheck (
            pkgs.haskell.lib.compose.doJailbreak (
              pkgs.haskell.packages.ghc963.hspec-core
            )
          );
          QuickCheck = pkgs.haskell.lib.compose.dontCheck (
            pkgs.haskell.lib.compose.doJailbreak (
              pkgs.haskell.packages.ghc963.QuickCheck
            )
          );
        };

        # Add a check to run the test
        checks.test = self'.packages.time-bandits-test;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            (haskell.packages.ghc963.ghcWithPackages (ps: with ps; [
              cabal-install
              haskell-language-server
              polysemy
              polysemy-plugin
              tasty
              tasty-hunit
            ]))
            ghcid
            stack
          ];

          shellHook = ''
            export NIX_GHC="$(which ghc)"
            export NIX_GHCPKG="$(which ghc-pkg)"
            export NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
            export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
          '';
        };
      };
    };
}
