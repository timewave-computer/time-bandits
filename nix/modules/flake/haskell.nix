{ inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { self', lib, config, pkgs, ... }: {
    # Our only Haskell project. You can have multiple projects, but this template
    # has only one.
    # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
    haskellProjects.default = {
      projectRoot = ../../.;

      # The base package set (this value is the default)
      basePackages = pkgs.haskell.packages.ghc963; # Use GHC 9.6.3 consistently

      packages = {
        # Add source or Hackage overrides here
        # (Local packages are added automatically)
        time-bandits.root = ../../.;
        polysemy = {
          source = pkgs.fetchFromGitHub {
            owner = "polysemy-research";
            repo = "polysemy";
            rev = "cd2f01c";
            sha256 = "0vsb5cynxsrscc5pjqsgzw550ndqgmq7f4agqy1jmn31m0rkzl95";
          };
          cabalExtras = ''
            flags: -test-doctests
          '';
        };
        polysemy-plugin = {
          source = pkgs.fetchFromGitHub {
            owner = "polysemy-research";
            repo = "polysemy";
            rev = "cd2f01c";
            sha256 = "0vsb5cynxsrscc5pjqsgzw550ndqgmq7f4agqy1jmn31m0rkzl95";
          };
          cabalExtras = ''
            flags: -test-doctests
          '';
        };
        polysemy-resume = {
          source = pkgs.fetchFromGitHub {
            owner = "tek";
            repo = "polysemy-resume";
            rev = "v0.9.0.0";
            sha256 = "0r7js21mvvk913kgc9l9i8xayrahkd0hl9ldd45srsdrlag184fd";
          };
          cabalExtras = ''
            flags: -test-doctests
          '';
        };
        polysemy-log = {
          version = "0.9.0.0";
          cabalExtras = ''
            flags: -test-doctests
          '';
        };
        doctest = {
          source = "0.22.0";
          package = pkgs.haskell.packages.ghc963.doctest;
        };
      };

      # Add your package overrides here
      settings = {
        time-bandits = {
          jailbreak = true;
          check = false;
          ghcOptions = [
            "-package polysemy-plugin-0.4.5.0"
            "-fplugin=Polysemy.Plugin"
          ];
        };
        polysemy = {
          check = false;
          jailbreak = true;
        };
        polysemy-plugin = {
          check = false;
          jailbreak = true;
        };
        polysemy-resume = {
          check = false;
          jailbreak = true;
        };
        polysemy-log = {
          check = false;
          jailbreak = true;
        };
      };

      # Development shell configuration
      devShell = {
        hlsCheck.enable = true;
        tools = hp: {
          hls = hp.haskell-language-server;
          cabal = hp.cabal-install;
          ghc = hp.ghc;
          doctest = hp.doctest;
        };
      };

      # What should haskell-flake add to flake outputs?
      autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
    };

    # Default package & app.
    packages.default = self'.packages.time-bandits;
    apps.default = self'.apps.time-bandits;

    packages.time-bandits = pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "time-bandits" ../../. "--flag=-test-doctests" {
      polysemy = pkgs.haskell.lib.compose.dontCheck (
        pkgs.haskell.lib.compose.doJailbreak (
          pkgs.haskell.packages.ghc963.callCabal2nixWithOptions "polysemy" (pkgs.fetchFromGitHub {
            owner = "polysemy-research";
            repo = "polysemy";
            rev = "v1.9.2.0";
            sha256 = "sha256-Hs+Hn2QxZXKQs+LHQYm+mSD32GzGRXLHam8NY7igKLY=";
          }) "--flag=-test-doctests" {}
        )
      );
      polysemy-plugin = pkgs.haskell.lib.compose.dontCheck (
        pkgs.haskell.lib.compose.doJailbreak (
          pkgs.haskell.packages.ghc963.polysemy-plugin
        )
      );
    };
  };
}
