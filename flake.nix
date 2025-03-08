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
                    # Create patched toml package
                    patchedToml = pkgs.haskell.lib.appendPatch 
                      (pkgs.haskell.lib.compose.doJailbreak super.toml)
                      (pkgs.writeTextFile {
                        name = "toml-parser-patch.patch";
                        text = ''
                          diff --git a/src/Text/TOML/Parser.hs b/src/Text/TOML/Parser.hs
                          index 0123456..abcdef 100644
                          --- a/src/Text/TOML/Parser.hs
                          +++ b/src/Text/TOML/Parser.hs
                          @@ -15,7 +15,7 @@ import qualified Data.Map.Strict as M
                           import qualified Data.List as L
                           import Data.Time.Format
                           import Data.Time.Clock
                          -import System.Locale
                          +-- import System.Locale
                           import Control.Applicative
                           import Control.Monad
                           import Data.Char
                          @@ -60,7 +60,7 @@ timeP = do
                                   char 'T'
                                   char 'Z'
                                   let dstr = dt <> "T" <> tm <> "Z"
                          -        let mt = parseTime defaultTimeLocale (iso8601DateFormat (Just "%X")) (B.unpack dstr)
                          +        let mt = parseTimeM True Data.Time.Format.defaultTimeLocale (iso8601DateFormat (Just "%X")) (B.unpack dstr)
                                   case mt of
                                     Just t -> return t
                                     Nothing -> fail $ "could not parse time: " <> B.unpack dstr
                        '';
                      });
                    
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
                    # Override toml to use our patched version
                    toml = self.patchedToml;
                    # Also jailbreak aeson-schema to handle dependency issues
                    aeson-schema = pkgs.haskell.lib.markUnbroken (
                      pkgs.haskell.lib.compose.doJailbreak (
                        super.callHackage "aeson-schema" "0.3.0.0" {}
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
          toml = pkgs.haskell.packages.ghc963.toml;
          aeson-schema = pkgs.haskell.lib.markUnbroken (
            pkgs.haskell.lib.compose.doJailbreak (
              pkgs.haskell.packages.ghc963.callHackage "aeson-schema" "0.3.0.0" {}
            )
          );
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
          toml = pkgs.haskell.packages.ghc963.toml;
          aeson-schema = pkgs.haskell.lib.markUnbroken (
            pkgs.haskell.lib.compose.doJailbreak (
              pkgs.haskell.packages.ghc963.callHackage "aeson-schema" "0.3.0.0" {}
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
              # Include the overridden packages
              toml
              (pkgs.haskell.lib.markUnbroken (
                pkgs.haskell.lib.compose.doJailbreak (
                  pkgs.haskell.packages.ghc963.callHackage "aeson-schema" "0.3.0.0" {}
                )
              ))
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
