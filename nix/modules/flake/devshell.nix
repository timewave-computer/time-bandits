{
  perSystem = { config, pkgs, ... }: {
    # Default shell.
    devShells.default = pkgs.mkShell {
      name = "time-bandits";
      meta.description = "Haskell development environment";
      # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell # See ./nix/modules/haskell.nix
        config.pre-commit.devShell # See ./nix/modules/formatter.nix
      ];
      packages = with pkgs; [
        just
        nixd
        ghciwatch
        ghcid
        cabal-install
        haskell-language-server
        ghc
      ];
      shellHook = ''
        # Clean up any existing configs that might interfere
        rm -rf .ghc.environment.* dist-newstyle cabal.project.local* .cabal

        # Set up GHC environment
        export PATH="${pkgs.haskell.compiler.ghc96}/bin:${pkgs.cabal-install}/bin:$PATH"
        export GHC="${pkgs.haskell.compiler.ghc96}/bin/ghc"
        export GHC_PACKAGE_PATH="${pkgs.haskell.compiler.ghc96}/lib/ghc-9.6.4/package.conf.d"
        export HIE_BIOS_GHC="${pkgs.haskell.compiler.ghc96}/bin/ghc"

        # Set up local project environment
        cabal update
        cabal configure
      '';
    };
  };
}
