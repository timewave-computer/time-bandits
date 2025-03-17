# shell.nix
{ pkgs ? import <nixpkgs> { } }:

let
  # Use GHC 9.6.3 for compatibility with the current project
  hsPkgs = pkgs.haskell.packages.ghc963;

  # Create a Haskell environment with the packages we need
  haskellDeps = ps: with ps; [
    cabal-install
    polysemy
    polysemy-plugin
    doctest
    aeson
    bytestring
    containers
    cryptonite
    memory
    mtl
    text
    time
    transformers
    with-utf8
  ];

  # Create a custom GHC with the packages we need
  ghc = hsPkgs.ghcWithPackages haskellDeps;

in
pkgs.mkShell {
  buildInputs = [
    ghc
    hsPkgs.cabal-install
    hsPkgs.haskell-language-server
    hsPkgs.doctest
    pkgs.ghcid
  ];

  # Add any system libraries needed
  nativeBuildInputs = with pkgs; [
    zlib
  ];

  # Set up the environment
  shellHook = ''
    echo "Entering Time Bandits development environment..."
    export NIX_GHC="${ghc}/bin/ghc"
    export NIX_GHCPKG="${ghc}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${ghc}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR="${ghc}/lib/ghc-9.6.3"
    export PATH="$PWD/dist/build/time-bandits:$PATH"
  '';
}
