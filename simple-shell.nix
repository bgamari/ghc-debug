let
  _np = import <nixpkgs> { };

  np = import (_np.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "7d5375ebf4cd417465327d7ab453687fd19663c9";
    sha256 = "0vb7ikjscrp2rw0dfw6pilxqpjm50l5qg2x2mn1vfh93dkl2aan7";
  }) {};

  mkGhc = (import (np.fetchFromGitHub {
    owner  = "mpickering";
    repo   = "old-ghc-nix";
    rev    = "be3ca79e4e17cf7046110f14379307f3d348a702";
    sha256 = "1qz1lwk0j2r6jzpc5ssrlfcf1193ay5l23kwyda5wld5sm170ffw";
  }) { pkgs = np; }).mkGhc;

  ghc = mkGhc
        { url = "https://gitlab.haskell.org/ghc/ghc/-/jobs/150213/artifacts/raw/ghc-x86_64-fedora27-linux.tar.xz";
          hash = "02ympcnm33msdl4kv10nb1s44vc9mrsrgwgylis3l8ncr1564igy"; };

in
  _np.mkShell { buildInputs = [ ghc
                               np.ncurses
                               np.cabal-install
                             ]; }
