let

  np = import ((import <nixpkgs> { }).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "0da76dab4c2acce5ebf404c400d38ad95c52b152";
    sha256 = "1lj3h4hg3cnxl3avbg9089wd8c82i6sxhdyxfy99l950i78j0gfg";
  }) { };

  ghcs = import (np.fetchFromGitLab { owner = "bgamari";
                           repo = "ghcs-nix";
                           rev="c58f13f1f0c449ba027d8b36efc8a5f0719f8619";
                           sha256 = "18n9w8l76ymrxhp9w0fwd3f5v48aqvy1mlvvs9lzgq00qms0xd0p";
                           domain = "gitlab.haskell.org"; });
in
  np.mkShell { buildInputs = [ ghcs.cabal-install ghcs.ghc-9_2_4 ghcs.ghc-9_4_2 ];
              }
