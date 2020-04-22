let
  _np = import <nixpkgs> { };

  np = import (_np.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "541d9cce8af7a490fb9085305939569567cb58e6";
    sha256 = "0jgz72hhzkd5vyq5v69vpljjlnf0lqaz7fh327bvb3cvmwbfxrja";
  }) {};

  mkGhc = (import (np.fetchFromGitHub {
    owner  = "mpickering";
    repo   = "old-ghc-nix";
    rev    = "be3ca79e4e17cf7046110f14379307f3d348a702";
    sha256 = "1qz1lwk0j2r6jzpc5ssrlfcf1193ay5l23kwyda5wld5sm170ffw";
  }) { pkgs = np; }).mkGhc;

  # This archive will be gone on 20th May. Please update when a new build of the
  # `wip/ghc-debug` branch becomes available (which will hold for another month).
  ghc = mkGhc
        { url = "https://gitlab.haskell.org/ghc/ghc/-/jobs/312946/artifacts/raw/ghc-x86_64-fedora27-linux.tar.xz";
          hash = "1gw3q6g059yz0lqd9x3r23isghdwqw5zimysm73pp26jdg3rsc2w";
        };

in
  _np.mkShell { buildInputs = [ ghc
                               np.ncurses
                               np.cabal-install
                               np.zlib
                               np.elfutils
                               np.git
                              ];
              }
