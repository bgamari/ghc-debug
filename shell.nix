let
  np = import ((import <nixpkgs> { }).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "5272327b81ed355bbed5659b8d303cf2979b6953";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {};

  mkGhc = (import (np.fetchFromGitHub {
    owner = "supersven";
    repo = "old-ghc-nix";
    rev = "093fe8273876ab1b746f6f3301a27aa2dd93ed75";
    sha256 = "186j36idapllxllwcw8svkzf8zvadvls2y4kw0mx4syc6ap4glrx";
  }) { pkgs = np; }).mkGhc;

  # Cached artifacts do not last forever!
  # If this fails, please update url and hash for the lastest successful build
  # of wip/ghc-debug.
  ghc = mkGhc
        { url = "https://gitlab.haskell.org/ghc/ghc/-/jobs/427334/artifacts/raw/ghc-x86_64-fedora27-linux.tar.xz";
          hash = "116dm1hqi7bmz4zh65miyhw2rzlv2ajlhrv6namfhgrdcj048gal";
        };
  ghc-utils = import ../ghc-utils {};

in
  np.mkShell { buildInputs = [ ghc
                               # ghc-utils
                               np.linuxPackages.perf
                               np.ncurses
                               np.wget  # Used by cabal-install for https support when communicating with head.hackage
                               np.cabal-install
                               np.zlib.dev
                               np.zlib.out
                               np.elfutils
                               np.git
                               np.numactl
                               np.pkg-config

                               np.gmp
                               np.gmp.dev
                              ];

                # Export the location of the SSL CA bundle
                SSL_CERT_FILE = "${np.cacert}/etc/ssl/certs/ca-bundle.crt";
                NIX_SSL_CERT_FILE = "${np.cacert}/etc/ssl/certs/ca-bundle.crt";
                shellHook = ''
                  export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${np.lib.makeLibraryPath [np.numactl np.gmp]}";
                  unset LD

                  echo 'Please build with --extra-include-dirs=${np.zlib.dev}/include/, e.g.:'
                  echo cabal 'new-build all --extra-include-dirs=${np.zlib.dev}/include/'
                '';
              }
