let
  np2 = import ((import <nixpkgs> { }).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev =  "51d115ac89d676345b05a0694b23bd2691bf708a";
    sha256 = "1gfjaa25nq4vprs13h30wasjxh79i67jj28v54lkj4ilqjhgh2rs";
  }) { overlays = [(sel: sup: { allCabalHashes = newHashes sup; })]; };

  np = import ((import <nixpkgs> { }).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "0da76dab4c2acce5ebf404c400d38ad95c52b152";
    sha256 = "1lj3h4hg3cnxl3avbg9089wd8c82i6sxhdyxfy99l950i78j0gfg";
  }) { overlays = [(sel: sup: { allCabalHashes = newHashes sup; })]; };

  newHashes = super: super.fetchurl {
    url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/0b609355609de036fd5d0764e946e1ee33acda59.tar.gz";
    sha256 = "1qbzdngm4q8cmwydnrg7jvipw39nb1mjxw95vw6f789874002kn1";
  };

  mkGhc = (import (np.fetchFromGitHub {
    owner = "supersven";
    repo = "old-ghc-nix";
    rev = "093fe8273876ab1b746f6f3301a27aa2dd93ed75";
    sha256 = "186j36idapllxllwcw8svkzf8zvadvls2y4kw0mx4syc6ap4glrx";
  }) { pkgs = np2; }).mkGhc;

  # Required for https://github.com/haskell/cabal/pull/7183
  cabalFork = (np.fetchFromGitHub {
    owner = "bgamari";
    repo = "cabal";
    rev = "8a9621cdc6ffbe5c2c4512559e00c1b419e1e9cf";
    sha256 = "14jgpcvafibj285ywkkayy336xfpdj5akmj09rsrpmqdqymcwqwx";
  });

  hps = (np.haskellPackages.extend (self: super: rec {
    Cabal = np.haskell.lib.dontCheck (super.callCabal2nix "Cabal" "${cabalFork}/Cabal" {});
    random = np.haskell.lib.dontCheck (super.callHackage "random" "1.2.0" { });
    splitmix = np.haskell.lib.dontCheck super.splitmix_0_1_0_3;
    mkDerivation = args: super.mkDerivation (args // {
      doCheck = false;
    });
  }));

  fixedCabal = np.haskell.lib.dontCheck (hps.callCabal2nix "cabal-install" "${cabalFork}/cabal-install" { });

  # Cached artifacts do not last forever!
  # If this fails, please update url and hash for the lastest successful build
  # of wip/ghc-debug-ghc.
  ghc = mkGhc
        { url = "https://gitlab.haskell.org/ghc/ghc/-/jobs/612525/artifacts/raw/ghc-x86_64-fedora27-linux.tar.xz";
          hash = "05p90pnjvm95hdvj1gfi48lclp82fn80pbvipvm82bxsb6i5185m";
        };



  ghc-utils = import (np.fetchFromGitLab {
    domain = "gitlab.haskell.org";
    owner = "bgamari";
    repo = "ghc-utils";
    rev = "8ff33a41efdab01cb94e25648bc2a25864995466";
    sha256 = "0s745qvi26lbisz1y6zxdz0c10f69pvlrb87myv229f5k0vdp3vq";
  }) { nixpkgs = np; };

in
  np2.mkShell { buildInputs = [#ghc-utils
                               #ghc
                               fixedCabal
                               np.linuxPackages.perf
                               np2.ncurses
                               np.wget  # Used by cabal-install for https support when communicating with head.hackage
                               np2.zlib.dev
                               np2.zlib.out
                               np2.elfutils
                               np2.git
                               np2.numactl
                               np2.pkg-config
                               np2.graphviz

                               np2.gmp
                               np2.gmp.dev
                              ];

                # Export the location of the SSL CA bundle
                SSL_CERT_FILE = "${np.cacert}/etc/ssl/certs/ca-bundle.crt";
                NIX_SSL_CERT_FILE = "${np.cacert}/etc/ssl/certs/ca-bundle.crt";
                shellHook = ''
                  export LD_LIBRARY_PATH=${np2.gmp}/lib:${np2.zlib}/lib:${np2.ncurses}/lib:${np2.numactl}/lib
                '';
              }
