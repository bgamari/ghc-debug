{ci ? false, haskellCompiler ? "ghc8102" }:
let
  # Import the Haskell.nix library,
  haskell-src = import ((import ./nix/sources.nix)."haskell.nix") {};
  npSrc = haskell-src.sources.nixpkgs-2009;
  npArgs = haskell-src.nixpkgsArgs;
  pin = import npSrc npArgs;

  haskell = pin.haskell-nix;

  ciOptions = []; #[ { packages.eventlog2html.configureFlags = [ "--ghc-option=-Werror" ]; } ];

  opts = [ { packages.vault.doHaddock = false; } ];

  # Instantiate a package set using the generated file.
  pkgSet = haskell.cabalProject {
    compiler-nix-name = haskellCompiler;
    src = ./site;  #haskell.haskellLib.cleanSource { name = "ghc-debug-site"; src = ./site; };
    modules = (if ci then ciOptions else []) ++ opts;
    index-state = "2020-12-11T00:00:00Z";
    plan-sha256 = "04xkvgbxh5rpfmcd0mali9qw08yqskhcxvhbi3hf6lzn69sz9iw2";
  };


  site = import ./nix/site.nix { nixpkgs = pin; hspkgs = pkgSet; };

in
  { site = site;
  }
