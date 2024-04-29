#!/usr/bin/env bash
set -x
set -eo pipefail

runner_temp=$(mktemp -d)

export GHCUP_INSTALL_BASE_PREFIX=$runner_temp/foobarbaz
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_MINIMAL=1
export BOOTSTRAP_HASKELL_ADJUST_BASHRC=1

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

source $GHCUP_INSTALL_BASE_PREFIX/.ghcup/env || source ~/.bashrc

ghcup --version
which ghcup | grep foobarbaz

ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-prereleases-0.0.8.yaml

ghcup --metadata-caching=0 -v install ghc --set $VERSION
ghcup --metadata-caching=0 -v install cabal

ghc --version
ghc --info

cabal update
cabal build all
