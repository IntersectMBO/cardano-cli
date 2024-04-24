#!/usr/bin/env bash
#
# Runs "cabal check" in all directories containing a versioned .cabal file

for cabal_file in $(git ls-files "*.cabal")
do
  cd "$(dirname "$cabal_file")" || { echo "Cannot cd"; exit 1; }
  echo "$(pwd)> cabal-check"
  cabal check
  cd - || { echo "Cannot cd back"; exit 1; }
done
