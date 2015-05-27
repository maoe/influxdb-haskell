#!/bin/bash

eval "$(halcyon paths)"
run-cabal-test --cabal-name=cabal --show-details=always
cabal run influx-random-points -- 10 10
cabal check
cabal sdist
export SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}')
pushd dist/
if [ -f "$SRC_TGZ" ]; then
  cabal install "$SRC_TGZ"
else
  echo "expected '$SRC_TGZ' not found"
  exit 1
fi
popd
