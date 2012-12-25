#!/bin/bash -e
cabal configure --enable-executable-profiling
cabal build > build.out
echo "Running profiled program..."
time `dist/build/blokus/blokus +RTS -hd -p -sblokus.summary >&2`
