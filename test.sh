#!/bin/bash -e
cabal configure --enable-test
cabal build
cabal test
