#!/usr/bin/env bash
#
# Reproduces the memory leak. Assumes statsd running.

rm -f *.ps *.hp
stack build --profile --ghc-options="-rtsopts"
stack install
./bin/datadog-leak 100000 +RTS -L32 -hm
stack exec -- hp2ps datadog-leak.hp
