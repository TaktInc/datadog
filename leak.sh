#!/usr/bin/env bash
#
# Reproduces the memory leak. Assumes statsd running.

set -euo pipefail

stack build --profile --ghc-options="-rtsopts"
stack install
./bin/datadog-leak 10000 +RTS -L32 -hm
stack exec -- hp2ps datadog-leak.hp
open datadog-leak.ps
