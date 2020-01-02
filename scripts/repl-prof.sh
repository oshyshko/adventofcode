#!/bin/sh

set -xue

# jump to project directory
cd "$(dirname $0)/../"

stack --work-dir .stack-work-profile repl --ghci-options "-fexternal-interpreter -prof"
