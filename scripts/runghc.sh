#!/bin/sh

set -xue

# jump to project directory
cd "$(dirname $0)/../"

stack runghc -- -isrc src/Main.hs
