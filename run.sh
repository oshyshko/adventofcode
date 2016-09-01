#!/bin/sh

set -ue

# jump to project directory
cd "$(dirname $0)"

cd src

stack runghc Main.hs
