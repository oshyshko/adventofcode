#!/bin/sh

set -ue

# jump to project directory
cd "$(dirname $0)"

cd src

# TODO loop over *.hs

echo "## Running Day01.hs..."
stack runghc Day01.hs

echo "## Running Day02.hs..."
stack runghc Day02.hs
