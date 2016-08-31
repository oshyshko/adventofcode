#!/bin/sh

set -ue

# jump to project directory
cd "$(dirname $0)"

cd src

echo "## Running Day01.hs..."
cat Day01.txt | stack runghc Day01.hs

echo "## Running Day02.hs..."
cat Day02.txt | stack runghc Day02.hs
