#!/bin/sh

set -xue

# jump to project directory
cd "$(dirname $0)/../"

cd src

stack build
stack exec adventofcode-exe
