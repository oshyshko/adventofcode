#!/bin/sh

set -xue

# jump to project directory
cd "$(dirname $0)/../"

cd src

stack exec adventofcode-exe
