#!/bin/sh

set -xue

# jump to project directory
cd "$(dirname $0)/../"

stack clean
stack build
