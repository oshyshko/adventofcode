#!/bin/sh

set -xue

# jump to project directory
cd "$(dirname $0)/../"

# To select which tests to run, use:
# - in code         - use `fdescribe`, `fit` and `focus`
# - in command line - use ./scripts/auto-test.sh '--match "/Y21.D22/"'

# taken from https://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html
ghcid -W \
    --command "stack ghci adventofcode:lib adventofcode:test:spec" \
    --test "Main.main" \
    --setup ":set args $@"
