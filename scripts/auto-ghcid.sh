#!/bin/sh

# Usage: add a comment like below into a source file being worked on.
#        Edit file, save => see result.
#
# -- $> readInput "Y23.D18" <&> Y23.D18.solve8

set -xue

# jump to project directory
cd "$(dirname $0)/../"

# taken from https://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html
ghcid \
    --command "stack ghci adventofcode:lib adventofcode:adventofcode-exe" \
    --test "putStrLn \"\"" \
    --test-message=""