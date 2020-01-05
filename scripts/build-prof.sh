#!/bin/sh

set -xue

# jump to project directory
cd "$(dirname $0)/../"

stack --work-dir .stack-work-profile build --executable-profiling --no-library-profiling
stack --work-dir .stack-work-profile exec  --executable-profiling --no-library-profiling \
   -- adventofcode-exe +RTS \
   -xc \
   -p  \
   -hy \
   -s  \
   --RTS $@

# see https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html
# -xc  -- Show current cost centre stack on raising an exception
# -p   -- Time/allocation profile in tree format
#         https://downloads.haskell.org/ghc/latest/docs/html/users_guide/profiling.html#time-and-allocation-profiling
# -hc  -- Produce a heap profile grouped by closure type
# -hy  -- Produce a heap profile grouped by type
# -T   -- enable GHC.Stats

# run profiteur, if installed
if [ -x "$(command -v profiteur)" ]; then
  profiteur adventofcode-exe.prof

  # open profiteur report in browser, if on macos
  if [[ $OSTYPE == darwin* ]]; then
    open adventofcode-exe.prof.html
  fi
fi

# visualize heap allocations
stack exec hp2ps -- -e8in -c adventofcode-exe.hp
open adventofcode-exe.ps
