#!/bin/sh

set -xue

# jump to project directory
cd "$(dirname $0)/../"

stack --work-dir .stack-work-profile build --executable-profiling --no-library-profiling
stack --work-dir .stack-work-profile exec  --executable-profiling --no-library-profiling \
   -- adventofcode-exe +RTS -p -T --RTS $@

# run profiteur, if installed
if [ -x "$(command -v profiteur)" ]; then
  profiteur adventofcode-exe.prof

  # open profiteur report in browser, if on macos
  if [[ $OSTYPE == darwin* ]]; then
    open adventofcode-exe.prof.html
  fi
fi

