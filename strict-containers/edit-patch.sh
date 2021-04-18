#!/bin/bash
# Reverses a patch then applies it using quilt(1) for easy refreshing later.
#
# Run this on a given patch, then edit the working tree as normal.
#
# When you are done, run `./freeze-patch.sh` (no args required) to update the
# patch file with your working tree changes.
#
set -e
shopt -s globstar

which quilt >/dev/null 2>&1 || { echo >&2 "you need to install quilt(1)"; exit 1; }
test -n "$1" || { echo >&2 "Usage: $0 <patch basename>"; exit 2; }
[ ! -e patches/series ] || { echo >&2 "already editing a patch ($(echo $(cat patches/series))), freeze it first."; exit 1; }
patch="$(basename "$1")"

echo "Reversing patch $patch"
patch -s -r- -p1 -R < "patches/$patch"
echo "$patch" > patches/series

quilt_() {
  quilt "$@" && local x=$? || local x=$?
  case $x in
   2 ) return 0;; # quilt is a special snowflake that ignores unix exit code convention
   * ) return $x;;
  esac
}

quilt --quiltrc=.quiltrc push -q "$patch"

if [ "${patch%.patch}" = "tests" ]; then
  quilt_ add tests/**/*.hs
else
  if [ -f "src/Data/Strict/${patch%.patch}.hs" ]; then
    quilt_ add "src/Data/Strict/${patch%.patch}.hs"
  fi
  if [ -d "src/Data/Strict/${patch%.patch}" ]; then
    quilt_ add "src/Data/Strict/${patch%.patch}"/**/*.hs
  fi
fi
