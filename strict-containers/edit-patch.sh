#!/bin/sh
# Reverses a patch then applies it using quilt(1) for easy refreshing later.
#
# Run this on a given patch, then edit the working tree as normal.
#
# To add files to a patch being edited, run "quilt add <files>". You must do
# this *before* editing the file, or else `./freeze-patch.sh` won't work later.
#
# When you are done, run `./freeze-patch.sh` (no args required) to update the
# patch file with your working tree changes.
#
set -e

which quilt >/dev/null 2>&1 || { echo >&2 "you need to install quilt(1)"; exit 1; }
test -n "$1" || { echo >&2 "Usage: $0 <patch basename>"; exit 2; }
[ ! -e patches/series ] || { echo >&2 "already editing a patch ($(echo $(cat patches/series))), freeze it first."; exit 1; }
patch="$(basename "$1")"

echo "Reversing patch $patch"
patch -s -r- -p1 -R < "patches/$patch"
echo "$patch" > patches/series

quilt --quiltrc=.quiltrc push -q "$patch"
