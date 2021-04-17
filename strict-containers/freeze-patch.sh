#!/bin/sh
# Update a patch being edited from the working tree, then freeze it so you can
# commit the changes, or edit another patch.
#
set -e

which quilt >/dev/null 2>&1 || { echo >&2 "you need to install quilt(1)"; exit 1; }
[ -e patches/series ] || { echo >&2 "not currently editing a patch"; exit 1; }

quilt --quiltrc=.quiltrc refresh

rm -rf .pc patches/series
echo "Patch frozen; quilt deactivated"
