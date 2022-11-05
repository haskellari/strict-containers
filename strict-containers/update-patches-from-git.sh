#!/bin/sh
# This is useful when "rebasing" patches. Rough workflow:
#
# - git checkout -b refresh-patches @ && \
#   NOPATCH=1 ./regen.sh && \
#   git commit -m "revert patches" -a && \
#   ./regen.sh && \
#   git commit -m "apply patches" -a && \
#   git checkout -b update-base @~
#
# - # hack hack hack, keep running NOPATCH=1 ./regen.sh, make updates until it works
#
# - CLEAN=1 NOPATCH=1 ./regen.sh && \
#   git checkout src tests include *.cabal && \
#   git commit -m "update versions" -a && \
#   NOPATCH=1 ./regen.sh && \
#   git commit -m "regen" -a && \
#   git checkout refresh-patches && \
#   git rebase update-base
#
# - # hack hack hack, fix merge conflicts with git mergetool, fix any build errors
#
# - git rebase --continue
# - git checkout -b update-patches update-base~ && \
#   ./update-patches-from-git.sh refresh-patches && \
#   git commit -m "update patches" patches && \
#   git branch -D update-base refresh-patches && \
#   ./regen.sh
#
# - # hack hack hack, build, test, commit, etc

set -e

update_patch() {
  local commit="$1"
  local name="$2"
  shift 2
  sed -ne '/^---/!p;//q' "patches/$name.patch" > "patches/$name.patch.tmp"
  git show --format= --relative=strict-containers "$commit" -- "$@" >> "patches/$name.patch.tmp"
  mv "patches/$name.patch.tmp" "patches/$name.patch"
}

update_patch "$1" tests tests/ strict-containers.cabal
for i in ContainersUtils HashMap IntMap Map Sequence Vector; do
  update_patch "$1" "$i" "src/Data/Strict/$i"
done
