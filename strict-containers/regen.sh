#!/bin/bash
# Regenerate files from submodules.
# Set CLEAN=1 to delete generated files, but don't regenerate them.

set -e
shopt -s nullglob globstar

( cd ../contrib/containers && git checkout v0.6.4.1 )
( cd ../contrib/unordered-containers && git checkout v0.2.13.0 )

fixup_cabal() {
	local type="$1"
	local prefix="${2-    }"
	local header="-- DO NOT EDIT below, AUTOGEN $type"
	local footer="-- DO NOT EDIT above, AUTOGEN $type"
	{
	echo "$header"
	cat
	echo "$footer"
	} | sed -e 's/^/'"$prefix"'/g' > "$type.list"
	sed -i -e "/$header/,/$footer/d" -e '/-- generated list for '"$type"'/r'"$type.list" strict-containers.cabal
	rm -f "$type.list"
}

rename_modules() {
	test -z "$CLEAN" || return 0
	local path_r="$1"
	local path_l="$2"
	shift 2

	local mod_r="$(echo "${path_r}" | sed -e 's,/,\.,g')"
	local mod_l="$(echo "${path_l}" | sed -e 's,/,.,g')"
	sed -e 's/'"${mod_r}"'/'"${mod_l}"'/g' -i "$@"
}

copy_and_rename() {
	local pkg="$1"
	local type="$2"
	local path_r="$3"
	local excludes="$4"

	local path_l="Data/Strict/$type/Autogen"

	rm -rf "src/${path_l}.hs" "src/${path_l}"
	mkdir -p "src/${path_l}"
	cat /dev/null | fixup_cabal "$type"
	test -z "$CLEAN" || return 0

	cp -a ../contrib/"$pkg/${path_r}"/* "src/${path_l}/"
	if [ -f ../contrib/"$pkg/${path_r}.hs" ]; then
		cp -a ../contrib/"$pkg/${path_r}.hs" "src/${path_l}.hs"
	fi
	for i in $excludes; do
		rm -f "src/${path_l}$i"
	done
	rename_modules "$path_r" "$path_l" "src/${path_l}.hs"* "src/${path_l}"/**/*.hs
	( cd src && find "${path_l}.hs" "${path_l}" -type f 2>/dev/null | sed -e 's/.hs$//g' -e 's,/,.,g' ) | fixup_cabal "$type"
	patch -p1 < "patches/$type.patch"
}

get_section() {
	# $1: regexp to indicate start of section
	# $2: regexp to indicate section name, after start-section regexp
	sed -n -e '/'"$1""$2"'/,/'"$1"'/{
	  s/'"$1""$2"'/\0/;                       tTHEN;
	  :ELSE;x;p;x;${/'"$1"'/!p};              bENDIF;
	  :THEN;$p;
	  :ENDIF;
	  };h;'
	# corner cases all handled :)
}

copy_test_and_rename() {
	local pkg="$1"
	local test="$2"
	local testname="$3"
	local path_r="$4"
	local path_l="$5"
	cp -d --preserve=all "../contrib/$pkg/$test" tests/
	rename_modules "$path_r" "$path_l" tests/"$(basename "$test")"
	cat "../contrib/$pkg"/*.cabal | \
	  get_section "test-suite " "$testname" | \
	  sed -e 's/'"$(basename "$pkg")"'/strict-containers/g' >> "$TESTS_CABAL"
}

copy_and_rename unordered-containers HashMap Data/HashMap "/Lazy.hs /Internal/Lazy.hs"
rm -rf include && mkdir -p include
if [ -z "$CLEAN" ]; then
	cp -a ../contrib/containers/containers/include/* include/
	find include -type f | fixup_cabal includes
else
	cat /dev/null | fixup_cabal includes
fi
copy_and_rename containers/containers/src ContainersUtils Utils/Containers/Internal
copy_and_rename containers/containers/src Map Data/Map ".hs /Lazy.hs /Merge/Lazy.hs /Lazy/Internal.hs"
rename_modules Utils/Containers/Internal Data/Strict/ContainersUtils/Autogen \
  src/Data/Strict/Map/Autogen.hs* src/Data/Strict/Map/Autogen/**/*.hs
copy_and_rename containers/containers/src Sequence Data/Sequence "/Internal/sorting.md"
rename_modules Utils/Containers/Internal Data/Strict/ContainersUtils/Autogen \
  src/Data/Strict/Sequence/Autogen.hs* src/Data/Strict/Sequence/Autogen/**/*.hs

rm -rf tests && mkdir -p tests
if [ -z "$CLEAN" ]; then
	cp -a ../contrib/containers/containers-tests/tests/Utils tests/
	rm -f tests.cabal.in
	export TESTS_CABAL=tests.cabal.in
	copy_test_and_rename containers/containers-tests tests/map-properties.hs map-strict-properties Data/Map Data/Strict/Map/Autogen
	copy_test_and_rename containers/containers-tests tests/map-strictness.hs map-strictness-properties Data/Map Data/Strict/Map/Autogen
	copy_test_and_rename containers/containers-tests tests/seq-properties.hs seq-properties Data/Sequence Data/Strict/Sequence/Autogen
	copy_test_and_rename unordered-containers tests/HashMapProperties.hs hashmap-strict-properties Data/HashMap Data/Strict/HashMap/Autogen
	cat tests.cabal.in | fixup_cabal tests ""
	export -n TESTS_CABAL
	rm -f tests.cabal.in
	# some containers tests depend on other structures like set, lazy map
	sed -i -e 's/test-suite map-strict-properties/test-suite map-strict-properties\
  cpp-options: -DSTRICT\
  build-depends: containers/g' -e 's/test-suite map-strictness-properties/test-suite map-strictness-properties\
  build-depends: containers/g' strict-containers.cabal
	patch -p1 < "patches/tests.patch"
else
	cat /dev/null | fixup_cabal tests ""
fi
