#!/bin/bash
# Regenerate files from submodules.
# Set CLEAN=1 to delete generated files, but don't regenerate them.
# Set NOPATCH=1 to not apply patches.

set -e
shopt -s nullglob globstar

ensure_checkout() {
	local pkg="$1"
	local ver="$2"
	( cd ../contrib/"$pkg" && git checkout "$ver" )
	echo "  * $pkg $ver" >> "$VERSIONS_CABAL"
}

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
	if [ "$#" -lt 4 ]; then shift 3; else shift 4; fi

	local path_l="Data/Strict/$type/Autogen"

	rm -rf "src/${path_l}.hs" "src/${path_l}"
	mkdir -p "src/${path_l}"
	cat /dev/null | fixup_cabal "$type"
	test -z "$CLEAN" || return 0

	local includes=( "${@:-*.hs}" )
	local findexpr=( -false ); for((i=0;i<${#includes[@]};i++)); do findexpr+=( -o -path "./${path_r}/${includes[i]}" ); done
	#echo >&2 cd ../contrib/"$pkg/" '&&' find . "${findexpr[@]}"
	( cd ../contrib/"$pkg/" && find . "${findexpr[@]}" ) | while read found; do
		local f="${found#./${path_r}/}"
		mkdir -p "$(dirname "src/${path_l}/$f")"
		#echo >&2 "${f}"
		cp -a ../contrib/"$pkg/${path_r}/$f" "src/${path_l}/$f"
	done
	if [ -f ../contrib/"$pkg/${path_r}.hs" ]; then
		cp -a ../contrib/"$pkg/${path_r}.hs" "src/${path_l}.hs"
	fi
	for i in $excludes; do
		rm -f "src/${path_l}$i"
	done
	rename_modules "$path_r" "$path_l" "src/${path_l}.hs"* "src/${path_l}"/**/*.hs
	( cd src && find "${path_l}.hs" "${path_l}" -type f 2>/dev/null | sed -e 's/.hs$//g' -e 's,/,.,g' ) | fixup_cabal "$type"
	if [ -z "$NOPATCH" ]; then patch -p1 < "patches/$type.patch"; fi
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
	cp -d --preserve=all "../contrib/$pkg/$test" "$TESTDIR"/
	rename_modules "$path_r" "$path_l" "$TESTDIR"/"$(basename "$test")"
	cat "../contrib/$pkg"/*.cabal | \
	  get_section "^[a-zA-Z][-a-zA-Z]* \?" "$testname" |
	  sed -re 's,hs-source-dirs:( *)tests,hs-source-dirs:\1'"$TESTDIR"',g' >> "$TESTS_CABAL"
}

if [ -z "$CLEAN" ]; then
	VERSIONS_CABAL=versions.cabal.in
	rm -f $VERSIONS_CABAL
	ensure_checkout containers v0.6.4.1
	ensure_checkout unordered-containers v0.2.13.0
	ensure_checkout vector v0.12.3.0
	cat $VERSIONS_CABAL | fixup_cabal versions ""
	rm -f $VERSIONS_CABAL
else
	cat /dev/null | fixup_cabal versions
fi

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
copy_and_rename containers/containers/src IntMap Data/IntMap ".hs /Lazy.hs /Merge/Lazy.hs /Lazy/Internal.hs"
rename_modules Utils/Containers/Internal Data/Strict/ContainersUtils/Autogen \
  src/Data/Strict/IntMap/Autogen.hs* src/Data/Strict/IntMap/Autogen/**/*.hs
copy_and_rename containers/containers/src Sequence Data/Sequence "/Internal/sorting.md"
rename_modules Utils/Containers/Internal Data/Strict/ContainersUtils/Autogen \
  src/Data/Strict/Sequence/Autogen.hs* src/Data/Strict/Sequence/Autogen/**/*.hs
copy_and_rename vector Vector Data/Vector "" Mutable.hs

TESTDIR=tests

rm -rf "$TESTDIR" && mkdir -p "$TESTDIR"
if [ -z "$CLEAN" ]; then
	cp -a ../contrib/containers/containers-tests/tests/Utils "$TESTDIR"
	TESTS_CABAL=tests.cabal.in
	rm -f "$TESTS_CABAL"
	copy_test_and_rename containers/containers-tests tests/map-properties.hs map-strict-properties Data/Map Data/Strict/Map/Autogen
	copy_test_and_rename containers/containers-tests tests/map-strictness.hs map-strictness-properties Data/Map Data/Strict/Map/Autogen
	copy_test_and_rename containers/containers-tests tests/intmap-properties.hs intmap-strict-properties Data/IntMap Data/Strict/IntMap/Autogen
	copy_test_and_rename containers/containers-tests tests/intmap-strictness.hs intmap-strictness-properties Data/IntMap Data/Strict/IntMap/Autogen
	copy_test_and_rename containers/containers-tests tests/seq-properties.hs seq-properties Data/Sequence Data/Strict/Sequence/Autogen
	cp -a ../contrib/containers/containers-tests/tests/IntMapValidity.hs "$TESTDIR"
	rename_modules Data/IntMap Data/Strict/IntMap/Autogen "$TESTDIR"/IntMapValidity.hs
	rename_modules Utils/Containers/Internal Data/Strict/ContainersUtils/Autogen "$TESTDIR"/IntMapValidity.hs
	copy_test_and_rename unordered-containers tests/HashMapProperties.hs hashmap-strict-properties Data/HashMap Data/Strict/HashMap/Autogen
	copy_test_and_rename vector tests/Main.hs vector-tests-O0 XXX XXX
	mv "$TESTDIR"/Main.hs "$TESTDIR"/VectorMain.hs
	cp -a ../contrib/vector/tests/{Tests,Boilerplater.hs,Utilities.hs} "$TESTDIR"
	rm -f "$TESTDIR"/Tests/Vector/{Primitive,Unboxed,Storable}.hs

	cat "$TESTS_CABAL" | fixup_cabal tests ""
	rm -f "$TESTS_CABAL"
	if [ -z "$NOPATCH" ]; then patch -p1 < "patches/tests.patch"; fi
else
	cat /dev/null | fixup_cabal tests ""
fi
