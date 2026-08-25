#!/bin/sh
# Vendors oneTBB, following this package's convention for bundled libraries
# (see src/sundials, inst/include/{stan,walnutpie,...}): public headers go
# in inst/include (installed with the package), implementation sources go
# directly under src/ (compiled by src/Makevars, not installed). Only
# src/tbb and src/tbbmalloc are needed -- no cmake -- see src/Makevars.
set -e

ONETBB_VERSION=2022.0.0
ONETBB_DIR=oneTBB-$ONETBB_VERSION
ONETBB_TARBALL=v$ONETBB_VERSION.tar.gz
ONETBB_URL=https://github.com/oneapi-src/oneTBB/archive/refs/tags/$ONETBB_TARBALL

if [ ! -f "$ONETBB_TARBALL" ]; then
  wget $ONETBB_URL
fi
rm -rf "$ONETBB_DIR"
tar -xf "$ONETBB_TARBALL"

INST_INCLUDE=../inst/include
SRC=../src

rm -rf "$INST_INCLUDE/oneapi" "$INST_INCLUDE/tbb" "$SRC/tbb" "$SRC/tbbmalloc" "$SRC/tbbmalloc_proxy"

cp -Rf "$ONETBB_DIR/include/oneapi" "$INST_INCLUDE/"
cp -Rf "$ONETBB_DIR/include/tbb" "$INST_INCLUDE/"

cp -Rf "$ONETBB_DIR/src/tbb" "$SRC/"
cp -Rf "$ONETBB_DIR/src/tbbmalloc" "$SRC/"

# Customize.h includes this header just for its macros; nothing else in
# tbbmalloc_proxy is needed.
mkdir -p "$SRC/tbbmalloc_proxy"
cp -f "$ONETBB_DIR/src/tbbmalloc_proxy/proxy.h" "$SRC/tbbmalloc_proxy/"

# Only used by oneTBB's own CMake build.
rm -rf "$SRC/tbb/def" "$SRC/tbbmalloc/def"
rm -f "$SRC/tbb/tbb.rc" "$SRC/tbbmalloc/tbbmalloc.rc"
rm -f "$SRC/tbb/CMakeLists.txt" "$SRC/tbbmalloc/CMakeLists.txt"

# RTM/TSX speculative-lock fast path: src/Makevars never passes -mrtm (it
# trips R CMD check's non-portable-flags NOTE), so these are never compiled.
rm -f "$SRC/tbb/rtm_mutex.cpp" "$SRC/tbb/rtm_rw_mutex.cpp"

sed -i.bak \
  -e 's/#define __TBB_WAITPKG_INTRINSICS_PRESENT (/#define __TBB_WAITPKG_INTRINSICS_PRESENT (0 \&\& (/' \
  -e '/__TBB_x86_32 || __TBB_x86_64/ s/\&\& !__ANDROID__)/\&\& !__ANDROID__))/' \
  "$INST_INCLUDE/oneapi/tbb/detail/_config.h"
rm -f "$INST_INCLUDE/oneapi/tbb/detail/_config.h.bak"

rm -f "$SRC/tbb/itt_notify.cpp"
rm -rf "$SRC/tbb/tools_api"

# Keep stanr's dynamically linked TBB private from other R packages. These
# names must agree with src/Makevars.tbb on every supported native platform.
for file in "$SRC/tbb/allocator.cpp" "$SRC/tbbmalloc/tbbmalloc.cpp"; do
  sed -i.bak \
    -e 's/"tbbmalloc" DEBUG_SUFFIX "\.dll"/"stanr_tbbmalloc" DEBUG_SUFFIX ".dll"/' \
    -e 's/"libtbbmalloc"/"libstanr_tbbmalloc"/g' \
    -e 's/DEBUG_SUFFIX "\.2\.dylib"/DEBUG_SUFFIX ".dylib"/' \
    "$file"
  rm -f "$file.bak"
done


sed -i.bak 's#\.\./src/tbb/environment\.h#../tbb/environment.h#' "$SRC/tbbmalloc/large_objects.cpp"
rm -f "$SRC/tbbmalloc/large_objects.cpp.bak"


sed -i.bak \
  -e 's/\*backRefBl\[1\];   /*backRefBl[];    /' \
  -e 's/= 1+(BackRefMain::bytes-sizeof(BackRefMain))/= (BackRefMain::bytes-sizeof(BackRefMain))/' \
  "$SRC/tbbmalloc/backref.cpp"
rm -f "$SRC/tbbmalloc/backref.cpp.bak"

# arena::allocation_size() already sizes from sizeof(base_type) rather than
# sizeof(arena), so my_slots needs no arithmetic fix -- only the assertion that
# spelled out the old layout (compiled out anyway under -DNDEBUG).
sed -i.bak 's/arena_slot my_slots\[1\];/arena_slot my_slots[];/' "$SRC/tbb/arena.h"
rm -f "$SRC/tbb/arena.h.bak"
sed -i.bak 's/sizeof(base_type) + sizeof(arena_slot) == sizeof(arena)/sizeof(base_type) == sizeof(arena)/' "$SRC/tbb/arena.cpp"
rm -f "$SRC/tbb/arena.cpp.bak"

# R CMD check flags non-portable diagnostic-suppression pragmas.
files_list=(
  "$SRC/tbb/co_context.h"
  "$SRC/tbb/concurrent_monitor.h"
  "$SRC/tbbmalloc/tbbmalloc_internal.h"
)

for file in "${files_list[@]}"; do
  if [ -f "$file" ]; then
    sed -i.bak \
      -e '/#pragma clang diagnostic/d' \
      -e '/#pragma GCC diagnostic/d' \
      -e '/#pragma warning( *push *)/d' \
      -e '/#pragma warning( *disable *:/d' \
      -e '/#pragma warning( *pop *)/d' \
      -e '/#pragma warning push/d' \
      -e '/#pragma warning disable/d' \
      "$file"
    rm -f "$file.bak"
  fi
done
