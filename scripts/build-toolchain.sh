#!/bin/bash
# build-toolchain.sh - Build the Fuse toolchain for Linux x86_64 or macOS ARM64
#
# This script builds a self-contained toolchain bundle containing:
# - Fuse compiler (built with Scala Native)
# - GRIN compiler (built with Nix)
# - LLVM 15 (clang, opt, llc) - pre-built binaries
# - Boehm GC v8.0.6 - built from source
# - Runtime files (runtime.c, prim_ops.c, prim_ops.h)
#
# Usage:
#   ./scripts/build-toolchain.sh                    # Auto-detect platform
#   ./scripts/build-toolchain.sh --grin-ref boehm-gc # Specify GRIN version
#   ./scripts/build-toolchain.sh --output-dir ./out # Specify output directory
#   ./scripts/build-toolchain.sh --skip-grin /path  # Use pre-built GRIN
#   ./scripts/build-toolchain.sh --skip-fuse        # Skip Fuse compiler build
#
# Linux builds use Docker (manylinux2014) for maximum compatibility.
# macOS builds run natively.

set -euo pipefail

# =============================================================================
# Constants
# =============================================================================

LLVM_VERSION_LINUX="15.0.6"
LLVM_VERSION_MACOS="15.0.7"
GC_VERSION="v8.0.6"
DEFAULT_GRIN_REF="boehm-gc"
DOCKER_IMAGE="quay.io/pypa/manylinux2014_x86_64"

# LLVM download URLs (15.0.7 doesn't have Linux x86_64 binary, use 15.0.6)
LLVM_URL_LINUX="https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VERSION_LINUX}/clang+llvm-${LLVM_VERSION_LINUX}-x86_64-linux-gnu-ubuntu-18.04.tar.xz"
LLVM_URL_MACOS="https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VERSION_MACOS}/clang+llvm-${LLVM_VERSION_MACOS}-arm64-apple-darwin22.0.tar.xz"

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# =============================================================================
# Utility Functions
# =============================================================================

say() {
    printf '[build-toolchain] %s\n' "$1"
}

err() {
    say "ERROR: $1" >&2
    exit 1
}

need_cmd() {
    if ! command -v "$1" > /dev/null 2>&1; then
        err "need '$1' (command not found)"
    fi
}

get_parallelism() {
    case "$(uname -s)" in
        Linux)  nproc ;;
        Darwin) sysctl -n hw.ncpu ;;
        *)      echo 4 ;;
    esac
}

# =============================================================================
# Platform Detection
# =============================================================================

detect_platform() {
    local os arch
    os="$(uname -s)"
    arch="$(uname -m)"

    case "$os-$arch" in
        Linux-x86_64)  echo "linux-x86_64" ;;
        Darwin-arm64)  echo "macos-arm64" ;;
        Darwin-x86_64) echo "macos-x86_64" ;;
        *)             err "Unsupported platform: $os-$arch" ;;
    esac
}

# =============================================================================
# Build Functions
# =============================================================================

build_grin() {
    local grin_ref="$1"
    local output_bin="$2"
    local toolchain_dir="$(dirname "$(dirname "$output_bin")")"

    say "Building GRIN from github:fuselang/grin/${grin_ref}..."
    need_cmd nix

    # Use experimental features for flakes support
    nix --extra-experimental-features "nix-command flakes" \
        build "github:fuselang/grin/${grin_ref}" --no-link --print-out-paths > /tmp/grin-result
    local grin_path
    grin_path="$(cat /tmp/grin-result)/bin/grin"

    if [ ! -f "$grin_path" ]; then
        err "GRIN build failed: $grin_path not found"
    fi

    mkdir -p "$(dirname "$output_bin")"
    cp "$grin_path" "$output_bin"
    chmod +x "$output_bin"

    # Fix library paths - copy Nix store dependencies and rewrite references
    fix_nix_binary_paths "$output_bin" "$toolchain_dir"

    say "GRIN built successfully: $output_bin"
    "$output_bin" --help | head -3
}

fix_nix_binary_paths() {
    local binary="$1"
    local toolchain_dir="$2"
    local lib_dir="${toolchain_dir}/lib"

    mkdir -p "$lib_dir"

    case "$(uname -s)" in
        Darwin)
            # Bundle Nix store dependencies recursively
            bundle_nix_libs_darwin "$binary" "$lib_dir"

            # Also check bundled libraries for transitive Nix dependencies
            for bundled_lib in "$lib_dir"/*.dylib; do
                [ -f "$bundled_lib" ] || continue
                bundle_nix_libs_darwin "$bundled_lib" "$lib_dir"
            done

            # Add rpath for good measure
            install_name_tool -add_rpath @loader_path/../lib "$binary" 2>/dev/null || true

            # Re-sign binary after modifications (required on macOS)
            codesign --force --sign - "$binary" 2>/dev/null || true
            ;;
        Linux)
            need_cmd patchelf
            # Bundle Nix store dependencies recursively (excludes glibc)
            bundle_nix_libs_linux "$binary" "$lib_dir"

            # Also check bundled libraries for transitive Nix dependencies
            for bundled_lib in "$lib_dir"/*.so*; do
                [ -f "$bundled_lib" ] || continue
                bundle_nix_libs_linux "$bundled_lib" "$lib_dir"
            done

            # Remove any glibc libraries that may have been accidentally bundled
            # These MUST use the system versions to match the system dynamic linker
            say "Removing any accidentally bundled glibc libraries..."
            for glibc_lib in libc.so* libm.so* libdl.so* librt.so* libpthread.so* ld-linux*.so* libgcc_s.so*; do
                rm -f "$lib_dir"/$glibc_lib 2>/dev/null || true
            done

            # Fix bundled libraries to use relative rpath
            for bundled_lib in "$lib_dir"/*.so*; do
                [ -f "$bundled_lib" ] || continue
                [ -L "$bundled_lib" ] && continue  # Skip symlinks
                chmod +w "$bundled_lib" 2>/dev/null || true
                patchelf --set-rpath '$ORIGIN' "$bundled_lib" 2>/dev/null || true
            done

            # Make binary writable and fix paths
            chmod +w "$binary"
            # Set interpreter to system ld-linux (Nix binary has Nix store interpreter)
            patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 "$binary"
            # Set rpath to look relative to binary for bundled libs
            patchelf --set-rpath '$ORIGIN/../lib' "$binary"

            # Verify the fix worked
            local actual_interp
            actual_interp="$(patchelf --print-interpreter "$binary")"
            if [ "$actual_interp" != "/lib64/ld-linux-x86-64.so.2" ]; then
                err "Failed to set interpreter. Got: $actual_interp"
            fi
            say "Interpreter set to: $actual_interp"

            # Verify no Nix store references remain
            local nix_refs
            nix_refs="$(ldd "$binary" 2>/dev/null | grep -c '/nix/store' || true)"
            if [ "$nix_refs" -gt 0 ]; then
                say "WARNING: $nix_refs Nix store references still present in binary"
                ldd "$binary" 2>/dev/null | grep '/nix/store' || true
            else
                say "OK: No Nix store references in binary"
            fi
            ;;
    esac
}

bundle_nix_libs_darwin() {
    local target="$1"
    local lib_dir="$2"

    otool -L "$target" 2>/dev/null | grep '/nix/store' | awk '{print $1}' | while read -r lib; do
        if [ -f "$lib" ]; then
            local lib_name="$(basename "$lib")"
            # Skip if already bundled
            [ -f "$lib_dir/$lib_name" ] && continue
            say "  Bundling: $lib_name"
            cp "$lib" "$lib_dir/"
            chmod +w "$lib_dir/$lib_name"
        fi
    done

    # Rewrite all Nix store references to use @loader_path
    otool -L "$target" 2>/dev/null | grep '/nix/store' | awk '{print $1}' | while read -r lib; do
        local lib_name="$(basename "$lib")"
        install_name_tool -change "$lib" "@loader_path/../lib/$lib_name" "$target" 2>/dev/null || true
    done
}

bundle_nix_libs_linux() {
    local target="$1"
    local lib_dir="$2"

    # Note: grep may return 1 if no matches, so use || true to avoid pipefail errors
    # Use process substitution to avoid subshell issues with continue
    while read -r lib; do
        [ -z "$lib" ] && continue
        if [ -f "$lib" ]; then
            local lib_name="$(basename "$lib")"
            # Skip if already bundled
            [ -f "$lib_dir/$lib_name" ] && continue
            # Skip glibc libraries - they must match the system dynamic linker
            # Bundling glibc causes symbol mismatch errors with the system ld-linux
            case "$lib_name" in
                libc.so*|libm.so*|libdl.so*|librt.so*|libpthread.so*|ld-linux*.so*|libgcc_s.so*)
                    continue
                    ;;
            esac
            say "  Bundling: $lib_name"
            cp "$lib" "$lib_dir/"
        fi
    done < <(ldd "$target" 2>/dev/null | { grep '/nix/store' || true; } | awk '{print $3}')
}

build_fuse() {
    local toolchain_dir="$1"
    local clang_path="${toolchain_dir}/bin/clang"

    say "Building Fuse compiler with Scala Native..."

    # Verify clang is available (needed by Scala Native)
    if [ ! -f "$clang_path" ]; then
        err "Clang not found at $clang_path - run download_llvm first"
    fi

    # On macOS, use system clang (from Xcode) which has proper SDK paths
    # On Linux, use the downloaded clang and add to PATH
    case "$(uname -s)" in
        Darwin)
            say "Using system clang on macOS (for SDK headers)"
            # Add /usr/bin to PATH first so Scala Native finds system clang/clang++
            # Add downloaded LLVM bin for llc/opt
            export PATH="/usr/bin:${toolchain_dir}/bin:$PATH"
            # Verify system clang works
            say "System clang version: $(clang --version | head -1)"
            say "System clang++ location: $(which clang++)"
            ;;
        *)
            export PATH="${toolchain_dir}/bin:$PATH"
            export CLANG="$clang_path"
            export CLANGPP="${clang_path}++"
            ;;
    esac

    # Build with SBT
    cd "$PROJECT_ROOT"
    sbt fuseNative/nativeLink

    # Find and copy the binary
    local fuse_bin
    fuse_bin=$(find . -path "*native*" -name "fuse-out" -type f | head -1)

    if [ -z "$fuse_bin" ] || [ ! -f "$fuse_bin" ]; then
        err "Fuse build failed: fuse-out not found"
    fi

    cp "$fuse_bin" "${toolchain_dir}/bin/fuse"
    chmod +x "${toolchain_dir}/bin/fuse"

    say "Fuse compiler built successfully"
}

download_llvm() {
    local platform="$1"
    local toolchain_dir="$2"

    local llvm_url llvm_version llvm_prefix
    case "$platform" in
        linux-x86_64)
            llvm_url="$LLVM_URL_LINUX"
            llvm_version="$LLVM_VERSION_LINUX"
            llvm_prefix="clang+llvm-${llvm_version}-x86_64-linux-gnu-ubuntu-18.04"
            ;;
        macos-arm64|macos-x86_64)
            llvm_url="$LLVM_URL_MACOS"
            llvm_version="$LLVM_VERSION_MACOS"
            llvm_prefix="clang+llvm-${llvm_version}-arm64-apple-darwin22.0"
            ;;
        *)
            err "Unsupported platform for LLVM: $platform"
            ;;
    esac

    say "Downloading pre-built LLVM ${llvm_version} (selective extraction)..."
    need_cmd curl

    mkdir -p "${toolchain_dir}/bin" "${toolchain_dir}/lib"

    # Download and extract only the specific files we need (saves disk space)
    local tmp_archive="/tmp/llvm-archive.tar.xz"
    curl -L "$llvm_url" -o "$tmp_archive"

    # Extract only the binaries we need (clang is a symlink to clang-15)
    say "Extracting clang, opt, llc..."
    tar -xJf "$tmp_archive" --strip-components=2 -C "${toolchain_dir}/bin" \
        "${llvm_prefix}/bin/clang" \
        "${llvm_prefix}/bin/clang-15" \
        "${llvm_prefix}/bin/opt" \
        "${llvm_prefix}/bin/llc"

    # Extract only essential shared libraries
    say "Extracting shared libraries..."
    tar -xJf "$tmp_archive" --strip-components=2 -C "${toolchain_dir}/lib" \
        --wildcards \
        "${llvm_prefix}/lib/libclang*.so*" \
        "${llvm_prefix}/lib/libclang*.dylib*" \
        "${llvm_prefix}/lib/libLLVM*.so*" \
        "${llvm_prefix}/lib/libLLVM*.dylib*" \
        "${llvm_prefix}/lib/libLTO*.so*" \
        "${llvm_prefix}/lib/libLTO*.dylib*" \
        "${llvm_prefix}/lib/libRemarks*.so*" \
        "${llvm_prefix}/lib/libRemarks*.dylib*" \
        2>/dev/null || true

    # Extract clang resource directory (needed for headers)
    say "Extracting clang headers..."
    tar -xJf "$tmp_archive" --strip-components=2 -C "${toolchain_dir}/lib" \
        "${llvm_prefix}/lib/clang" 2>/dev/null || true

    rm -f "$tmp_archive"
    say "LLVM ${llvm_version} installed"
    "${toolchain_dir}/bin/clang" --version | head -1
}

build_gc() {
    local toolchain_dir="$1"

    say "Building Boehm GC ${GC_VERSION}..."

    local src_dir="/tmp/gc-build"
    rm -rf "$src_dir"
    mkdir -p "$src_dir"
    cd "$src_dir"

    git clone --depth 1 -b "$GC_VERSION" https://github.com/ivmai/bdwgc.git
    cd bdwgc
    git clone --depth 1 https://github.com/ivmai/libatomic_ops.git

    ./autogen.sh
    ./configure --prefix="$toolchain_dir" --enable-shared --disable-static
    make -j"$(get_parallelism)"
    make install

    cd "$PROJECT_ROOT"
    rm -rf "$src_dir"

    say "Boehm GC ${GC_VERSION} installed"
}

copy_runtime() {
    local toolchain_dir="$1"

    say "Copying runtime files..."

    mkdir -p "${toolchain_dir}/runtime"
    cp "${PROJECT_ROOT}/grin/runtime.c" "${toolchain_dir}/runtime/"
    cp "${PROJECT_ROOT}/grin/prim_ops.c" "${toolchain_dir}/runtime/"
    cp "${PROJECT_ROOT}/grin/prim_ops.h" "${toolchain_dir}/runtime/"

    say "Runtime files copied"
}

fix_macos_paths() {
    local toolchain_dir="$1"

    say "Fixing macOS library paths..."

    # Fix binary rpaths
    cd "${toolchain_dir}/bin"
    for bin in clang grin opt llc fuse; do
        if [ -f "$bin" ]; then
            install_name_tool -add_rpath @loader_path/../lib "$bin" 2>/dev/null || true
        fi
    done

    # Fix GC library install_name to use @rpath (fuseup will set actual path at install time)
    local gc_lib="${toolchain_dir}/lib/libgc.1.dylib"
    if [ -f "$gc_lib" ]; then
        chmod +w "$gc_lib"
        install_name_tool -id "@rpath/libgc.1.dylib" "$gc_lib"
        say "Fixed libgc install_name to @rpath/libgc.1.dylib"
    fi

    say "macOS library paths fixed"
}

create_tarball() {
    local platform="$1"
    local toolchain_dir="$2"
    local output_dir="$3"

    say "Creating tarball..."

    # Remove static libraries to reduce size
    rm -f "${toolchain_dir}/lib/"*.a 2>/dev/null || true

    local tarball_name="fuse-toolchain-${platform}.tar.gz"
    mkdir -p "$output_dir"

    # Create tarball with toolchain/ prefix
    local parent_dir
    parent_dir="$(dirname "$toolchain_dir")"
    local base_name
    base_name="$(basename "$toolchain_dir")"

    tar -czvf "${output_dir}/${tarball_name}" -C "$parent_dir" "$base_name/"

    say "Tarball created: ${output_dir}/${tarball_name}"
    ls -lh "${output_dir}/${tarball_name}"
}

# =============================================================================
# Linux Docker Build
# =============================================================================

build_gc_in_docker() {
    local toolchain_dir="$1"

    say "Building Boehm GC in Docker (manylinux2014)..."

    need_cmd docker

    # Run GC build in Docker for manylinux compatibility
    docker run --rm \
        -v "${toolchain_dir}:/toolchain" \
        "$DOCKER_IMAGE" \
        /bin/bash -c '
            set -ex

            # Install dependencies
            yum install -y git autoconf automake libtool

            mkdir -p /build/src
            cd /build/src

            # Build Boehm GC
            echo "=== Building Boehm GC ==="
            git clone --depth 1 -b "'"$GC_VERSION"'" https://github.com/ivmai/bdwgc.git
            cd bdwgc
            git clone --depth 1 https://github.com/ivmai/libatomic_ops.git
            ./autogen.sh
            ./configure --prefix=/toolchain --enable-shared --disable-static
            make -j$(nproc)
            make install

            # Verify glibc requirements for GC library
            echo "=== Checking glibc requirements ==="
            for lib in /toolchain/lib/libgc*.so*; do
                if [ -f "$lib" ]; then
                    echo "$(basename $lib):"
                    objdump -T "$lib" 2>/dev/null | grep GLIBC | sed "s/.*GLIBC_/  GLIBC_/" | sort -u | tail -3 || true
                fi
            done
        '

    say "Boehm GC built successfully"
}

build_linux_in_docker() {
    local grin_ref="$1"
    local output_dir="$2"
    local grin_bin="$3"
    local build_fuse="$4"

    say "Building Linux toolchain..."

    local toolchain_dir="/tmp/fuse-build/toolchain"
    rm -rf "/tmp/fuse-build"
    mkdir -p "$toolchain_dir"/{bin,lib,include,runtime}

    # Download LLVM first (needed for Fuse build)
    download_llvm "linux-x86_64" "$toolchain_dir"

    # Build Fuse compiler early (fail fast before slow GRIN build)
    if [ "$build_fuse" = "true" ]; then
        build_fuse "$toolchain_dir"
    fi

    # Build or copy GRIN (on host with Nix - slow step)
    if [ -n "$grin_bin" ] && [ -f "$grin_bin" ]; then
        say "Using pre-built GRIN: $grin_bin"
        cp "$grin_bin" "${toolchain_dir}/bin/grin"
        chmod +x "${toolchain_dir}/bin/grin"
    else
        build_grin "$grin_ref" "${toolchain_dir}/bin/grin"
    fi

    # Build Boehm GC in Docker for manylinux compatibility
    build_gc_in_docker "$toolchain_dir"

    # Copy runtime files
    copy_runtime "$toolchain_dir"

    # Create tarball
    create_tarball "linux-x86_64" "$toolchain_dir" "$output_dir"

    say "Linux toolchain built successfully"
}

# =============================================================================
# macOS Native Build
# =============================================================================

build_macos_native() {
    local grin_ref="$1"
    local output_dir="$2"
    local grin_bin="$3"
    local platform="$4"
    local build_fuse="$5"

    say "Building macOS toolchain natively..."

    local toolchain_dir="/tmp/fuse-build/toolchain"
    rm -rf "/tmp/fuse-build"
    mkdir -p "$toolchain_dir"/{bin,lib,include,runtime}

    # Download LLVM first (needed for Fuse build)
    download_llvm "$platform" "$toolchain_dir"

    # Build Fuse compiler early (fail fast before slow GRIN build)
    if [ "$build_fuse" = "true" ]; then
        build_fuse "$toolchain_dir"
    fi

    # Build or copy GRIN (slow step)
    if [ -n "$grin_bin" ] && [ -f "$grin_bin" ]; then
        say "Using pre-built GRIN: $grin_bin"
        cp "$grin_bin" "${toolchain_dir}/bin/grin"
        chmod +x "${toolchain_dir}/bin/grin"
    else
        build_grin "$grin_ref" "${toolchain_dir}/bin/grin"
    fi

    # Build Boehm GC
    build_gc "$toolchain_dir"

    # Copy runtime
    copy_runtime "$toolchain_dir"

    # Fix macOS library paths
    fix_macos_paths "$toolchain_dir"

    # Create tarball
    create_tarball "$platform" "$toolchain_dir" "$output_dir"

    say "macOS toolchain built successfully"
}

# =============================================================================
# Main
# =============================================================================

usage() {
    cat << EOF
build-toolchain.sh - Build the Fuse toolchain

USAGE:
    ./scripts/build-toolchain.sh [OPTIONS]

OPTIONS:
    --grin-ref <ref>      GRIN git ref (default: $DEFAULT_GRIN_REF)
    --output-dir <path>   Output directory (default: ./output)
    --skip-grin <path>    Path to pre-built GRIN binary
    --skip-fuse           Skip building Fuse compiler (built by default)
    -h, --help            Print this help

EXAMPLES:
    ./scripts/build-toolchain.sh
    ./scripts/build-toolchain.sh --grin-ref boehm-gc
    ./scripts/build-toolchain.sh --skip-fuse
    ./scripts/build-toolchain.sh --output-dir ./build/out
EOF
}

main() {
    local grin_ref="$DEFAULT_GRIN_REF"
    local output_dir="${PROJECT_ROOT}/output"
    local grin_bin=""
    local build_fuse="true"

    # Parse arguments
    while [ $# -gt 0 ]; do
        case "$1" in
            --grin-ref)
                shift
                grin_ref="$1"
                ;;
            --output-dir)
                shift
                output_dir="$1"
                ;;
            --skip-grin)
                shift
                grin_bin="$1"
                [ ! -f "$grin_bin" ] && err "GRIN binary not found: $grin_bin"
                ;;
            --skip-fuse)
                build_fuse="false"
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            *)
                err "Unknown option: $1"
                ;;
        esac
        shift
    done

    # Detect platform
    local platform
    platform="$(detect_platform)"
    say "Detected platform: $platform"

    # Create output directory
    mkdir -p "$output_dir"
    output_dir="$(cd "$output_dir" && pwd)"

    # Build based on platform
    case "$platform" in
        linux-x86_64)
            build_linux_in_docker "$grin_ref" "$output_dir" "$grin_bin" "$build_fuse"
            ;;
        macos-arm64|macos-x86_64)
            build_macos_native "$grin_ref" "$output_dir" "$grin_bin" "$platform" "$build_fuse"
            ;;
        *)
            err "Unsupported platform: $platform"
            ;;
    esac

    say "Build complete!"
    say "Output: ${output_dir}/fuse-toolchain-${platform}.tar.gz"
}

main "$@"
