#!/bin/sh
#
# Local “runner” wrapper for your Lox interpreter.
# Builds once, quietly, then just dispatches to the binary.

set -e

# Where we build
BUILD_DIR=/tmp/codecrafters-build-interpreter-rust
BIN="$BUILD_DIR/release/codecrafters-interpreter"

# 1) If the binary is missing (first run), build in release, but send all build logs to /dev/null
if [ ! -x "$BIN" ]; then
  (
    cd "$(dirname "$0")"
    cargo build --release \
      --target-dir="$BUILD_DIR" \
      --manifest-path Cargo.toml \
      > /dev/null 2>&1
  )
fi

# 2) Now run the interpreter with the exact args you passed in:
#    e.g. ./your_program.sh tokenize test.lox
exec "$BIN" "$@"
