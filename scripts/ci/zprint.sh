#!/usr/bin/env bash
set -e

VERSION="1.3.0"

# To get the checksum run:
# curl -sSL https://github.com/kkinnear/zprint/releases/download/${VERSION}/zprintl-${VERSION} | sha256sum
CHECKSUM="3447ad2fe18847d884a1555e8384763c461b55b8959063a7454d68e76fe2071e"

BIN_DIR=".bin"
ZPRINT_BIN="${BIN_DIR}/zprint-${VERSION}"

if [ ! -f "$ZPRINT_BIN" ]; then
    mkdir -p "$BIN_DIR"
    echo "Downloading zprint ${VERSION} for Linux..."

    URL="https://github.com/kkinnear/zprint/releases/download/${VERSION}/zprintl-${VERSION}"
    curl -sSL -o "$ZPRINT_BIN" "$URL"

    echo "Verifying checksum..."
    echo "${CHECKSUM}  ${ZPRINT_BIN}" | sha256sum -c -

    chmod +x "$ZPRINT_BIN"
fi

"$ZPRINT_BIN" "$(cat .zprint.edn)" -w "$@"
