#!/usr/bin/env bash
set -e

VERSION="2026.08.03"

# To get the checksum run:
# curl -sSL https://github.com/clj-kondo/clj-kondo/releases/download/v${VERSION}/clj-kondo-${VERSION}-linux-amd64.zip.sha256
CHECKSUM="e1ff7a1867247357a85c28b578be728b1c51bffb34cea8f7c8b5160b56436657"

BIN_DIR=".bin"
KONDO_BIN="${BIN_DIR}/clj-kondo-${VERSION}"

if [ ! -f "$KONDO_BIN" ]; then
    mkdir -p "$BIN_DIR"
    TMP_DIR=$(mktemp -d)
    echo "Downloading clj-kondo ${VERSION} for Linux..."

    FILE="clj-kondo-${VERSION}-linux-amd64.zip"
    URL="https://github.com/clj-kondo/clj-kondo/releases/download/v${VERSION}/${FILE}"

    curl -sSL -o "${TMP_DIR}/${FILE}" "$URL"

    echo "Verifying checksum..."
    echo "${CHECKSUM}  ${TMP_DIR}/${FILE}" | sha256sum -c -

    unzip -q "${TMP_DIR}/${FILE}" -d "$TMP_DIR"
    mv "${TMP_DIR}/clj-kondo" "$KONDO_BIN"
    rm -rf "$TMP_DIR"
fi

"$KONDO_BIN" "$@"
