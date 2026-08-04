#! /usr/bin/env bash

PROJECT_DIR="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)/.."
JS_PATH="$PROJECT_DIR/target/node/cljslox.js"

if [ ! -f "$JS_PATH" ]; then
    echo "Error: $JS_PATH not found." >&2
    echo "Please run 'npx shadow-cljs compile node' in $PROJECT_DIR first." >&2
    exit 1
fi

node "$JS_PATH" "$@"
