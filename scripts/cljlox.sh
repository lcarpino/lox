#! /usr/bin/env bash

PROJECT_DIR="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)/.."
JAR_PATH="$PROJECT_DIR/target/cljlox.jar"

if [ ! -f "$JAR_PATH" ]; then
    echo "Error: $JAR_PATH not found." >&2
    echo "Please run 'clojure -T:build uber' in $PROJECT_DIR first." >&2
    exit 1
fi

java -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -jar "$JAR_PATH" "$@"
