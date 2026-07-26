#! /usr/bin/env bash

PROJECT_DIR="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)/.."
JAR_PATH="$PROJECT_DIR/bazel-bin/jlox/com/craftinginterpreters/lox/lox_deploy.jar"

if [ ! -f "$JAR_PATH" ]; then
    echo "Error: $JAR_PATH not found." >&2
    echo "Please run 'bazel build //jlox/com/craftinginterpreters/lox:lox_deploy.jar' in $PROJECT_DIR first." >&2
    exit 1
fi

java -jar "$JAR_PATH" "$@"
