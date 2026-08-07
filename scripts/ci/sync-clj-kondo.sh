#! /usr/bin/env bash
set -euo pipefail

rm -rf .clj-kondo/.cache/

clojure -M:dev -e "
(require '[malli.dev]
         '[malli.clj-kondo]
         '[lox.main])

(malli.dev/start!)
(malli.clj-kondo/clean! {})
(malli.clj-kondo/emit!)"

clojure -M:dev -m clj-kondo.main \
  --copy-configs \
  --dependencies \
  --parallel \
  --lint "$(clojure -Spath -A:dev)"
