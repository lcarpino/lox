# Profiling `cljlox`

We use [`clj-async-profiler`](https://github.com/clojure-goes-fast/clj-async-profiler) to profile the `cljlox`
interpreter.

## Prerequisites
Configure `perf` kernel parameters to allow the profiler to attach to the process correctly:

```bash
sudo sysctl -w kernel.perf_event_paranoid=1
sudo sysctl -w kernel.kptr_restrict=0
```

## Running the Profiler via the REPL

Start your REPL and require the profiler namespace:
```clojure
(require '[clj-async-profiler.core :as prof])
```

### CPU Profiling

```clojure
(require '[clj-async-profiler.core :as prof])

(prof/profile (lox.main/run-file "demo/slow-fib.lox"))
```

### Memory Profiling

```clojure
(require '[clj-async-profiler.core :as prof])

(prof/profile {:event :alloc} (lox.main/run-file "demo/slow-fib.lox"))
```

## Viewing the Results
By default, `clj-async-profiler` saves the generated flame graphs to `/tmp/clj-async-profiler/results/` from which they
can be viewed or interacted with. The profiler also ships with a built in web-server for serving multiple profiles which
can be run with:

```clojure
(require '[clj-async-profiler.core :as prof])

(prof/serve-ui "0.0.0.0" 8080)
```
