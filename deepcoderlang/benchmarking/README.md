The benchmarks are organized by signature; they either take one integer and one list of integers, or two lists of integers.

To print a human-readable version of the benchmarks:

```
(print-from-random-program (read-program-from-file path/to/benchmark 5) int-input-count list-input-count)
```
