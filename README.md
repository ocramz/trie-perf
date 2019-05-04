# trie-perf

[![Build Status](https://travis-ci.org/ocramz/trie-perf.png)](https://travis-ci.org/ocramz/trie-perf)

Performance shootout of various trie implementations.

"space" benchmarking performed with `weigh` and "time" benchmarks were done with `criterion`.

Currently comparing two unoptimized implementations taken (but slightly modified) from didactic blogposts (https://alexandersgreen.wordpress.com/2010/09/13/prefix-trees-in-haskell/ and https://blog.jle.im/entry/tries-with-recursion-schemes.html) and two available on Hackage (`generic-trie` and `bytestring-trie`).

Note : I am still somewhat inexperienced with Haskell benchmarking so the numbers below should be taken with a grain of salt. 

Contributing : I would love to receive pull requests with extended and/or improved benchmarks.


Results from a laptop GHCi session :


```
Benchmark space: RUNNING...

AG
  Case    Allocated  GCs
  small       1,208    0
  medium     12,960    0
  large     133,008    0

JL
  Case    Allocated  GCs
  small       5,816    0
  medium     60,240    0
  large     637,824    0

generic-trie
  Case    Allocated  GCs
  small       4,320    0
  medium     24,752    0
  large     222,320    0

bytestring-trie
  Case    Allocated  GCs
  small       2,432    0
  medium     52,712    0
  large   2,600,152    2
Benchmark space: FINISH
```

```
Benchmark time: RUNNING...
benchmarking AG/small
time                 181.5 ns   (176.1 ns .. 185.9 ns)
                     0.996 R²   (0.994 R² .. 0.997 R²)
mean                 178.4 ns   (174.9 ns .. 181.7 ns)
std dev              10.86 ns   (9.374 ns .. 12.77 ns)
variance introduced by outliers: 77% (severely inflated)

benchmarking AG/medium
time                 5.185 μs   (5.007 μs .. 5.362 μs)
                     0.993 R²   (0.991 R² .. 0.995 R²)
mean                 5.289 μs   (5.155 μs .. 5.431 μs)
std dev              451.8 ns   (388.5 ns .. 611.0 ns)
variance introduced by outliers: 83% (severely inflated)

benchmarking AG/large
time                 103.6 μs   (101.1 μs .. 106.6 μs)
                     0.993 R²   (0.989 R² .. 0.996 R²)
mean                 105.6 μs   (103.4 μs .. 107.9 μs)
std dev              7.921 μs   (6.453 μs .. 9.934 μs)
variance introduced by outliers: 71% (severely inflated)

benchmarking JL/small
time                 950.7 ns   (914.4 ns .. 986.9 ns)
                     0.991 R²   (0.988 R² .. 0.994 R²)
mean                 968.5 ns   (937.7 ns .. 1.003 μs)
std dev              117.4 ns   (90.40 ns .. 164.0 ns)
variance introduced by outliers: 92% (severely inflated)

benchmarking JL/medium
time                 15.88 μs   (15.37 μs .. 16.39 μs)
                     0.994 R²   (0.992 R² .. 0.996 R²)
mean                 16.00 μs   (15.62 μs .. 16.40 μs)
std dev              1.332 μs   (1.165 μs .. 1.573 μs)
variance introduced by outliers: 80% (severely inflated)

benchmarking JL/large
time                 102.2 μs   (99.34 μs .. 104.8 μs)
                     0.995 R²   (0.993 R² .. 0.997 R²)
mean                 98.88 μs   (97.04 μs .. 100.9 μs)
std dev              6.480 μs   (5.649 μs .. 7.350 μs)
variance introduced by outliers: 65% (severely inflated)

benchmarking generic-trie/small
time                 540.2 ns   (524.5 ns .. 559.1 ns)
                     0.994 R²   (0.991 R² .. 0.997 R²)
mean                 564.8 ns   (552.7 ns .. 579.3 ns)
std dev              43.96 ns   (34.92 ns .. 58.14 ns)
variance introduced by outliers: 84% (severely inflated)

benchmarking generic-trie/medium
time                 6.480 μs   (6.294 μs .. 6.653 μs)
                     0.994 R²   (0.991 R² .. 0.997 R²)
mean                 6.411 μs   (6.252 μs .. 6.612 μs)
std dev              576.4 ns   (485.7 ns .. 785.2 ns)
variance introduced by outliers: 84% (severely inflated)

benchmarking generic-trie/large
time                 69.79 μs   (68.15 μs .. 72.17 μs)
                     0.993 R²   (0.990 R² .. 0.997 R²)
mean                 73.61 μs   (71.47 μs .. 76.05 μs)
std dev              7.236 μs   (5.887 μs .. 9.019 μs)
variance introduced by outliers: 82% (severely inflated)

benchmarking bytestring-trie/small
time                 353.1 ns   (344.1 ns .. 363.8 ns)
                     0.994 R²   (0.993 R² .. 0.996 R²)
mean                 361.5 ns   (350.2 ns .. 383.7 ns)
std dev              51.29 ns   (28.04 ns .. 90.24 ns)
variance introduced by outliers: 95% (severely inflated)

benchmarking bytestring-trie/medium
time                 4.707 μs   (4.543 μs .. 4.858 μs)
                     0.992 R²   (0.990 R² .. 0.995 R²)
mean                 4.729 μs   (4.606 μs .. 4.890 μs)
std dev              464.3 ns   (388.5 ns .. 685.7 ns)
variance introduced by outliers: 87% (severely inflated)

benchmarking bytestring-trie/large
time                 99.09 μs   (94.92 μs .. 102.1 μs)
                     0.991 R²   (0.988 R² .. 0.995 R²)
mean                 97.70 μs   (95.20 μs .. 100.7 μs)
std dev              9.242 μs   (7.809 μs .. 11.49 μs)
variance introduced by outliers: 80% (severely inflated)

```