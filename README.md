# trie-perf

[![Build Status](https://travis-ci.org/ocramz/trie-perf.png)](https://travis-ci.org/ocramz/trie-perf)

Performance shootout of various prefix tree ("trie") implementations.

"space" benchmarking performed with `weigh` and "time" benchmarks done with `criterion`.

Currently comparing two unoptimized implementations taken (but slightly modified) from didactic blogposts (https://alexandersgreen.wordpress.com/2010/09/13/prefix-trees-in-haskell/ and https://blog.jle.im/entry/tries-with-recursion-schemes.html) and four available on Hackage ([`generic-trie`](https://hackage.haskell.org/package/generic-trie), [`bytestring-trie`](https://hackage.haskell.org/package/bytestring-trie), [`text-trie`](https://hackage.haskell.org/package/text-trie), and [`trie-simple`](https://hackage.haskell.org/package/trie-simple)).

`generic-trie` seems to be the best choice, at least for a "lookup - fromList" pair, but I was curious to see how very diverse implementation techniques, notably one based on recursion schemes and another using an arrow type internally, lead to different space and time scaling behaviours.


## Contributing

Pull requests that extend and/or improve these benchmarks are very welcome.


Results obtained on a 2017 MacBook Pro using `stack bench`:

```
Benchmark space: RUNNING...

AG

  Case    Allocated  GCs
  small       1,208    0
  medium     12,968    0
  large     133,456    0

JL

  Case    Allocated  GCs
  small       5,976    0
  medium     59,928    0
  large     645,264    0

generic-trie

  Case    Allocated  GCs
  small       4,320    0
  medium     21,992    0
  large     221,760    0

bytestring-trie

  Case    Allocated  GCs
  small       2,224    0
  medium     52,416    0
  large   2,600,752    2

text-trie

  Case    Allocated  GCs
  small       2,536    0
  medium     62,264    0
  large   3,071,280    2

trie-simple

  Case     Allocated  GCs
  small        3,088    0
  medium     201,448    0
  large   18,897,064   18
Benchmark space: FINISH
```

```
Benchmark time: RUNNING...
benchmarking AG/small
time                 135.4 ns   (132.6 ns .. 138.7 ns)
                     0.994 R²   (0.989 R² .. 0.998 R²)
mean                 133.4 ns   (130.8 ns .. 138.2 ns)
std dev              11.43 ns   (6.546 ns .. 17.15 ns)
variance introduced by outliers: 87% (severely inflated)

benchmarking AG/medium
time                 4.207 μs   (4.111 μs .. 4.371 μs)
                     0.989 R²   (0.981 R² .. 0.997 R²)
mean                 4.364 μs   (4.246 μs .. 4.522 μs)
std dev              452.0 ns   (337.3 ns .. 582.7 ns)
variance introduced by outliers: 88% (severely inflated)

benchmarking AG/large
time                 70.83 μs   (68.15 μs .. 73.93 μs)
                     0.991 R²   (0.986 R² .. 0.997 R²)
mean                 69.11 μs   (67.75 μs .. 71.10 μs)
std dev              5.418 μs   (3.809 μs .. 7.481 μs)
variance introduced by outliers: 75% (severely inflated)

benchmarking JL/small
time                 887.7 ns   (866.9 ns .. 918.7 ns)
                     0.993 R²   (0.983 R² .. 0.999 R²)
mean                 886.1 ns   (874.1 ns .. 924.3 ns)
std dev              64.19 ns   (32.05 ns .. 129.6 ns)
variance introduced by outliers: 81% (severely inflated)

benchmarking JL/medium
time                 12.11 μs   (11.89 μs .. 12.40 μs)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 12.25 μs   (12.09 μs .. 12.48 μs)
std dev              638.9 ns   (425.2 ns .. 969.5 ns)
variance introduced by outliers: 62% (severely inflated)

benchmarking JL/large
time                 68.27 μs   (65.93 μs .. 71.35 μs)
                     0.991 R²   (0.982 R² .. 1.000 R²)
mean                 67.17 μs   (66.33 μs .. 69.05 μs)
std dev              4.090 μs   (1.513 μs .. 7.208 μs)
variance introduced by outliers: 63% (severely inflated)

benchmarking generic-trie/small
time                 486.8 ns   (477.0 ns .. 499.4 ns)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 478.3 ns   (470.7 ns .. 489.4 ns)
std dev              30.40 ns   (20.92 ns .. 48.38 ns)
variance introduced by outliers: 77% (severely inflated)

benchmarking generic-trie/medium
time                 4.786 μs   (4.620 μs .. 4.987 μs)
                     0.994 R²   (0.988 R² .. 1.000 R²)
mean                 4.677 μs   (4.627 μs .. 4.773 μs)
std dev              222.9 ns   (99.69 ns .. 392.9 ns)
variance introduced by outliers: 60% (severely inflated)

benchmarking generic-trie/large
time                 52.39 μs   (50.40 μs .. 53.86 μs)
                     0.994 R²   (0.992 R² .. 0.998 R²)
mean                 50.30 μs   (49.64 μs .. 51.10 μs)
std dev              2.537 μs   (1.805 μs .. 3.436 μs)
variance introduced by outliers: 55% (severely inflated)

benchmarking bytestring-trie/small
time                 258.9 ns   (255.1 ns .. 263.4 ns)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 257.7 ns   (255.8 ns .. 261.0 ns)
std dev              7.759 ns   (5.639 ns .. 11.23 ns)
variance introduced by outliers: 44% (moderately inflated)

benchmarking bytestring-trie/medium
time                 3.325 μs   (3.256 μs .. 3.426 μs)
                     0.994 R²   (0.990 R² .. 0.998 R²)
mean                 3.354 μs   (3.301 μs .. 3.433 μs)
std dev              209.7 ns   (134.9 ns .. 298.7 ns)
variance introduced by outliers: 73% (severely inflated)

benchmarking bytestring-trie/large
time                 73.46 μs   (72.62 μs .. 74.44 μs)
                     0.994 R²   (0.987 R² .. 0.999 R²)
mean                 74.85 μs   (72.91 μs .. 79.93 μs)
std dev              9.677 μs   (3.383 μs .. 16.95 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking text-trie/small
time                 282.0 ns   (278.1 ns .. 286.3 ns)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 289.9 ns   (282.4 ns .. 304.0 ns)
std dev              32.29 ns   (16.36 ns .. 51.40 ns)
variance introduced by outliers: 92% (severely inflated)

benchmarking text-trie/medium
time                 3.279 μs   (3.237 μs .. 3.340 μs)
                     0.997 R²   (0.992 R² .. 0.999 R²)
mean                 3.396 μs   (3.311 μs .. 3.534 μs)
std dev              352.3 ns   (223.2 ns .. 507.7 ns)
variance introduced by outliers: 89% (severely inflated)

benchmarking text-trie/large
time                 74.49 μs   (73.21 μs .. 76.11 μs)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 78.65 μs   (76.51 μs .. 82.87 μs)
std dev              10.06 μs   (6.959 μs .. 14.33 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking trie-simple/small
time                 274.3 ns   (270.3 ns .. 279.6 ns)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 272.6 ns   (269.8 ns .. 277.2 ns)
std dev              12.07 ns   (7.266 ns .. 18.85 ns)
variance introduced by outliers: 63% (severely inflated)

benchmarking trie-simple/medium
time                 23.87 μs   (23.52 μs .. 24.49 μs)
                     0.995 R²   (0.991 R² .. 0.999 R²)
mean                 24.86 μs   (24.37 μs .. 25.73 μs)
std dev              2.058 μs   (1.379 μs .. 3.075 μs)
variance introduced by outliers: 79% (severely inflated)

benchmarking trie-simple/large
time                 16.70 ms   (15.57 ms .. 17.95 ms)
                     0.977 R²   (0.956 R² .. 0.995 R²)
mean                 16.51 ms   (16.05 ms .. 17.14 ms)
std dev              1.378 ms   (1.037 ms .. 1.926 ms)
variance introduced by outliers: 40% (moderately inflated)

Benchmark time: FINISH
Completed 2 action(s).
```

