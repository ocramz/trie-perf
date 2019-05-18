# trie-perf

[![Build Status](https://travis-ci.org/ocramz/trie-perf.png)](https://travis-ci.org/ocramz/trie-perf)

Performance shootout of various prefix tree ("trie") implementations.

"space" benchmarking performed with `weigh` and "time" benchmarks done with `criterion`.

Currently comparing two unoptimized implementations taken (but slightly modified) from didactic blogposts (https://alexandersgreen.wordpress.com/2010/09/13/prefix-trees-in-haskell/ and https://blog.jle.im/entry/tries-with-recursion-schemes.html) and three available on Hackage ([`generic-trie`](https://hackage.haskell.org/package/generic-trie), [`bytestring-trie`](https://hackage.haskell.org/package/bytestring-trie), and [`trie-simple`](https://hackage.haskell.org/package/trie-simple)).

`generic-trie` seems to be the best choice, at least for a "lookup - fromList" pair, but I was curious to see how very diverse implementation techniques, notably one based on recursion schemes and another using an arrow type internally, lead to different space and time scaling behaviours.


## Contributing

Pull requests that extend and/or improve these benchmarks are very welcome.


Results obtained on a 2015 MacBook Pro GHCi session :


```
Benchmark space: RUNNING...

AG

  Case    Allocated  GCs
  small       1,360    0
  medium     13,112    0
  large     133,752    0

JL

  Case    Allocated  GCs
  small       5,928    0
  medium     61,824    0
  large     637,056    0

generic-trie

  Case    Allocated  GCs
  small       6,416    0
  medium     25,608    0
  large     218,344    0

bytestring-trie

  Case    Allocated  GCs
  small       2,432    0
  medium     52,656    0
  large   2,598,008    2

trie-simple

  Case     Allocated  GCs
  small        2,992    0
  medium     201,784    0
  large   18,895,048   18
Benchmark space: FINISH
```

```
Benchmark time: RUNNING...
benchmarking AG/small
time                 175.6 ns   (174.5 ns .. 177.3 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 176.4 ns   (175.5 ns .. 178.1 ns)
std dev              4.243 ns   (2.521 ns .. 7.114 ns)
variance introduced by outliers: 34% (moderately inflated)

benchmarking AG/medium
time                 5.278 μs   (5.249 μs .. 5.316 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.320 μs   (5.267 μs .. 5.535 μs)
std dev              306.5 ns   (102.7 ns .. 669.0 ns)
variance introduced by outliers: 69% (severely inflated)

benchmarking AG/large
time                 87.55 μs   (86.39 μs .. 88.67 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 87.88 μs   (86.85 μs .. 90.20 μs)
std dev              4.967 μs   (1.965 μs .. 8.544 μs)
variance introduced by outliers: 59% (severely inflated)

benchmarking JL/small
time                 1.060 μs   (1.055 μs .. 1.065 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.074 μs   (1.061 μs .. 1.115 μs)
std dev              70.53 ns   (22.19 ns .. 141.5 ns)
variance introduced by outliers: 78% (severely inflated)

benchmarking JL/medium
time                 15.38 μs   (15.27 μs .. 15.52 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 15.65 μs   (15.46 μs .. 15.90 μs)
std dev              756.3 ns   (545.8 ns .. 1.075 μs)
variance introduced by outliers: 57% (severely inflated)

benchmarking JL/large
time                 89.09 μs   (87.90 μs .. 90.15 μs)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 91.01 μs   (88.69 μs .. 97.55 μs)
std dev              11.52 μs   (5.425 μs .. 24.76 μs)
variance introduced by outliers: 88% (severely inflated)

benchmarking generic-trie/small
time                 469.9 ns   (468.0 ns .. 472.9 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 475.3 ns   (471.0 ns .. 487.6 ns)
std dev              23.10 ns   (7.637 ns .. 49.55 ns)
variance introduced by outliers: 66% (severely inflated)

benchmarking generic-trie/medium
time                 6.506 μs   (6.400 μs .. 6.647 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 6.563 μs   (6.476 μs .. 6.725 μs)
std dev              400.6 ns   (242.9 ns .. 685.1 ns)
variance introduced by outliers: 71% (severely inflated)

benchmarking generic-trie/large
time                 69.24 μs   (68.48 μs .. 70.16 μs)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 70.59 μs   (69.17 μs .. 76.47 μs)
std dev              8.090 μs   (2.274 μs .. 17.78 μs)
variance introduced by outliers: 86% (severely inflated)

benchmarking bytestring-trie/small
time                 319.3 ns   (317.2 ns .. 322.8 ns)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 322.1 ns   (319.5 ns .. 326.4 ns)
std dev              10.69 ns   (7.791 ns .. 13.87 ns)
variance introduced by outliers: 48% (moderately inflated)

benchmarking bytestring-trie/medium
time                 4.354 μs   (4.292 μs .. 4.448 μs)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 4.371 μs   (4.324 μs .. 4.479 μs)
std dev              225.9 ns   (146.2 ns .. 369.1 ns)
variance introduced by outliers: 64% (severely inflated)

benchmarking bytestring-trie/large
time                 90.30 μs   (89.40 μs .. 91.22 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 89.86 μs   (89.33 μs .. 90.66 μs)
std dev              2.082 μs   (1.396 μs .. 3.239 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking trie-simple/small
time                 402.4 ns   (387.5 ns .. 422.7 ns)
                     0.984 R²   (0.970 R² .. 0.998 R²)
mean                 395.3 ns   (385.9 ns .. 415.2 ns)
std dev              43.83 ns   (25.89 ns .. 70.51 ns)
variance introduced by outliers: 92% (severely inflated)

benchmarking trie-simple/medium
time                 33.17 μs   (32.77 μs .. 33.71 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 33.23 μs   (33.04 μs .. 33.54 μs)
std dev              814.7 ns   (537.0 ns .. 1.141 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking trie-simple/large
time                 19.79 ms   (18.73 ms .. 21.76 ms)
                     0.969 R²   (0.921 R² .. 0.998 R²)
mean                 20.43 ms   (19.77 ms .. 21.49 ms)
std dev              1.875 ms   (1.201 ms .. 2.877 ms)
variance introduced by outliers: 41% (moderately inflated)

Benchmark time: FINISH

```
