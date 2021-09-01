# adventofcode [![ci](https://github.com/oshyshko/adventofcode/actions/workflows/ci.yaml/badge.svg)](https://github.com/oshyshko/adventofcode/actions/workflows/ci.yaml)

Haskell solutions for http://adventofcode.com/

## Running

```
$ ./scripts/build-exec.sh
-----------+--------------------+- part 1 --------------+- part 2 --------------
 day       | answers            |    time  alloc   peak |    time  alloc   peak
-----------+--------------------+-----------------------+-----------------------
 Y15.D01   | 138, 1771          |    13ms 373.4K  52.4K |    13ms 619.8K  60.6K
 Y15.D02   | 1586300, 3737498   |    13ms  13.8M  67.1K |    13ms  13.8M  67.1K
 Y15.D03   | 2565, 2639         |    58ms   1.8M  52.4K |    61ms   4.1M 465.4K
 Y15.D04   | 117946, 3938038    |    44ms  80.4M  43.5K |  1329ms   2.9G  43.5K
 Y15.D05   | 236, 51            |    13ms   3.0M  76.7K |    12ms   4.2M  52.4K
 Y15.D06AI | 400410, 15343601   |   293ms  95.0M  43.7M |   213ms  82.7M  29.8M
 Y15.D06MH | 400410, 15343601   | 22415ms  12.9G  54.4M | 30050ms  18.6G  68.3M
 Y15.D06MI | 400410, 15343601   |  6298ms  16.4G  58.5M |  7262ms  16.7G  72.8M
 Y15.D06MS | 400410, 15343601   | 10256ms  18.6G  58.5M | 11946ms  18.9G  72.8M
 Y15.D06VB | 400410, 15343601   |  2957ms 643.5M 598.1M |  3206ms 942.7M 598.1M
 Y15.D06VR | 400410, 15343601   |    56ms  63.4M   3.9M |    46ms  84.0M   2.0M
 Y15.D06VS | 400410, 15343601   |    38ms  13.5M   1.1M |    35ms  15.4M   2.0M
 Y15.D06VU | 400410, 15343601   |    33ms  13.5M   1.1M |    38ms  15.4M   2.0M
 Y15.D07   | 3176, 14710        |    12ms   7.4M  85.6K |    13ms   7.8M 116.7K
 Y15.D08   | 1333, 2046         |    12ms   9.9M 221.1K |    11ms  15.2M 131.7K
 Y15.D09   | 117, 909           |    80ms  22.5M  84.8K |    82ms  22.5M  84.8K
 Y15.D10   | 360154, 5103798    |   104ms 451.6M  84.6K |  1445ms   6.2G  87.4K
 Y15.D11   | vzbxxyzz, vzcaabcc |    22ms  35.5M  43.5K |   105ms 175.0M  43.5K
 Y15.D12   | 111754, 65402      |    13ms   9.5M 184.0K |    23ms   9.1M 184.0K
 Y15.D13   | 618, 601           |   138ms 160.5M  86.9K |  1138ms   1.5G  88.1K
 Y15.D14   | 2640, 1102         |    12ms  10.2M 169.2K |    11ms  10.2M 169.2K
 Y15.D15   | 13882464, 11171160 |    47ms  83.1M  43.5K |    35ms  65.6M  43.5K
 Y15.D16   | 103, 405           |    12ms  25.8M  74.1K |    13ms  26.0M  74.1K
 Y15.D17   | 1638, 17           |    35ms  55.8M  70.3K |    33ms  56.0M  71.6K
 Y15.D18   | 1061, 1006         |   128ms 491.0M  69.3K |   135ms 490.9M  69.3K
 Y15.D19   | 518, 1             |    35ms  17.0M   2.9M |    11ms  95.7K  60.6K  <-- expected: 518, 0
-----------+--------------------+-----------------------+-----------------------
 Platform: darwin, x86_64, v10.15.7, MacBookPro12,1
 CPU:      Intel(R) Core(TM) i7-5557U CPU @ 3.10GHz, 2 cores
 RAM:      16.0G @ ?MHz
 Compiler: ghc-8.10 (x86_64)
```

To run tasks with a particular prefix:
```
$ ./scripts/build-exec.sh Y15.D06V
------------+--------------------+- day 1 ---------------+- day 2 ---------------
 day       | answers            |    time  alloc   peak |    time  alloc   peak
-----------+--------------------+-----------------------+-----------------------
 Y15.D06VB | 400410, 15343601   |  2472ms 643.5M 598.1M |  3019ms 942.7M 598.1M
 Y15.D06VR | 400410, 15343601   |    47ms  63.4M   3.9M |    48ms  84.0M   2.0M
 Y15.D06VS | 400410, 15343601   |    38ms  13.5M   1.1M |    47ms  15.4M   2.0M
 Y15.D06VU | 400410, 15343601   |    38ms  13.5M   1.1M |    37ms  15.4M   2.0M
-----------+--------------------+-----------------------+-----------------------
 Platform: darwin, x86_64, v10.15.7, MacBookPro12,1
 CPU:      Intel(R) Core(TM) i7-5557U CPU @ 3.10GHz, 2 cores
 RAM:      16.0G @ ?MHz
 Compiler: ghc-8.10 (x86_64)
```

## Running a solver using stdin/stdout
```bash
$ cat res/Y15/D01.txt | ./scripts/build-exec.sh runday Y15.D01 0
  ^^^^^^^^^^^^^^^^^^^                                            - input
                                                       ^^^^^^^   - year + day
                                                               ^ - solver index (0 or 1)
<prints result to stdout>
```

## Running on Raspberry Pi

In `stack.yaml` replace line `resolver: ...` with:
```
resolver: lts-13.11
jobs: 1
```

Then run with:
```
$ ./scripts/build-exec.sh
```

## Running on Windows

```
> stack build
> stack exec adventofcode-exe
```
