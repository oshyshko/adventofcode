# adventofcode [![ci](https://github.com/oshyshko/adventofcode/actions/workflows/ci.yaml/badge.svg)](https://github.com/oshyshko/adventofcode/actions/workflows/ci.yaml)

Haskell solutions for http://adventofcode.com/

## Running

```
$ ./scripts/build-exec.sh fast
-----------+---------------------+- part 1 ---------------------+- part 2 ---------------------
 day       | answers             |    time allocs maxhea maxmem |    time allocs maxhea maxmem
-----------+---------------------+------------------------------+------------------------------
 Y15.D01   | 138, 1771           |    11ms 375.9K  52.1K   2.0M |    11ms 622.3K  60.2K   2.0M
 Y15.D02   | 1586300, 3737498    |    13ms  13.8M  66.6K   2.0M |    12ms  13.8M  66.6K   2.0M
 Y15.D03   | 2565, 2639          |    68ms   1.8M  52.1K   2.0M |    71ms   4.1M 464.4K   3.0M
 Y15.D04   | 117946, 3938038     |    52ms  60.6M  43.5K   2.0M |  1411ms   2.2G  43.5K   2.0M
 Y15.D05   | 236, 51             |    13ms   3.0M  76.7K   2.0M |    13ms   4.2M  52.1K   2.0M
 Y15.D06   | 400410, 15343601    |    47ms  13.5M   1.1M   4.0M |    46ms  15.5M   2.0M   6.0M
 Y15.D07   | 3176, 14710         |    11ms   7.5M  84.7K   2.0M |    11ms   7.9M 115.9K   2.0M
 Y15.D08   | 1333, 2046          |    16ms   9.9M 220.6K   2.0M |    12ms  15.2M 131.4K   2.0M
 Y15.D09   | 117, 909            |    94ms  22.5M  83.6K   3.0M |    95ms  22.5M  83.6K   3.0M
 Y15.D10   | 360154, 5103798     |   151ms 451.6M  84.6K   3.0M |  1682ms   6.2G  87.4K   3.0M
 Y15.D11   | vzbxxyzz, vzcaabcc  |    26ms  35.5M  43.5K   2.0M |   106ms 175.0M  43.5K   3.0M
 Y15.D12   | 111754, 65402       |    13ms   9.5M 183.1K   3.0M |    13ms   9.1M 183.1K   3.0M
 Y15.D13   | 618, 601            |   148ms 160.5M  86.7K   3.0M |  1737ms   1.5G  88.1K   3.0M
 Y15.D14   | 2640, 1102          |    11ms  10.2M 167.7K   3.0M |    12ms  10.2M 167.7K   3.0M
 Y15.D15   | 13882464, 11171160  |   193ms  83.1M  43.5K   3.0M |    45ms  65.6M  43.5K   3.0M
 Y15.D16   | 103, 405            |    53ms  25.9M  72.8K   2.0M |    42ms  26.1M  72.8K   2.0M
 Y15.D17   | 1638, 17            |    50ms  55.8M  68.9K   2.0M |    34ms  56.0M  70.1K   2.0M
 Y15.D18   | 1061, 1006          |   287ms 491.0M  68.9K   2.0M |   181ms 490.9M  68.9K   2.0M
 Y15.D19   | 518, 200            |    61ms  23.8M   2.8M   7.0M |   392ms 470.6M  17.8M  38.0M
 Y15.D20   | 831600, 884520      |   558ms 873.2M  43.5K   2.0M |  1203ms   4.1G  43.5K   2.0M
 Y15.D21   | 78, 148             |    12ms 468.5K  52.1K   2.0M |    13ms 438.8K  52.1K   2.0M
 Y21.D01   | 1477, 1523          |    12ms  10.6M  53.6K   2.0M |    22ms  11.9M  58.5K   2.0M
 Y21.D02   | 1947824, 1813062561 |    12ms  10.8M  74.9K   2.0M |    11ms  10.8M  74.9K   2.0M
 Y21.D03   | 738234, 3969126     |    22ms   5.8M 613.0K   3.0M |    12ms   3.5M 358.7K   2.0M
 Y21.D04   | 31424, 23042        |    22ms  30.7M  84.9K   2.0M |    25ms  38.6M  84.9K   2.0M
-----------+---------------------+------------------------------+------------------------------
 Platform: darwin, x86_64, v10.15.7, MacBookPro12,1
 CPU:      Intel(R) Core(TM) i7-5557U CPU @ 3.10GHz, 2 cores
 RAM:      16.0G @ ?MHz
 Compiler: ghc-8.10 (x86_64)
```

To run tasks with a particular prefix:
```
$ ./scripts/build-exec.sh Y15.D06V
-----------+---------------------+- part 1 ---------------------+- part 2 ---------------------
 day       | answers             |    time allocs maxhea maxmem |    time allocs maxhea maxmem
-----------+---------------------+------------------------------+------------------------------
 Y15.D06VB | 400410, 15343601    |  4802ms 643.6M 594.0M   1.2G |  3626ms 942.8M 594.0M   1.2G
 Y15.D06VR | 400410, 15343601    |    57ms  63.5M   3.9M  10.0M |    56ms  84.1M   2.0M   6.0M
 Y15.D06VS | 400410, 15343601    |    34ms  13.5M   1.1M   4.0M |    48ms  15.5M   2.0M   6.0M
 Y15.D06VU | 400410, 15343601    |    36ms  13.5M   1.1M   4.0M |    46ms  15.5M   2.0M   6.0M
-----------+---------------------+------------------------------+------------------------------
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
