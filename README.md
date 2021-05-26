# adventofcode [![ci](https://github.com/oshyshko/adventofcode/actions/workflows/ci.yaml/badge.svg)](https:/github.com/oshyshko/adventofcode/actions/workflows/ci.yaml)

Haskell solutions for http://adventofcode.com/

## Running

```
$ ./scripts/build-exec.sh
-----------+--------------------+- day 1 ---------------+- day 2 ---------------
 day       | answers            |    time  alloc   peak |    time  alloc   peak
-----------+--------------------+-----------------------+-----------------------
 Y15.D01   | 138, 1771          |    15ms 376.2K  52.7K |    15ms 636.4K  60.8K
 Y15.D02   | 1586300, 3737498   |    15ms  14.1M  67.9K |    15ms  14.1M  67.9K
 Y15.D03   | 2565, 2639         |    61ms   1.8M  52.7K |    62ms   4.1M 472.4K
 Y15.D04   | 117946, 3938038    |    67ms  80.4M  43.7K |  1301ms   2.9G  43.7K
 Y15.D05   | 236, 51            |    14ms   3.0M  77.3K |    14ms   4.2M  52.7K
 Y15.D06AI | 400410, 15343601   |   225ms  88.6M  43.7M |   172ms  67.2M  29.8M
 Y15.D06AS | 400410, 15343601   |   217ms  88.6M  43.8M |   180ms  67.2M  31.6M
 Y15.D06MH | 400410, 15343601   | 22415ms  12.9G  54.4M | 30050ms  18.6G  68.3M
 Y15.D06MI | 400410, 15343601   |  5846ms  15.6G  58.5M |  6385ms  16.2G  72.5M
 Y15.D06MS | 400410, 15343601   |  8292ms  17.8G  58.5M |  9355ms  18.3G  72.5M
 Y15.D06VI | 400410, 15343601   |    50ms  13.2M   1.1M |    47ms  15.1M   2.0M
 Y15.D06VS | 400410, 15343601   |    53ms  13.2M   1.1M |    50ms  15.1M   2.0M
 Y15.D07   | 3176, 14710        |    15ms   7.3M  85.1K |    15ms   7.6M 116.5K
 Y15.D08   | 1333, 2046         |    15ms   9.8M 221.6K |    15ms  15.2M 132.0K
 Y15.D09   | 117, 909           |   101ms  29.0M  85.9K |    87ms  29.0M  85.9K
 Y15.D10   | 360154, 5103798    |   110ms 475.2M  85.7K |  1450ms   6.6G  88.7K
 Y15.D11   | vzbxxyzz, vzcaabcc |    27ms  35.5M  43.7K |    97ms 175.0M  43.7K
 Y15.D12   | 111754, 65402      |    15ms   8.8M 205.8K |    15ms   8.3M 205.8K
 Y15.D13   | 618, 601           |   137ms 160.5M  87.2K |  1102ms   1.5G  88.7K
 Y15.D14   | 2640, 1102         |    15ms  10.7M 169.9K |    15ms  10.7M 169.9K
 Y15.D15   | 13882464, 11171160 |    48ms  83.2M  43.7K |    37ms  65.7M  43.7K
 Y15.D16   | 103, 405           |    15ms  24.8M  75.0K |    15ms  24.9M  75.0K
 Y15.D17   | 1638, 17           |    28ms  55.8M  70.7K |    27ms  56.0M  72.0K
 Y15.D18   | 1061, 1006         |   302ms 705.9M 755.2K |   308ms 705.8M 755.1K
-----------+--------------------+-----------------------+-----------------------
 Platform: darwin, x86_64, v10.15.2, MacBookPro12,1
 CPU:      Intel(R) Core(TM) i7-5557U CPU @ 3.10GHz, 2 cores
 RAM:      16.0G @ ?MHz
 Compiler: ghc-8.6
```

To run tasks with a particular prefix:
```
$ ./scripts/build-exec.sh Y15.D06
-----------+--------------------+- day 1 ---------------+- day 2 ---------------
 day       | answers            |    time  alloc   peak |    time  alloc   peak
-----------+--------------------+-----------------------+-----------------------
 Y15.D06AI | 400410, 15343601   |   225ms  88.6M  43.7M |   172ms  67.2M  29.8M
 Y15.D06AS | 400410, 15343601   |   217ms  88.6M  43.8M |   180ms  67.2M  31.6M
 Y15.D06MH | 400410, 15343601   | 22415ms  12.9G  54.4M | 30050ms  18.6G  68.3M
 Y15.D06MI | 400410, 15343601   |  5846ms  15.6G  58.5M |  6385ms  16.2G  72.5M
 Y15.D06MS | 400410, 15343601   |  8292ms  17.8G  58.5M |  9355ms  18.3G  72.5M
 Y15.D06VI | 400410, 15343601   |    50ms  13.2M   1.1M |    47ms  15.1M   2.0M
 Y15.D06VS | 400410, 15343601   |    53ms  13.2M   1.1M |    50ms  15.1M   2.0M
-----------+--------------------+-----------------------+-----------------------
 Darwin x86_64 i386
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
