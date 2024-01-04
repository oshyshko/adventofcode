# adventofcode [![ci](https://github.com/oshyshko/adventofcode/actions/workflows/ci.yaml/badge.svg)](https://github.com/oshyshko/adventofcode/actions/workflows/ci.yaml)

Haskell solutions for http://adventofcode.com/

## Running

```
$ ./scripts/build-exec.sh
-----------+-----------------------+- part 1 ---------------------+- part 2 ---------------------
 day       | answers               |    time allocs maxhea maxmem |    time allocs maxhea maxmem
-----------+-----------------------+------------------------------+------------------------------
 Y15.D01   | 138, 1771             |    13ms 382.0K  52.0K   2.0M |    13ms 614.3K  60.2K   2.0M
 Y15.D02   | 1586300, 3737498      |    11ms  13.9M  66.5K   2.0M |    11ms  13.9M  66.5K   2.0M
 Y15.D03   | 2565, 2639            |    13ms   5.1M 158.7K   2.0M |    13ms   7.3M 357.6K   3.0M
 Y15.D04   | 117946, 3938038       |    52ms  58.0M  43.4K   2.0M |  1267ms   2.1G  43.4K   2.0M
 Y15.D05   | 236, 51               |    12ms   3.0M  77.8K   2.0M |    12ms   4.2M  52.0K   2.0M
 Y15.D06   | 400410, 15343601      |    48ms  15.9M   1.1M   3.0M |    34ms  10.8M   2.1M   4.0M
 Y15.D07   | 3176, 14710           |    14ms   4.6M 173.0K   2.0M |    13ms   5.0M 173.0K   2.0M
 Y15.D08   | 1333, 2046            |    12ms   5.9M 220.7K   2.0M |    12ms   9.0M 170.0K   2.0M
 Y15.D09   | 117, 909              |    89ms  22.3M  84.0K   3.0M |    73ms  22.3M  84.0K   3.0M
 Y15.D10   | 360154, 5103798       |   120ms 451.6M  84.4K   3.0M |  1445ms   6.2G  87.1K   3.0M
 Y15.D11   | vzbxxyzz, vzcaabcc    |    23ms  35.5M  43.4K   2.0M |    95ms 175.0M  43.4K   3.0M
 Y15.D12   | 111754, 65402         |    13ms  10.8M 167.9K   3.0M |    12ms  10.3M 167.9K   3.0M
 Y15.D13   | 618, 601              |   144ms 159.8M  86.9K   3.0M |  1104ms   1.5G  88.2K   3.0M
 Y15.D14   | 2640, 1102            |    11ms  10.1M 167.2K   3.0M |    12ms  10.1M 167.2K   3.0M
 Y15.D15   | 13882464, 11171160    |    49ms  83.0M  43.4K   3.0M |    35ms  65.5M  43.4K   3.0M
 Y15.D16   | 103, 405              |    11ms  15.8M 116.7K   2.0M |    12ms  15.9M 116.7K   2.0M
 Y15.D17   | 1638, 17              |    26ms  55.8M  68.7K   2.0M |    24ms  56.0M  70.0K   2.0M
 Y15.D18   | 1061, 1006            |   123ms 486.1M 118.0K   2.0M |   143ms 486.1M 118.0K   2.0M
 Y15.D19   | 518, 200              |    35ms  23.4M   2.6M   7.0M |   327ms 470.2M  17.8M  38.0M
 Y15.D20   | 831600, 884520        |   479ms 873.2M  43.4K   2.0M |  1037ms   4.1G  43.4K   2.0M
 Y15.D21   | 78, 148               |    13ms 463.2K  52.0K   2.0M |    13ms 433.5K  52.0K   2.0M
 Y21.D01   | 1477, 1523            |    11ms  10.7M  54.2K   2.0M |    12ms  12.0M  58.6K   2.0M
 Y21.D02   | 1947824, 1813062561   |    11ms   8.2M 122.4K   2.0M |    12ms   8.2M 122.4K   2.0M
 Y21.D03   | 738234, 3969126       |    11ms   5.8M 613.9K   3.0M |    12ms   3.5M 358.4K   2.0M
 Y21.D04   | 31424, 23042          |    24ms  25.2M 118.2K   3.0M |    12ms  33.1M 118.2K   3.0M
 Y21.D05   | 6189, 19164           |    23ms  25.2M   2.1M   4.0M |    33ms  32.9M   2.1M   4.0M
 Y21.D06   | 346063, 1572358335990 |    13ms   1.6M  52.0K   2.0M |    13ms   1.9M  52.0K   2.0M
 Y21.D07   | 339321, 95476244      |    35ms  93.9M  69.9K   2.0M |    37ms 123.4M  69.9K   2.0M
 Y21.D08   | 476, 1011823          |    11ms  11.7M 190.8K   3.0M |    34ms  20.4M 606.4K   3.0M
 Y21.D09   | 564, 1038240          |    13ms   2.4M 199.1K   2.0M |    11ms   4.6M 199.1K   2.0M
 Y21.D10   | 296535, 4245130838    |    13ms   1.8M  52.0K   2.0M |    14ms   1.8M  52.0K   2.0M
 Y21.D11   | 1640, 312             |    13ms   7.0M  43.4K   2.0M |    22ms  20.5M  43.4K   2.0M
 Y21.D12   | 4970, 137948          |    12ms   8.9M  73.2K   2.0M |   209ms 265.2M  73.9K   3.0M
 Y21.D13   | 666, CJHAZHKU         |    11ms  11.2M 202.0K   2.0M |    12ms  12.6M 202.0K   3.0M
 Y21.D14   | 2874, 5208377027195   |    12ms   1.2M  52.0K   2.0M |    12ms   3.6M  92.2K   2.0M
 Y21.D15   | 366, 2829             |    22ms  21.3M 228.5K   3.0M |   506ms 658.9M  12.4M  30.0M
 Y21.D16   | 886, 184487454837     |    11ms  18.9M 138.0K   2.0M |    13ms  18.9M 138.0K   2.0M
 Y21.D17   | 7626, 2032            |    12ms   8.8M  45.0K   2.0M |    25ms   8.6M  45.0K   2.0M
 Y21.D18   | 3654, 4578            |    12ms  15.6M 115.1K   2.0M |   101ms 265.4M 115.6K   3.0M
 Y21.D19   | 425, 13354            |   208ms 224.0M 898.3K   4.0M |   207ms 223.7M 898.3K   4.0M
-----------+-----------------------+------------------------------+------------------------------
 Platform: darwin, x86_64, v10.15.7, MacBookPro12,1
 CPU:      Intel(R) Core(TM) i7-5557U CPU @ 3.10GHz, 2 cores
 RAM:      16.0G @ 1867MHz
 Compiler: ghc-9.0 (x86_64)
 ```

To run tasks with a particular prefix (also to include alternative solvers):
```
$ ./scripts/build-exec.sh Y15.D06M alts
-----------+-----------------------+- part 1 ---------------------+- part 2 ---------------------
 day       | answers               |    time allocs maxhea maxmem |    time allocs maxhea maxmem
-----------+-----------------------+------------------------------+------------------------------
 Y15.D06MA | 400410, 15343601      |    57ms  15.1M 292.6K   2.0M |    40ms  10.8M   2.1M   4.0M
 Y15.D06MB | 400410, 15343601      |  2545ms 639.3M 533.8M   1.1G |  3261ms 947.7M 622.2M   1.2G
 Y15.D06MS | 400410, 15343601      |    34ms  12.7M   4.0M   6.0M |    33ms  10.8M   2.1M   4.0M
 Y15.D06MU | 400410, 15343601      |    47ms  15.9M   1.1M   3.0M |    44ms  10.8M   2.1M   4.0M
-----------+-----------------------+------------------------------+------------------------------
 Platform: darwin, x86_64, v10.15.7, MacBookPro12,1
 CPU:      Intel(R) Core(TM) i7-5557U CPU @ 3.10GHz, 2 cores
 RAM:      16.0G @ 1867MHz
 Compiler: ghc-9.0 (x86_64)
```

## Running a solver using stdin/stdout
```bash
$ ./scripts/build-exec.sh exec Y15.D01 1
                               ^^^^^^^   - year + day
                                       ^ - part (1 or 2)
<prints result to stdout>

$ cat res/Y15/D01.txt | ./scripts/build-exec.sh pipe Y15.D01 1
  ^^^^^^^^^^^^^^^^^^^                                          - input
                                                     ^^^^^^^   - year + day
                                                             ^ - part (1 or 2)
<prints result to stdout>
```

## Running on Windows

```
> stack build
> stack exec adventofcode-exe
```
