# adventofcode
Haskell solutions for http://adventofcode.com/

## Running

```
$ ./scripts/build-exec.sh
-----------+--------------------+- day 1 ---------------+- day 2 ---------------
 day       | answers            |    time  alloc   peak |    time  alloc   peak
-----------+--------------------+-----------------------+-----------------------
 Y15.D01   | 138, 1771          |    15ms 371.0K  52.7K |    15ms 631.2K  60.8K
 Y15.D02   | 1586300, 3737498   |    15ms  14.1M  67.7K |    13ms  14.1M  67.7K
 Y15.D03   | 2565, 2639         |    50ms   1.8M  52.7K |    64ms   4.1M 473.4K
 Y15.D04   | 117946, 3938038    |   241ms   1.6G  46.4K |  7699ms  53.1G  46.4K
 Y15.D05   | 236, 51            |    15ms   3.0M  76.5K |    13ms   4.2M  52.7K
 Y15.D06IO | 400410, 15343601   |   244ms 381.2M  20.3M |   254ms 680.5M  23.0M
 Y15.D06MS | 400410, 15343601   | 10034ms  18.6G  58.6M | 11795ms  19.0G  65.3M
 Y15.D06ST | 400410, 15343601   |   149ms 327.2M   7.8M |   157ms 626.5M   7.8M
 Y15.D07   | 3176, 14710        |    15ms   7.3M  85.7K |    15ms   7.6M 116.9K
 Y15.D08   | 1333, 2046         |    14ms   9.8M 221.3K |    13ms  15.2M 131.7K
 Y15.D09   | 117, 909           |    97ms  29.0M  85.8K |   100ms  29.0M  85.8K
 Y15.D10   | 360154, 5103798    |   110ms 475.2M  85.5K |  1467ms   6.6G  88.7K
 Y15.D11   | vzbxxyzz, vzcaabcc |    25ms  35.5M  43.7K |    99ms 175.0M  43.7K
 Y15.D12   | 111754, 65402      |    15ms   8.8M 206.2K |    15ms   8.3M 206.2K
 Y15.D13   | 618, 601           |   139ms 160.5M  87.4K |  1089ms   1.5G  88.7K
 Y15.D14   | 2640, 1102         |    15ms  10.7M 169.9K |    15ms  10.7M 169.9K
 Y15.D15   | 13882464, 11171160 |    47ms  83.2M  43.7K |    26ms  65.7M  43.7K
 Y15.D16   | 103, 405           |    15ms  24.8M  72.9K |    13ms  24.9M  72.9K
 Y15.D17   | 1638, 17           |    26ms  55.8M  70.6K |    26ms  56.0M  71.9K
 Y15.D18IO | 1061, 1006         |   376ms 735.3M 887.2K |   401ms 735.2M 877.4K
 Y15.D18ST | 1061, 1006         |   382ms 734.5M 900.6K |   377ms 734.5M 902.4K
-----------+--------------------+-----------------------+-----------------------
 Darwin x86_64 i386
 ```

## Runnig On Raspberry Pi

In `stack.yaml` replace line `resolver: ...` with:
```
resolver: lts-13.11
jobs: 1
```

Then run with:
```
$ ./scripts/build-exec.sh
-----------+--------------------+- day 1 ---------------+- day 2 ---------------
 day       | answers            |    time  alloc   peak |    time  alloc   peak
-----------+--------------------+-----------------------+-----------------------
 Y15.D01   | 138, 1771          |     3ms 215.5K  50.4K |     3ms 345.2K  58.5K
 Y15.D02   | 1586300, 3737498   |    25ms   7.1M  51.5K |    25ms   7.1M  51.5K
 Y15.D03   | 2565, 2639         |   215ms 967.0K  50.4K |   237ms   2.1M  41.9K
 Y15.D04   | 117946, 3938038    |   975ms   1.1G  44.2K | 29640ms   3.7G  44.2K
 Y15.D05   | 236, 51            |    11ms   1.5M  50.4K |    12ms   2.1M  50.4K
 Y15.D06IO | 400410, 15343601   |   817ms 190.6M  11.1M |   905ms 340.3M  13.3M
 Y15.D06MS | 400410, 15343601   | 34852ms   1.3G  29.3M | 37846ms   1.5G  32.7M
 Y15.D06ST | 400410, 15343601   |   553ms 163.7M   3.9M |   617ms 313.3M   3.9M
 Y15.D07   | 3176, 14710        |    11ms   3.7M  82.3K |    11ms   3.9M  82.3K
 Y15.D08   | 1333, 2046         |    11ms   4.9M 109.0K |    16ms   7.6M 107.7K
 Y15.D09   | 117, 909           |   289ms  14.5M  41.9K |   285ms  14.5M  41.9K
 Y15.D10   | 360154, 5103798    |   382ms 237.6M  78.9K |  5346ms   3.3G  80.3K
 Y15.D11   | vzbxxyzz, vzcaabcc |    63ms  17.8M  41.9K |   301ms  87.5M  41.9K
 Y15.D12   | 111754, 65402      |    38ms   4.3M 311.6K |    33ms   4.1M 311.6K
 Y15.D13   | 618, 601           |   414ms  85.2M  46.3K |  3373ms 834.0M  46.9K
 Y15.D14   | 2640, 1102         |    18ms   5.4M 113.5K |    18ms   5.4M 113.5K
 Y15.D15   | 13882464, 11171160 |    91ms  41.6M  41.9K |    43ms  32.9M  41.9K
 Y15.D16   | 103, 405           |    21ms  12.5M  69.2K |    21ms  12.5M  69.2K
 Y15.D17   | 1638, 17           |    53ms  27.9M  41.9K |    53ms  28.0M  41.9K
 Y15.D18IO | 1061, 1006         |  1164ms 367.7M 295.3K |  1182ms 367.6M 299.3K
 Y15.D18ST | 1061, 1006         |  1186ms 367.3M 339.4K |  1206ms 367.3M 342.1K
-----------+--------------------+-----------------------+-----------------------
 Linux armv7l unknown
```