# Benchmarking Results (Local Cluster)

##

One wallet, one account

Benchmark cardano-sl-wallet-bench: RUNNING...
benchmarking NewAddressBench ... took 62.02 s, total 477 iterations
benchmarked NewAddressBench
time                 130.5 ms   (129.2 ms .. 131.9 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 129.8 ms   (128.9 ms .. 130.9 ms)
std dev              2.921 ms   (2.343 ms .. 4.376 ms)

Benchmark cardano-sl-wallet-bench: FINISH

Time isn't grow when number of (already created) wallets grows.

##

benchmarking IsValidAddressBench ... took 43.15 s, total 477 iterations
benchmarked IsValidAddressBench
time                 92.32 ms   (90.77 ms .. 94.12 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 89.60 ms   (88.24 ms .. 90.96 ms)
std dev              3.828 ms   (3.108 ms .. 5.131 ms)
variance introduced by outliers: 19% (moderately inflated)

##

benchmarking GetSyncProgressBench ... took 43.89 s, total 477 iterations
benchmarked GetSyncProgressBench
time                 92.99 ms   (90.91 ms .. 95.16 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 91.01 ms   (88.43 ms .. 92.37 ms)
std dev              5.318 ms   (3.664 ms .. 8.247 ms)
variance introduced by outliers: 29% (moderately inflated)

Benchmark cardano-sl-wallet-bench: FINISH

##

For EMPTY history:

Benchmark cardano-sl-wallet-bench: RUNNING...
benchmarking GetHistoryBench ... took 21.45 s, total 211 iterations
benchmarked GetHistoryBench
time                 101.1 ms   (97.33 ms .. 104.6 ms)
                     0.994 R²   (0.987 R² .. 0.998 R²)
mean                 101.8 ms   (99.95 ms .. 104.5 ms)
std dev              5.126 ms   (3.428 ms .. 8.087 ms)
variance introduced by outliers: 19% (moderately inflated)

Benchmark cardano-sl-wallet-bench: FINISH

##

For 1 wallet

Benchmark cardano-sl-wallet-bench: RUNNING...
benchmarking GetWalletBench ... took 26.56 s, total 211 iterations
benchmarked GetWalletBench
time                 126.9 ms   (124.5 ms .. 129.3 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 126.1 ms   (124.7 ms .. 127.4 ms)
std dev              3.091 ms   (2.485 ms .. 3.885 ms)

##

For 1 wallet

Benchmark cardano-sl-wallet-bench: RUNNING...
benchmarking GetWalletsBench ... took 26.21 s, total 211 iterations
benchmarked GetWalletsBench
time                 124.9 ms   (122.8 ms .. 126.8 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 125.2 ms   (124.1 ms .. 127.1 ms)
std dev              3.245 ms   (2.016 ms .. 4.750 ms)

###################################################################################

System start time is 1517833990000000
