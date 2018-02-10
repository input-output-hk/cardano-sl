# Benchmark Environment

User Story

| Issue   | Owner            | Sprint                       |
|---------|------------------|------------------------------|
| CBR-23  | Denis Shevchenko | Cardano #57: Novaya Nadezhda |

Computer

| OS                       | CPU                     | RAM                 |
|--------------------------|-------------------------|---------------------|
| Debian Linux 9.3 (64bit) | Core i7-7500U @ 2.70GHz | 16 GB DDR4 2133 MHz |

Bench Tool

| Package | Version |
|---------|---------|
| gauge   | 0.2.1   |

Code

| Feature Branch               | Base Branch | Commit    |
|------------------------------|-------------|-----------|
| `feature/cbr23-wallet-bench` | `master`    | e66d16bbf |

Build

| Script                | RTS Options | Use Nix |
|-----------------------|-------------|---------|
| `build/cardano-sl.sh` | `-N2`       | No      |

Launch

| Script                           | Number of nodes | Connect to Mainnet |
|----------------------------------|-----------------|--------------------|
| `launch/demo-with-wallet-api.sh` | 4               | No                 |

# Benchmark Launch

## Launch Command

Example of complete command:

```
$ stack bench cardano-sl-wallet --benchmark-arguments  \
    "--tls-pub-cert=$PWD/scripts/tls-files/ca.crt      \
     --tls-priv-key=$PWD/scripts/tls-files/server.key  \
     --wal-conf=$PWD/wallet/bench/config/Wallets.yaml  \
     --ep-conf=$PWD/wallet/bench/config/Endpoints.csv"
```

Run:

```
$ stack bench cardano-sl-wallet --benchmark-arguments "--help"
```

to see description of supported arguments.

## Configuration

There are two different configuration files:

1. `Wallets.yaml` Contains wallets, accounts and addresses we are using during benchmarking.
2. `Endpoints.csv` Contains a list of Wallet Web API endpoints we want to benchmark. By
        default all listed benchmarks will be launched sequentially, one by one.

## Preparing

To make benchmarking more realistic, wallet database was generated, using `dbgen` tool.
Please follow [these instructions](https://iohk.myjetbrains.com/youtrack/issue/CSL-2249\#comment=93-17408)
to reproduce it on your local computer.

As a result we have 1 wallet with 80k addresses in it. Most of these addresses contains constant
amount of money.

## Measurements Explanation

Package `gauge` returns an output with basic measurements, for example:

```
benchmarking GetHistoryBench ... took 61.18 s, total 56 iterations
benchmarked GetHistoryBench
time                 1.069 s    (986.0 ms .. 1.174 s)
                     0.985 R²   (0.966 R² .. 0.996 R²)
mean                 1.121 s    (1.082 s .. 1.179 s)
std dev              80.42 ms   (47.68 ms .. 128.8 ms)
variance introduced by outliers: 19% (moderately inflated)
```

where:

1. Value of `time` corresponds to **Time, ms** in tables below.
2. Value of `mean` corresponds to **Mean, ms** in tables below.
3. Percentage of `variance introduced by outliers` corresponds to **Variance, %** in tables below.
4. Number of `iterations` is an actual number of requests sent to endpoint.
   All benchmarks took from 56 to 211 iterations.


# `GetHistory`

## Empty tx history

| Wallets | Accounts | Addresses | **Time, ms** | **Mean, ms** |
|---------|----------|-----------|-------------:|-------------:|
| 1       | 1        | 1         | **101.1**    | **101.8**    |
| 1       | 2        | 80k       | **637**      | **641.2**    |

## Non-empty tx history, 20k addresses

| Wallets | Accounts | Transactions | **Time, ms**             | **Mean, ms**             | **Variance, %** |
|---------|----------|--------------|-------------------------:|-------------------------:|----------------:|
| 1       | 2        | 3k           | **864.3** (8134 .. 9395) | **853.6** (8311 .. 8770) | 82              |

## Non-empty tx history, 81k addresses

| Wallets | Accounts | Transactions | **Time, ms**            | **Mean, ms**            | **Variance, %** |
|---------|----------|--------------|------------------------:|------------------------:|----------------:|
| 1       | 2        | 435          | **691.3**               | **882.5**               | 29              |
| 1       | 2        | 1.7k         | **1106** (1081 .. 1129) | **1091** (1062 .. 1109) | 19              |


# `GetWallet`

## Empty tx history

| Wallets | Accounts | Addresses | **Time, ms** | **Mean, ms** |
|---------|----------|-----------|-------------:|-------------:|
| 1       | 1        | 1         | **126.9**    | **126.1**    |
| 1       | 2        | 80k       | **3641**     | **3407**     |

## Non-empty tx history, 80k addresses

| Wallets | Accounts | Transactions | **Time, ms**            | **Mean, ms**            | **Variance, %** |
|---------|----------|--------------|------------------------:|------------------------:|----------------:|
| 1       | 2        | 435          | **3625**                | **3435**                | none            |
| 1       | 2        | 1.7k         | **4521** (3894 .. 5412) | **3892** (3688 .. 4298) | 38              |
| 1       | 2        | 1.8k         | **3588** (3475 .. 3761) | **3513** (3432 .. 3593) | none            |


# `GetWallets`

## Empty tx history

| Wallets | Accounts | Addresses | **Time, ms** | **Mean, ms** |
|---------|----------|-----------|-------------:|-------------:|
| 1       | 1        | 1         | **124.9**    | **125.2**    |
| 1       | 2        | 80k       | **3115**     | **3256**     |

## Non-empty tx history, 81k addresses

| Wallets | Accounts | Transactions | **Time, ms**            | **Mean, ms**            | **Variance, %** |
|---------|----------|--------------|------------------------:|------------------------:|----------------:|
| 1       | 2        | 435          | **3485** (3390 .. 3574) | **3488** (3435 .. 3546) | none            |
| 1       | 2        | 1.7k         | **3924** (3271 .. 4469) | **3623** (3438 .. 3883) | 29              |


# `IsValidAddress`

## Empty/non-empty tx history

| Wallets | Accounts | Addresses | **Time, ms**               | **Mean, ms**               | **Variance, %** |
|---------|----------|-----------|---------------------------:|---------------------------:|----------------:|
| 1       | 1        | 1         | **91.61** (76.87 .. 100.8) | **93.27** (87.16 .. 110.9) | 58              |
| 1       | 2        | 80k       | **92.46** (85.07 .. 100.6) | **86.66** (83.95 .. 91.42) | 17              |
| 1       | 2        | 81k       | **93.64** (81.87 .. 101.6) | **94.77** (90.11 .. 103.8) | 38              |


# `NewAddress`

## Empty/non-empty tx history

| Wallets | Accounts | Addresses | **Time, ms**               | **Mean, ms**               | **Variance, %** |
|---------|----------|-----------|---------------------------:|---------------------------:|----------------:|
| 1       | 1        | 1         | **139.2** (129.8 .. 149.5) | **144.7** (138.7 .. 161.1) | 38              |
| 1       | 2        | 80k       | **141.2** (137.0 .. 146.1) | **141.7** (136.6 .. 144.8) | none            |


# `GetSyncProgress`

## Empty/non-empty tx history

| Wallets | Accounts | Addresses | **Time, ms**               | **Mean, ms**               | **Variance, %** |
|---------|----------|-----------|---------------------------:|---------------------------:|----------------:|
| 1       | 1        | 1         | **84.95** (78.53 .. 92.20) | **94.46** (90.79 .. 101.1) | 28              |
| 1       | 2        | 80k       | **91.11** (88.56 .. 94.92) | **87.76** (85.73 .. 89.47) | none            |


# `NewPayment`

## Empty tx history before starting

| Wallets | Accounts | Addresses | **Time, ms** | **Mean, ms** |
|---------|----------|-----------|-------------:|-------------:|
| 1       | 2        | 80k       | **7680**     | **8115**     |

## Non-empty tx history, 20k addresses

| Wallets | Accounts | Transactions | **Time, ms**            | **Mean, ms**            | **Variance, %** |
|---------|----------|--------------|------------------------:|------------------------:|----------------:|
| 1       | 2        | 3k           | **6203** (6195 .. 6209) | **6189** (6181 .. 6194) | none            |

## Non-empty tx history, 80k addresses

| Wallets | Accounts | Transactions | **Time, ms**            | **Mean, ms**            | **Variance, %** |
|---------|----------|--------------|------------------------:|------------------------:|----------------:|
| 1       | 2        | 435          | **8142** (8081 .. 8212) | **8171** (8109 .. 8245) | none            |
| 1       | 2        | 1.2k         | **8487** (8294 .. 8715) | **8643** (8502 .. 8947) | 19              |
| 1       | 2        | 1.7k         | **8593** (8253 .. 8871) | **8160** (8017 .. 8443) | 19              |
| 1       | 2        | 1.7k         | **9242** (8165 .. 1048) | **8382** (8193 .. 8813) | 33              |
