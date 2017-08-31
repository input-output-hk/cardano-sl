# How to connect to cluster

We have two Cardano SL working environments: **staging** and **production**. Each environment contains one ore more clusters. To connect node to the cluster you have to:

1. build a node properly,
2. run corresponding script from `scripts/launch/connect-to-cluster` subdirectory.

For example, there's `testnet-staging-0.6` cluster. So first you have to build a node:

```
$ ./scripts/build/cardano-sl.sh --tns
```

After that run a node:

```
$ ./scripts/launch/connect-to-cluster/testnet-staging-0.6.sh
```
