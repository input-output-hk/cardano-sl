# How to launch a local demo cluster with nix

1. Make sure nix is installed and IOHK binary cache is configured [nix setup] (https://github.com/input-output-hk/cardano-sl/blob/develop/docs/how-to/build-cardano-sl-and-daedalus-from-source-code.md#nix-build-mode-recommended)
2. To generate a script that will launch the demo cluster run `nix-build -A demoCluster -o launch_demo_cluster`
3. To generate a script that will launch demo cluster suitable for Daedalus development, run `nix-build -A demoClusterDaedalusDev -o launch_demo_cluster`
4. To launch cluster, run `./launch_demo_cluster`
5. To stop, hit `ctrl-c` and it will terminate all the nodes.
6. A `state-demo` state folder will be automatically created relative to your current
   working directory.
  * Logs will be found in `state-demo/logs`
  * TLS certs/keys will be found in `state-demo/tls`
  * 11 genesis wallets will be pre-loaded with 37 million Ada each
