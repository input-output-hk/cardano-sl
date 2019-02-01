# script-runner

This module contains scripts to perform tests at the cluster level - running clusters of nodes, issuing update proposals to them, restarting them, etc. Some of the code here was extracted from the `auxx` package and reworked.

### Usage

See the `stack-gui` and `stack-test` scripts which contain usage commands.

### Testing OBFT

`script-runner` launches nodes using the `dev:` key of `lib/configuration.yaml`. We can start in OBFT era by applying a patch such as
```
diff --git a/lib/configuration.yaml b/lib/configuration.yaml
index b55039730b..f67e581929 100644
--- a/lib/configuration.yaml
+++ b/lib/configuration.yaml
@@ -48,7 +48,7 @@ dev: &dev
             txSizeLinear:
               a: 155381 # absolute minimal fees per transaction
               b: 43.946 # additional minimal fees per byte of transaction size
-          unlockStakeEpoch: 18446744073709551615 # last epoch (maxBound @Word64)
+          unlockStakeEpoch: 9999999999999999999
         protocolConstants: &dev_core_genesis_spec_protocolConstants
           k: 2
           protocolMagic: 55550001
```
and then running a test e.g. `cd script-runner && ./stack-test test4.1` which ensures that we can issue update proposals and see them adopted in OBFT era.
