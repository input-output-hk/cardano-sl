# About the Cardano SL Launcher

Launcher is a utility that is responsible for:
 * Launching a node or wallet
 * Executing updates (the given update archive is retrieved by a node)
 * Reporting to the report server in the case where a node or wallet finishes unexpectedly

For example, when an update is downloaded, Launcher will do the following:
 * Wait for the wallet to finish
 * Stop the node
 * Execute an update (to apply the update), delete the update archive
 * Relaunch the node or wallet

## Operation Modes

The launcher has two primary modes of operation: the server mode
and the client mode. The server mode is used when the launcher is used
for the node alone (without a wallet), and the client mode is used when the
launcher coordinates the node together with the wallet.

The choice of mode is determined by whether or not the `walletPath` value was specified
in the configuration.

### Server Mode

In the server mode, the launcher operates as follows:

1. Execute the update script (if one exists)
2. Launch the node and wait for it to exit
3. Depending on the node's exit code
   * Exit code `20`: node indicated that it needs to update
      Launcher goes to step 1 of server scenario
   * Other exit code: 
        1. If a report server is available, report a crash
        2. Finish execution of launcher

Note: The node is not supposed to exit by itself except in the case of an update, even with an exit code of 0 a crash is reported.

### Client Mode

In the client mode, the launcher operates as follows:

1. Execute the update script (if one exists)
2. Launch in parallel:
      * node
      * wallet
3. Wait for either the node or the wallet to exit (whichever exists first)
4. In the case where the node exits first
    1. Assume it is a crash and send a report (when a report server is available)
    2. Wait for the wallet
    3. If the wallet indicats that it needs to update (exit with a code 20)
    4. Goto to step 1 of client mode
6. If the wallet exits first, check its exit code:
    1. If it indicated that it needs an update (code 20)
        * Wait for the node to die (the timeout value is specified in `nodeTimeoutSec` in the configuration file)
        * Kill the node in case of a timeout, and then go to the step 1 of client mode
    2. If the wallet exits with a different code, assume it has crashed and kill the node (without performing a timeout)

## Update script execution

This section describes the flow of the updater execution.

1. Check that the updater script exists
    * If it does not exist, the update script execution finishes
    * Note: that the updater script is passed via the `updaterPath` configuration parameter
2. Launch updater:
    * Pass the downloaded update archive (provided via the `updateArchive` configuration parameter) to the updater script as a last argument
3. Wait for the updater to finish and check the exit code as follows:
    * Update script failed (non-zero exit code):
        * The update archive is not touched
        * Update script execution finishes
    * Update script succeeded (zero exit code):
        1. Mark the update as installed in the database
           * Later, if the node tries to download another update, it will check
           this database entry and skip the update that was already installed.
        2. Delete the update archive, as this indicates to the launcher and the
        node that there is no update pending or update in progress.

## Configuration parameters

For more information about the configuration parameters, see [`tools/src/launcher/launcher-config.yaml`](https://github.com/input-output-hk/cardano-sl/blob/develop/tools/src/launcher/launcher-config.yaml).
