# Cardano SL Launcher

Launcher is utility that is responsible for:
 * Launching node, wallet
 * Executing updates (given update archive is retrieved by node)
 * Reporting to report server in case node/wallet finish unexpectedly

For example, when an update is downloaded, Launcher will:
 * Wait for wallet to finish
 * Stop the node
 * Execute update (to apply the update), delete the update archive
 * Relaunch node, wallet

## Scenarios

The launcher has two primary modes of operation (scenarios): the server scenario
and the client scenario. The server scenario is used when the launcher is used
for the node alone (without wallet), and the client scenario is used when the
launcher coordinates the node together with the wallet.

The choice of scenario is determined by whether the optional CLI parameter
`--wallet` was specified.

### Server scenario

In the server scenario, the launcher operates as follows:

1. Execute the update script (if exists)
2. Launch the node and wait for it to exit
3. Depending on node's exit code
   * Exit code `20`: node indicated that it needs to update
      Launcher goes to step 1 of server scenario
   * Other exit code: 
        1. If a report server is available, report a crash
        2. Finish execution of launcher

NB: the node isn't supposed to exit by itself except in case of an update,
so even with exit code 0 we report a crash

### Client scenario

In the client scenario, the launcher operates as follows:

1. Execute the update script (if exists)
2. Launch in parallel:
      * node
      * wallet
3. Wait for either the node or the wallet to exit (whichever exists first)
4. In case the node exited first
    1. Assume it's a crash and report (when a report server is available)
    2. Wait for the wallet
    3. If the wallet indicated that it needs to update (exited with code 20)
    4. Goto to step 1 of client scenario
6. If the wallet exited first, check its exit code:
    1. If it indicated that it needs an update (code 20)
        * Wait for the node to die (timeout specified in CLI)
        * Kill the node in case of timeout, and then goto the step 1 of client scenario
    2. If the wallet exited with a different code, assume it has crashed and kill the node (without timeout)

## Update script execution

Here we describe flow of updater execution.

1. Check that updater script exists
    * If it doesn't exist, update script execution finishes
    * Note, that updater script is passed via CLI parameter `--updater`
2. Launch updater:
    * Pass the downloaded update archive (provided via CLI parameter `--update-archive`) to updater script as last
argument
3. Wait for updater to finish, check the exit code:
    * Update script failed (non-zero exit code):
        * The update archive is not touched
        * Update script execution finishes
    * Update script succeeded (zero exit code):
        1. Mark the update as installed in the database
           * Later, if the node tries to download another update, it will check this database entry and skip already installed update.
        2. Delete the update archive, as this indicates to the launcher, node that there's no update pending or in progress.

## CLI Parameters

* `--node`: the path to the node executable

* `--wallet`: the path to the wallet executable (determines the scenario)

* `--db-path`: the path to node database (used for tracking of installed updates)

* `--node-log-config`: the path to the node logging config (attached to crash
reports)

* `--node-log-path`: the path to a log file for the node; passed to the node as
is, but also has defaulting logic (a temporary file is created when no path is
specified)

* `--updater`: the path to the updater script (used when an update is available)

* `--update-archive`: the path at which an update archive will be saved by the
node when a new update is broadcasted is registered in the blockchain

* `--node-timeout`: the time (in seconds) to wait for a graceful shutdown of the
  node in case the wallet exited with code 20

* `--report-server`: a URL of the server where crash reports will be sent

* `--launcher-logs-prefix`: a directory for launcher logs (optional)

Besides these launcher-specific parameters, the launcher also expects:

* parameters prefixed with `-n` -- those are passed to the node verbatim
* parameters prefixed with `-w` -- those are passed to the wallet verbatim
* paremeters prefixed with `-u` -- those are passed to the update script
  verbatim
* `--configuration-file`, `--configuration-key`, `--system-start`, and
  `--configuration-seed`: these are used to initialize the global constant
  configuration
