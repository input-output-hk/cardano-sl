# Cardano SL Launcher

The launcher is an executable that manages running, killing, and re-running the
node and the wallet. For instance, when an update is downloaded, the launcher
will stop the node and the wallet, apply the update, delete the update archive,
and relaunch the node and the wallet.

## Scenarios

The launcher has two primary modes of operation (scenarios): the server scenario
and the client scenario. The server scenario is used when the launcher is used
for the node alone (without wallet), and the client scenario is used when the
launcher coordinates the node together with the wallet.

The choice of scenario is determined by whether the optional CLI parameter
`--wallet` was specified.

In the server scenario, the launcher operates as follows:

* run the update script (if exists)
* run the node and wait for it to exit
* in case the node indicated that it needs to update (exited with code 20),
  goto the first step
* in case the node exited with a different code and a report server is
  available, report a crash

NB: the node isn't supposed to exit by itself except in case of an update,
so even with exit code 0 we report a crash

In the client scenario, the launcher operates as follows:

* run the update script (if exists)
* run the node
* run the wallet
* wait for either the node or the wallet to exit (whichever exists first)
* in case the node exited first, assume it's a crash and report (when a report
  server is available); wait for the wallet; in case the wallet indicated that
  it needs to update (exited with code 20), goto the first step
* in case the wallet exited first, check its exit code; if it indicated that it
  needs an update (code 20), wait for the node to die (timeout specified in CLI)
  or kill the node, and then goto the first step; if the wallet exited with a
  different code, assume it has crashed and kill the node (without timeout)

## Running the Update Script

When we run the update script, first we check that it exists. In case it
doesn't, it's a no-op. We pass the downloaded update archive to it as an
argument, and check the exit code.

In case the update script failed (non-zero exit code), the archive remains in
place and we proceed as if nothing happened.

In case the update script succeeded (zero exit code), we mark the update as
installed in the database. Later, if the node tries to download another update,
it will check this database entry and skip already installed updates.
Furthermore, we delete the update archive, as this indicates to the node that
there's no update pending or in progress.

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
