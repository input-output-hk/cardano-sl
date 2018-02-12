# CHANGELOG

## Mainnet 1.1.0

Most important code changes which made to release 1.1.0.

### Features

* User is shown a notification when his time is out of sync with global time (notification advises to configure system NTP client).

* Minor: Logs being sent to report server are now compressed before sending (which reduces bandwidth usage in case of large logs).
* Minor: Launcher now launched with YAML config which removes the need for custom .bat/.sh scripts to launch node.
* Minor: Special option for node to switch off TLS protection of wallet endpoints (useful for integration/testing).

* Backend: Support of transactions with multiple recipients (useful for integration)


### Important fixes

* Applied several fixes to `rocksdb` bindings in order to reduce the probability of improper node shutdown.
* Fixed bug in transaction resubmission logic: a queue of locally proposed transaction wasn't sorted well which caused problems for some users, in particular exchanges.
* Applied several minor fixes to update system, for node to correctly retrieve and install updates from blockchain:
   * Node finishing earlier than Daedalus (UI) causing an update to fail.
* Fixed several space leaks in node.
* Fixed few issues with block retrieval:
   * Block syncing became extremely slow after reaching 99% of sync status.
   * Block syncing hanged indefinitely in case of network connection dip (or temporary disconnection).
   * Block syncing hanged after node left running for some hours (some cases).
   * Unnecessary serialization/deserizalization of data on server side which slowed down block retrieval conversation.
* Fixed issue with node failing to start with 5s timeout on slow machines.
* Fixed networking policy to allow network disconnects up to 15s (previous value 2s was too restrictive and slowed down operation between user and Cardano network).
* Applied number of performance improvements on node to support users with huge wallets (e.g. exchanges).
* Improved logging: 
  * Made logs less verbose in some cases, more informative overall.
  * Added more logs in various parts of code to ease investigation upon issues discovered by users, QA.
* Improved automated reporting in case of particular node failures.
* Improved node shutdown behaviour by removing some concurrency and exception handling anti-patterns from codebase.
* Applied several fixes for bugs in Explorer (discovered by users and QA).

### Other work

* Migrated from Travis to Buildkite with nix-based workers as a CI solution.
* Applied few minor fixes were applied to consensus logic.
* Introduced auxx: tooling suite for developers to be able to quickly test and closely interact with node or network.
* Introduced technical documentation for various parts of system, technical documentation is located at https://github.com/input-output-hk/cardano-sl/tree/release/1.1.0/docs.
* Peformed a series of huge refactorings to have better code decomposition, type definitions.
* Implemented first methods for v1 wallet API (prototype).

## Mainnet 1.0.3

TODO: write down

## Mainnet 1.0.2

TODO: write down

## Mainnet 1.0.1

TODO: write down

## Mainnet 1.0.0

TODO: write down

## Testnet 0.6

TODO: Move from https://github.com/input-output-hk/cardano-sl/blob/f556682ab0f0e02e571998ae207139ecc1a24f84/docs/changelog/v0_6_0.md

## Testnet 0.5

### Core

Important fixes:

1. Better Kademlia peer selection (based on explicitly kept timestamps)
2. Message limits: accurate limits for few types, runtime-accurate handling (for the node to work correctly after an update comes in, w/o need to restart it)
3. Accurate version handshake (there were few cases where it wouldn't be handled properly)
4. MPC: check more than 50% of stake provided commitments
5. Fixes for consensus bugs found on Testnet: transaction sorting, undo construction and duplicate block creation

Important codebase changes:

1. Code split to separate components (for each logical part of Cardano SL)
2. Combined config for wallet, node, dev modes
3. Block handling refactoring (needed to write tests)
4. Data relay refactoring
5. Classy-like structure of monad stack

Minor features:

1. P2P peers w/o explicit Kademlia ID provided (only IP and port)
2. More efficient block space usage
3. Consistent block creation logic (check block we just created is valid before applying and distributing)
4. Ability to launch the node with static peers

### Explorer 0.2

1. Auto-generated documentation available at https://cardanodocs.com/technical/explorer/api/
2. Upgraded the client-side with the new version of the libraries, now using PureScript `0.11.5`
3. Added paging for blocks, simplified API
4. Added and improved socket.io, simplified events
5. Japanese translations
6. ADA formatting using Lovelaces
7. Speed optimizations for block and epoch fetching
8. Corrected mobile issues and improved mobile experience
9. Added Waypoint header for better UX
