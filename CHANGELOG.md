# CHANGELOG

## Cardano SL 1.1.1 (Mainnet)

Bug fix release.

 - When making a bug report in the wallet, ensure that usable log
   files exist before packing them into an archive.

 - Update the report-server version used in the Cardano network
   infrastructure, which fixes some issues in receiving bug reports.


## Cardano SL 1.1.0 (Mainnet)

Most important code changes which made to release 1.1.0.

### Features

- A new API endpoint is created for providing information on how much time on user’s machine is out of sync with the global time.

- An API endpoint for creating transactions is improved to support multiple destination addresses (transaction batching).

- Logs are compressed when sent to the reporting server to reduce bandwidth usage.

- The Cardano launcher configuration is improved with support for the YAML format to remove the need for custom scripts for launching the Cardano node. 

- A new option for launching the Cardano node allows usage of API endpoints without TLS encryption for easier testing while developing integrations.

### Bug fixes and improvements

- Fixed improper node shutdowns, which caused some of the ‘connecting to network’ issues when using Daedalus. Improved Cardano node shutdown behavior by improvements to concurrency and exception handling, providing fixes to rocksdb database bindings.

- Fixed transaction queuing and resubmission logic to remove some of the issues reported by cryptocurrency exchanges. 

- Several minor fixes to the update system for correctly receiving and installing updates from the blockchain to remove some of the reported cases of failed updates.

- Fixed several space leaks in the operation of the Cardano node, fixing the Cardano node using all memory resources issue.

- Fixed a bug with block retrieval, causing the extremely slow syncing after blockchain syncing reaches 99%.

- Fixed a bug with blockchain syncing in case of unreliable internet connection, causing the blockchain syncing to never complete.

- Fixed a bug causing block syncing to stop working in some cases when the Cardano node is left running for an extended period of time.

- Improved performance of blockchain syncing by the removal of some unnecessary serialization and deserialization.

- Fixed a bug causing the Cardano node to fail to start on slow computers, caused by a too-restrictive 5-second timeout.

- Improved networking policy by allowing more time to connect to the network, resolving connectivity problems for users on slow internet connections.

- Significant performance improvements for API endpoints for handling wallet operations, where the issues previously caused slow performance for users operating wallets with a large number of addresses, such as cryptocurrency exchanges.

- Logging improved by making logs less verbose when that is not needed and by expanding the logs to cover more cases necessary for better issue diagnostics and easier quality assurance.

- Improved automated issue reporting for some previously uncovered cases of Cardano node failures.

### Other work

- Migrated from Travis to Buildkite with nix-based workers as a CI solution.

- Applied few minor fixes were applied to consensus logic.

- Introduced auxx: tooling suite for developers to be able to quickly test and closely interact with node or network.

- Introduced technical documentation for various parts of system, technical documentation is located at https://github.com/input-output-hk/cardano-sl/tree/release/1.1.0/docs.

- Performed a series of huge refactorings to have better code decomposition, type definitions.

- Implemented first methods for v1 wallet API (prototype).

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
