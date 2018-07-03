# CHANGELOG

## Cardano SL 1.3.0 (Mainnet)

### Features

- The V1 API and its test coverage are finalized (CBR-101).

- Expose SubscriptionStatus as part of /api/v1/node-info (CBR-186).

- Better error message for missing charset (Wallet Backend - CBR-223).

- Create AVVM mnemonic page screenshot (CBR-281).

- Sending raw data, without deserialization, to the network using diffusion layer (CBR-277).

- Speed up block retrieval (CDEC-49).

- Back port Timer to Pos.Diffusion.Subscription.Common (CDEC-243).

- Message size limits should not be configurable (CDEC-260).

- Consolidate the block and undo into a single file per block (CDEC-293).

- Upgrade to GHC 8.2.2 (CBR-51).

### Specifications & documentation

- The formal specifications for the new wallet backend are finished (CBR-60).

- Document the new Wallet V1 API (CBR-102, CBR-183, CO-105 & CBR-278).

- Write a devops guide for the Exchanges (CBR-137).

- Feedback about the current Wallet API has been collected from Exchanges (CBR-104).

- Complete Peer Discovery (P2P) design (CDEC-157).

- Specification of shared seed generation via VSS (CDEC-180).

- Specification of Randomness Generation (CDEC-208).

- As-is specifications of ATRedeem addresses (CDEC-366).

### Testing

- Implement WalletActiveLayer & WalletPassiveLayer for wallet testing purposes (CBR-163).

- Add integration deterministic tests for the Transaction endpoints (CBR-184).

### Fixes

- High (and recurrent) IO traffic in Wallet is solved by removing bad logging of made transaction (CBR-83).

- V1 API wallet restoration issues solved by using asynchronous restoration (CBR-185).

- Fix AppVeyor hard limitation on Windows (CBR-268).

- Node doesn't reconnect to the network (CDEC-259).

- Wallet balance shows wrong Ada amount. Transaction is irrelevant to given wallet (CO-256).

- Fix tmux versions in demo-script (CO-295).

- Cannot create a Wallet via API V1 Wallet API (CO-315).

- Clean script fails if file is missing (CO-316).

- The endpoint /api/settings/time/difference sometimes returns incorrect value (TSD-42).

## Cardano SL 1.2.1 (Mainnet)

Bug fix release.

- The Wallet Launcher now uses a lock file. This prevents problems on
  Windows if upgrading Daedalus while the old version is still
  running. (DEVOPS-872)

- Fix character encoding error in the connect script of the Docker
  image. (DEVOPS-877)


## Cardano SL 1.2.0 (Mainnet)

These are the most important code changes which were included in release 1.2.0.

### Features

- Add to the V1 API all endpoints required for Exchanges (CBR-103).

- Development and execution of Benchmarking Tools for the Wallet V0 API (CBR-7, CBR-23 & CBR-88).

- Detailed design and documentation of how to test a wallet. Pre-requisite for building regression tests (CBR-24).

- Discriminate between publicly exposed wallet API endpoints and endpoints reserved to an internal IOHK usage (CBR-19).

- Design and implementation of the new V1 API endpoints (CBR-16).

- Optimize the way the mempool is used in the Wallet Backend (CBR-6).

- Analyse the usage of database updates and introduce new atomic updates in the Wallet Backend (CBR-8).

- Investigate and solve the balance discrepancy experienced by Bittrex (CBR-135).

- Asynchronous restoration of a wallet from seed (CBR-90).

- Add sorting and filtering capabilities to the wallet V1 API (CBR-20).

### Bug fixes

- Fix wallet creation and backup when there are non-latin characters in wallet name (R120-4).

- Fix issue where Daedalus remains stuck at "Connecting to network..." screen after updating version (CBR-282, R120-17).


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
