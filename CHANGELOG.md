# CHANGELOG

## Cardano SL 1.3.0

### Features

- #### Cardano wallet API v1 for the exchanges
  A subset of endpoints from the Cardano wallet API, used by cryptocurrency exchanges, was moved to the new 
  Cardano REST API v1. The remaining endpoints will move from v0 to v1 in the next iteration of work. 
  Cryptocurrency exchanges using the API are encouraged to update their integrations and move to API v1 
  because the old API is now regarded as deprecated and will be removed in future versions. (CBR-101)

- #### Subscription status added to node information
  The endpoint that provides information about the node (`/api/v1/node-info`) was expanded to include
  information about the node subscription status, which can be used to check if the Cardano node is 
  connected to the network or not. (CBR-186)

- #### Improved error message for missing charset in API calls
  The generic message returned from Cardano node endpoint calls when the charset was not specified is replaced with 
  a more descriptive error message to help users of the API troubleshoot the problem. (Wallet Backend - CBR-223)

- #### Support for sending raw data on the network
  It is now possible to send raw, CBOR serialized data to the network using the diffusion layer. Previously, the data 
  had to be serialized before sending it to the network, which had a negative impact on performance. (CBR-277)

### Specifications & documentation

- #### Formal specification for new wallet backend
  The formal specification of a wallet for Cardano (or any UTXO-based cryptocurrency) has been written and it is 
  available [here](https://cardanodocs.com/technical/formal-specification-for-a-cardano-wallet/). (CBR-60)

- Feedback about the current Wallet API has been collected from Exchanges (CBR-104).

- Complete Peer Discovery (P2P) design (CDEC-157).

- Specification of shared seed generation via VSS (CDEC-180).

- Specification of Randomness Generation (CDEC-208).

- As-is specifications of ATRedeem addresses (CDEC-366).

### Fixes and improvements

- #### Performance improvements for sending and receiving blocks
  Sending and receiving blocks on the network now works better because of the following changes. First, 
  deserialization performance has been improved by optimizing memory usage. Next, blocks are now downloaded 
  concurrently without batching. Finally, block traversal is optimized by the introduction of 'forward links' which 
  remove the need for header retrieval and serialization. (CDEC-49)

- #### Optimised block storage
  Block storage is now optimized by consolidating block and undo data in a single file. This change reduces 
  disk use and improves performance when reading and writing blocks. In later Cardano versions, much greater 
  optimizations for the storage of blocks will be introduced, so this is only an interim solution. (CDEC-293)

- #### High (and recurrent) I/O traffic in wallet
  I/O spikes in traffic were being caused by large logs being flushed. This issue has been fixed. (CBR-83)

- #### Failure to reconnect to the network
  Due to improper handling of DNS failures, Cardano node would sometimes fail to reconnect to the network after an 
  internet connection was interrupted and would need to be restarted. This issue has been fixed. (CDEC-259)

- #### Wrong time difference calculation between user’s computer and the network 
  The endpoint (`/api/settings/time/difference`) for calculating the time difference between a user’s computer and 
  Cardano network was returning an incorrect value in some cases. This was because the calculation was not properly 
  handling the time needed to request current time from NTP servers and to get the response. As a result, some Daedalus 
  users were prevented from using their wallet since Daedalus cannot be used if there is a time difference of more 
  han 15 seconds. This issue has been fixed. (TSD-42)

### Testing

- Implement WalletActiveLayer & WalletPassiveLayer for wallet testing purposes (CBR-163).

- Add integration deterministic tests for the Transaction endpoints (CBR-184).

### Chores

- Back port Timer to Pos.Diffusion.Subscription.Common (CDEC-243).

- Message size limits should not be configurable (CDEC-260).

- Upgrade to GHC 8.2.2 (CBR-51).

- Fix AppVeyor hard limitation on Windows (CBR-268).

- Fix tmux versions in demo-script (CO-295).

- Clean script fails if file is missing (CO-316).

## Cardano SL 1.2.1

Bug fix release.

- The Wallet Launcher now uses a lock file. This prevents problems on
  Windows if upgrading Daedalus while the old version is still
  running. (DEVOPS-872)

- Fix character encoding error in the connect script of the Docker
  image. (DEVOPS-877)


## Cardano SL 1.2.0 

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

### Specifications & documentation

- Document the new Wallet V1 API (CBR-102, CBR-183, CO-105 & CBR-278).

- Write a devops guide for the Exchanges (CBR-137).

### Bug fixes

- Fix wallet creation and backup when there are non-latin characters in wallet name (R120-4).

- Fix issue where Daedalus remains stuck at "Connecting to network..." screen after updating version (CBR-282, R120-17).


## Cardano SL 1.1.1

Bug fix release.

 - When making a bug report in the wallet, ensure that usable log
   files exist before packing them into an archive.

 - Update the report-server version used in the Cardano network
   infrastructure, which fixes some issues in receiving bug reports.


## Cardano SL 1.1.0

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
