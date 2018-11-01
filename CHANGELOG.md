# CHANGELOG


## Cardano SL vNext [1.4.0]

### Features

- #### Reduce number of files on disk
  Blund files (containing blocks and their undos) for older epochs (from zero up to current
  epoch minus two) are now consolidated into epoch/index file pairs. That means that the number
  of files to store the block chain for a single epoch is reduced from 21600 (one blund file for
  each slot in an epoch) to an epoch/index file pair. Consolidation happens on-the-fly in a
  background process.

- #### Add bouncing and throttling to the API
  Previously, exchanges could accidentally overload their wallet servers. We
  have added configurable throttling to the API service to prevent this
  problem. To configure this, setting, view the changes in the
  `configuration.yaml` file under the `wallet` section. The API will now return
  a 429 error containing the microseconds to wait until retry.

- We can force an NTP-check when getting node-info via the API (`?force_ntp_check` query flag) (CO-325)

- The API provides an endpoint to retrieve basic statistics on the UTxO distribution of a wallet
  (`/api/v1/wallets/{walletId}/statistics`). (CO-325)

- cardano-sl exposes a new package `x509` with tooling for defining a PKI infrastructure from
  pure Haskell. This is basically an export of the internals of the tool `cardano-sl-x509-generate` (CO-387)


### Fixes

- #### Make productionReporter more robust
  Add exception handling code in reporting exception handler, to prevent IOExceptions from killing
  the main thread. This was noticed when the network connection was interrupted, and the reporter
  died when it tried to report over the down network. (CDEC-470 / [PR 3365])

[PR 3365]: https://github.com/input-output-hk/cardano-sl/pull/3365

- Improve type safety (and as a consequence, API documentation) of account indexes (CBR-306)

- The Swagger specification had names with illegal characters. These names
  where changed to be URL friendly. [PR #3595](https://github.com/input-output-hk/cardano-sl/pull/3595)

- The creation of mnemonic doesn't throw anymore when provided words outside of the BIP39 English dictionnary.
  Instead, it returns an error value gracefully (CO-325)

- Response from `JSONValidationError` are now also encoded inline (instead of a pretty-encoding with newlines) (DDW-318)

- **[API BREAKING CHANGE]** The behavior of `/api/v1/addresses/{address}` has been adjusted to reflect more accurately
  the meaning of ownership regarding addresses.
  The previous version of this endpoint failed with an HTTP error when the given address was unknown to the wallet.
  This was misleading since an address that is unknown to the wallet may still belong to the wallet. To reflect this,
  the V1 endpoint does not fail anymore as it used to when an address is not recognised and returns instead a new field
  'is-ours' which indicates either that an address is ours, or that it is 'not-recognised'. (CBR-401)
  
 - **[API BREAKING CHANGE]** A DELETE request to `/api/v1/wallets/{wallet}` now correctly fails with 404 if the wallet doesn't exist. Previously it incorrectly responded with 204.

### Improvements

- Friendly error mistakes from deserializing invalid addresses instead of brutal 500 (CBR-283)

- **[API BREAKING CHANGE]** Add `walletId` to `WalletAlreadyExists` WalletLayerError (CBR-254)

- Small refactor of wallet Errors implementation to be more maintainable (CBR-26)

- Content-Type parser is now more lenient and accepts `application/json`, `application/json;charset=utf-8` and
  no Content-Type at all (defaulting to `application/json`).

- The codebase now relies on the package `cryptonite` (instead of `ed25519`) for Ed25519 implementation (CO-325)

- **[API BREAKING CHANGE]** Improve diagnostic for `NotEnoughMoney` error (CBR-461)

- When Content-Type's main MIME-type cannot fall back to 'application/json' then UnsupportedMimeTypeError is returned

### Specifications

### Documentation

- Make an inventory of existing wallet errors and exceptions (CBR-307)

- wallet-new README has been improved (especially on sections about testing) and updated (CO-325)


## Cardano SL 1.3.1

### Features

- Add genesis data for the Cardano SL public Testnet. ([#3265](https://github.com/input-output-hk/cardano-sl/pull/3265), [#3349](https://github.com/input-output-hk/cardano-sl/pull/3349), [#3585](https://github.com/input-output-hk/cardano-sl/pull/3585), DEVOPS-398)

- Add a "faucet" API to dispense Testnet Ada to users. ([#2939](https://github.com/input-output-hk/cardano-sl/pull/2939), DEVOPS-834)

- Support additional fields for Daedalus bug reports in the report server. ([#3394](https://github.com/input-output-hk/cardano-sl/pull/3394), TSD-116)

### Fixes

- Fix a case where the error reporting system can cause the node to stop syncing. ([#3294](https://github.com/input-output-hk/cardano-sl/pull/3294), CDEC-469, CDEC-439)

- Adjust the address format to distinguish testnet and mainnet addresses. ([#3540](https://github.com/input-output-hk/cardano-sl/pull/3540), [#3556](https://github.com/input-output-hk/cardano-sl/pull/3556), [#3558](https://github.com/input-output-hk/cardano-sl/pull/3558), [#3561](https://github.com/input-output-hk/cardano-sl/pull/3561), [#3578](https://github.com/input-output-hk/cardano-sl/pull/3578), [#3583](https://github.com/input-output-hk/cardano-sl/pull/3583), [#3618](https://github.com/input-output-hk/cardano-sl/pull/3618), [#3659](https://github.com/input-output-hk/cardano-sl/pull/3659), [#3685](https://github.com/input-output-hk/cardano-sl/pull/3685), CO-353, CO-354)

- Add a workaround for the RocksDB issue which prevented Daedalus Windows users with non-ASCII usernames from starting the wallet. ([#3465](https://github.com/input-output-hk/cardano-sl/pull/3465), CBR-391)

- Add support to cardano-sl-launcher for launching Daedalus in "safe mode". ([#3421](https://github.com/input-output-hk/cardano-sl/pull/3421), DDW-380)

- Fix the x509 certificate generation tool to allow using IP addresses as subject alternate names. ([#3390](https://github.com/input-output-hk/cardano-sl/pull/3390), DEVOPS-992)

- Fix display of the git version in Cardano Explorer. ([#3386](https://github.com/input-output-hk/cardano-sl/pull/3386), DEVOPS-999)

- Update Windows build script to use a newer openssl version. ([#3397](https://github.com/input-output-hk/cardano-sl/pull/3397), DEVOPS-1003)

- Fix a build system issue which caused slow builds. ([#3622](https://github.com/input-output-hk/cardano-sl/pull/3622), DEVOPS-916)


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
