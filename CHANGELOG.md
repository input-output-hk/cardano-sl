# CHANGELOG

## Cardano SL vNEXT

### Improvements

- Updated the legal disclaimer text in the footer of https://cardanoexplorer.com/ [#4261](https://github.com/input-output-hk/cardano-sl/pull/4261)

## Cardano SL 3.2.0

### Improvements
- Improve DNS subscription valency and fallback behaviour [#4243](https://github.com/input-output-hk/cardano-sl/pull/4243)
- Single-machine multi-node mixed cluster CI prerequisites [#4247](https://github.com/input-output-hk/cardano-sl/pull/4247)

## Cardano SL 3.1.0

### Features

- A new endpoint to calculate the walletid of a given mnemonic [#4237](https://github.com/input-output-hk/cardano-sl/pull/4237)

### Fixes

- Correct change category in 3.0.3 and add section for unreleased changes [#4232](https://github.com/input-output-hk/cardano-sl/pull/4232)

### Improvements

- Configuration files and environments map moved to iohk-nix [#4227](https://github.com/input-output-hk/cardano-sl/pull/4227)
- License update to apache 2 [#4242](https://github.com/input-output-hk/cardano-sl/pull/4242)


## Cardano SL 3.0.3

### Improvements

- Refactor scripts to use nix-tools and create windows daedalus-bridge output [#4183](https://github.com/input-output-hk/cardano-sl/pull/4183)
- Remove Report Server [#4194](https://github.com/input-output-hk/cardano-sl/pull/4194)
- New testnet Genesis [#4193](https://github.com/input-output-hk/cardano-sl/pull/4193)
- Add explorer endpoint to fetch a range of blocks [#4198](https://github.com/input-output-hk/cardano-sl/pull/4198)
- Add python service to dump explorer data to PostgreSQL [#4202](https://github.com/input-output-hk/cardano-sl/pull/4202)
- Security updates for explorer frontend [#4201](https://github.com/input-output-hk/cardano-sl/pull/4201)

## Cardano SL 3.0.2

### Fixes

- Fix issue that caused the node to lose sync with the chain in the OBFT era and then fail to re-sync once out of sync ([CBR-525](https://iohk.myjetbrains.com/youtrack/issue/CBR-525) [#4153](https://github.com/input-output-hk/cardano-sl/pull/4153))

- Fix wrong balance and payment reporting on coin selection failures ([CO-450](https://iohk.myjetbrains.com/youtrack/issue/CBR-450) [#4159](https://github.com/input-output-hk/cardano-sl/pull/4159))

- Additional coin selection fallback to cope with transaction filling up during random-improve selection ([CO-450](https://iohk.myjetbrains.com/youtrack/issue/CBR-450) [#4159](https://github.com/input-output-hk/cardano-sl/pull/4159))

- Additional guard to prevent too big transactions to be submitted ([CO-449](https://iohk.myjetbrains.com/youtrack/issue/CBR-449) [#4131](https://github.com/input-output-hk/cardano-sl/pull/4131))

## Cardano SL 3.0.1

### Fixes

- Fix inconsistent 'hasSpendingPassword' resolution from legacy data layer migration ([DEVOPS-1250](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1250) [#4112](https://github.com/input-output-hk/cardano-sl/pull/4112))

## Cardano SL 3.0.0

### Fixes

- Limit the rate of dns queries to avoid reaching the limit of open files, try resolve dns in ntp client every 30s rather than only once ([CDEC-659](https://iohk.myjetbrains.com/youtrack/issue/CDEC-659) [#4008](https://github.com/input-output-hk/cardano-sl/pull/4008))

- Fix incoherent To/FromJSON instances in `core`, `chain`, `lib`. Alter `TxValidationRules` datatype. ([CBR-502](https://iohk.myjetbrains.com/youtrack/issue/CBR-502) [#4037](https://github.com/input-output-hk/cardano-sl/pull/4037))

- Fix target name to correct integration tests ([CO-437](https://iohk.myjetbrains.com/youtrack/issue/CO-437) [#3794](https://github.com/input-output-hk/cardano-sl/pull/3794))

### Features

- Support for (unused) addresses batch import ([CO-447](https://iohk.myjetbrains.com/youtrack/issue/CO-447) [#4040](https://github.com/input-output-hk/cardano-sl/pull/4040))

- Add `script-runner` tool to automate cluster-level testing ([DEVOPS-1131](https://iohk.myjetbrains.com/youtrack/v2/issue/devops-1131): [#3916](https://github.com/input-output-hk/cardano-sl/pull/3916) [#4057](https://github.com/input-output-hk/cardano-sl/pull/4057))

- Node Monitoring API: nodes now serve their own settings and info via a web server via a `/api/v1/node-settings` and `/api/v1/node-info` (still proxied by the wallet backend) ([#110](https://github.com/input-output-hk/cardano-wallet/issues/110))
  - Set up scaffolding for node API [#3788](https://github.com/input-output-hk/cardano-sl/pull/3788)
  - Wire Up Node Api [#3851](https://github.com/input-output-hk/cardano-sl/pull/3851)
  - Unify Node APIs [#3882](https://github.com/input-output-hk/cardano-sl/pull/3882)
  - Node API Jsend errors [#3910](https://github.com/input-output-hk/cardano-sl/pull/3910)
  - Node API Documentation Server [#3925](https://github.com/input-output-hk/cardano-sl/pull/3925)
  - Implement IPC shutdown [#3939](https://github.com/input-output-hk/cardano-sl/pull/3939)
  - Implement Update endpoints for the Node API [#3957](https://github.com/input-output-hk/cardano-sl/pull/3957)
  - Parse required CLI params in cluster [#3987](https://github.com/input-output-hk/cardano-sl/pull/3987)
  - Fix Arbitrary instance for APIResponse [#3989](https://github.com/input-output-hk/cardano-sl/pull/3989)
  - Special case 404 errors with empty bodies [#3995](https://github.com/input-output-hk/cardano-sl/pull/3995)
  - Disable node monitoring API by default [#4051](https://github.com/input-output-hk/cardano-sl/pull/4051)

- Additional node settings exposed through the wallet backend API in `/api/v1/node-settings`. This is in order to align and be on-par with the new node monitoring API. [#4045](https://github.com/input-output-hk/cardano-sl/pull/4045)
  Added settings:
    - `slotId`: The current slot and epoch
    - `slotCount`: The number of slots per epoch
    - `maxTxSize`: The largest allowed transaction size in bytes
    - `feePolicy`: The fee policy, in flat Lovelace and variable Lovelace/byte
    - `securityParameter`: The consensus security parameter (usually referred as `k` in the papers)

- Implement block creation and validation for the OBFT `ConsensusEra`.
    - Implement hard fork mechanism ([CDEC-610](https://iohk.myjetbrains.com/youtrack/issue/CDEC-610): [#3822](https://github.com/input-output-hk/cardano-sl/pull/3822))
    - Adapt epoch consolidation functionality for OBFT ([CBR-497](https://iohk.myjetbrains.com/youtrack/issue/CBR-497) [#3966](https://github.com/input-output-hk/cardano-sl/pull/3966))
    - Extend `LastBlkSlots` data type and storage for OBFT ([CBR-499](https://iohk.myjetbrains.com/youtrack/issue/CBR-499) [#4003](https://github.com/input-output-hk/cardano-sl/pull/4003) [#4052](https://github.com/input-output-hk/cardano-sl/pull/4052))
    - Disable SSC during OBFT. ([CBR-490](https://iohk.myjetbrains.com/youtrack/issue/CBR-490) [#4026](https://github.com/input-output-hk/cardano-sl/pull/4026))
    - Implement OBFT block validation ([CBR-481](https://iohk.myjetbrains.com/youtrack/issue/CBR-481) [#4018](https://github.com/input-output-hk/cardano-sl/pull/4018) [#4029](https://github.com/input-output-hk/cardano-sl/pull/4029) [#4059](https://github.com/input-output-hk/cardano-sl/pull/4059))
    - Add cluster-level tests for OBFT using `script-runner` ([CBR-503](https://iohk.myjetbrains.com/youtrack/issue/CBR-503): [#4061](https://github.com/input-output-hk/cardano-sl/pull/4061) [#4073](https://github.com/input-output-hk/cardano-sl/pull/4073))
    - Improve block validation tests ([CBR-504](https://iohk.myjetbrains.com/youtrack/issue/CBR-504) [#4081](https://github.com/input-output-hk/cardano-sl/pull/4081))
    - Implement OBFT block creation ([CBR-482](https://iohk.myjetbrains.com/youtrack/issue/CBR-481) [#4077](https://github.com/input-output-hk/cardano-sl/pull/4077))

- Review node-info 'localTimeInfo' API to include pending statuses ([CBR-505](https://iohk.myjetbrains.com/youtrack/issue/CBR-505) [#4099](https://github.com/input-output-hk/cardano-sl/pull/4099))


### Improvements

- Removal of V0 API & Legacy Data Layer ([CO-372](https://iohk.myjetbrains.com/youtrack/issue/CO-372))
  - Remove V0 and `Legacy*` modules ([CO-394](https://iohk.myjetbrains.com/youtrack/issue/CO-372) [#3667](https://github.com/input-output-hk/cardano-sl/pull/3667))
  - Move `wallet@Pos.Util.Mnemonic` to `wallet-new@Cardano.Wallet.Kernel.BIP39` ([CO-374](https://iohk.myjetbrains.com/youtrack/issue/CO-374) [#3663](https://github.com/input-output-hk/cardano-sl/pull/3663))
  - Replace remaining uses of `Pos.Util.Mnemonic` ([CO-401](https://iohk.myjetbrains.com/youtrack/issue/CO-401) [#3663](https://github.com/input-output-hk/cardano-sl/pull/3663))
  - Remove `wallet` dependency from `Cardano/Wallet/Server/Plugins.hs` ([CO-403](https://iohk.myjetbrains.com/youtrack/issue/CO-403) [#3686](https://github.com/input-output-hk/cardano-sl/pull/3686))
  - Remove `wallet` dependency from `Cardano/Wallet/API/V1/Swagger/Example.hs` ([CO-404](https://iohk.myjetbrains.com/youtrack/issue/CO-404) [#3687](https://github.com/input-output-hk/cardano-sl/pull/3687))
  - Remove `wallet` dependency from `Cardano/Wallet/API/V1/Types.hs` ([CO-405](https://iohk.myjetbrains.com/youtrack/issue/CO-405) [#3688](https://github.com/input-output-hk/cardano-sl/pull/3688))
  - Remove `wallet` dependency from `Cardano/Wallet/API/V1/Swagger.hs` ([CO-406](https://iohk.myjetbrains.com/youtrack/issue/CO-406) [#3689](https://github.com/input-output-hk/cardano-sl/pull/3689))
  - Remove `wallet` dependency from `wallet-new/src/Cardano/Wallet/Kernel/Pending.hs` & `wallet-new/src/Cardano/Wallet/Kernel/Decrypt.hs` ([CO-402](https://iohk.myjetbrains.com/youtrack/issue/CO-402) [#3683](https://github.com/input-output-hk/cardano-sl/pull/3683))
  - Tweaks to `Cardano.Wallet.Kernel.Decrypt` ([CO-408](https://iohk.myjetbrains.com/youtrack/issue/CO-408) [#3699](https://github.com/input-output-hk/cardano-sl/pull/3699))
  - Remove `wallet` dependency from `wallet-new/src/Cardano/Wallet/Kernel/Migration.hs` ([CO-407](https://iohk.myjetbrains.com/youtrack/issue/CO-407) [#3836](https://github.com/input-output-hk/cardano-sl/pull/3836))
  - Remove `wallet-new/src/Cardano/Wallet/API/V1/Migration/Types.hs` ([CO-380](https://iohk.myjetbrains.com/youtrack/issue/CO-380) [#3700](https://github.com/input-output-hk/cardano-sl/pull/3700))
  - Remove `wallet` dependency from `wallet-new/src/Cardano/Wallet/Orphans/*` ([CO-413](https://iohk.myjetbrains.com/youtrack/issue/CO-413) [#3707](https://github.com/input-output-hk/cardano-sl/pull/3707))
  - Accommodate LegacyHandlers refactoring result and investigate possibility of removing `Core.Coin` instances ([CO-422](https://iohk.myjetbrains.com/youtrack/issue/CO-422) [#3809](https://github.com/input-output-hk/cardano-sl/pull/3809))
  - Follow-up on CO-373 (Move Mnemonic module into Kernel/BIP39) ([CO-426](https://iohk.myjetbrains.com/youtrack/issue/CO-426) [#3738](https://github.com/input-output-hk/cardano-sl/pull/3738))
  - Cleanup references to `cardano-sl-wallet` across `cardano-sl` ([CO-430](https://iohk.myjetbrains.com/youtrack/issue/CO-430) [#3750](https://github.com/input-output-hk/cardano-sl/pull/3750))
  - Remove `bench` package from cardano-sl-wallet-new ([CO-442](https://iohk.myjetbrains.com/youtrack/issue/CO-442) [#3828](https://github.com/input-output-hk/cardano-sl/pull/3828))
  - Remove `wallet` from cardano-sl codebase ([CO-443](https://iohk.myjetbrains.com/youtrack/issue/CO-443) [#3837](https://github.com/input-output-hk/cardano-sl/pull/3837))

- Rewritten and redesigned integration tests framework ([CO-356](https://iohk.myjetbrains.com/youtrack/issue/CO-356) [#4047](https://github.com/input-output-hk/cardano-sl/pull/4047))
- Simplify prefixing of transaction to account for foreign transactions in inputs rather than process them from outputs ([CO-432](https://iohk.myjetbrains.com/youtrack/issue/CO-432) [#3768](https://github.com/input-output-hk/cardano-sl/pull/3768))
- Implement validation rule that limits the `Attributes` size in a `Tx` ([CDEC-643](https://iohk.myjetbrains.com/youtrack/issue/CDEC-643): [#3878](https://github.com/input-output-hk/cardano-sl/pull/3878))
- Add proper mapping of kernel addresses to API errors when batch import of addresses ([CO-447](https://iohk.myjetbrains.com/youtrack/issue/CO-447) [#4084](https://github.com/input-output-hk/cardano-sl/pull/4084))

### Documentation

- Remove redundant [] in backupPhrase doc example ([#4038](https://github.com/input-output-hk/cardano-sl/pull/4038))

- New "common use-case" entry the API doc about importing addresses ([CO-447](https://iohk.myjetbrains.com/youtrack/issue/CO-447) [#4040](https://github.com/input-output-hk/cardano-sl/pull/4040))

- Links to specific sections of the API doc now work [#4064](https://github.com/input-output-hk/cardano-sl/pull/4064)

## Cardano SL 2.0.1

### Fixes

- Relocate fee sanity check and make it relative to each transaction (rather than absolute) ([CO-446](https://iohk.myjetbrains.com/youtrack/issue/CO-446) [#3993](https://github.com/input-output-hk/cardano-sl/pull/3993)
- Correction to block retrieval concurrency and exception handling, so that it will recover in case of certain network failure conditions. ([CDEC-658](https://iohk.myjetbrains.com/youtrack/issue/CDEC-658) [#3998](https://github.com/input-output-hk/cardano-sl/pull/3998))

## Cardano SL 2.0.0


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
  a 429 error containing the microseconds to wait until retry. ([CBR-179](https://iohk.myjetbrains.com/youtrack/issue/CBR-179), [#3431](https://github.com/input-output-hk/cardano-sl/pull/3431))

- We can force an NTP-check when getting node-info via the API (`?force_ntp_check` query flag)
    - [CO-325](https://iohk.myjetbrains.com/youtrack/issue/CO-325), [#3372](https://github.com/input-output-hk/cardano-sl/pull/3372), [#3461](https://github.com/input-output-hk/cardano-sl/pull/3461), [#3607](https://github.com/input-output-hk/cardano-sl/pull/3607)
    - [CBR-427](https://iohk.myjetbrains.com/youtrack/issue/CBR-427) [#3586](https://github.com/input-output-hk/cardano-sl/pull/3586)

- The API provides an endpoint to retrieve basic statistics on the UTxO distribution of a wallet (`/api/v1/wallets/{walletId}/statistics`) ([CO-347](https://iohk.myjetbrains.com/youtrack/issue/CO-347), [#3402](https://github.com/input-output-hk/cardano-sl/pull/3402))

    - [CO-388](https://iohk.myjetbrains.com/youtrack/issue/CO-388), [#3610](https://github.com/input-output-hk/cardano-sl/pull/3610)
    - [CO-389](https://iohk.myjetbrains.com/youtrack/issue/CO-389), [#3620](https://github.com/input-output-hk/cardano-sl/pull/3620), [#3658](https://github.com/input-output-hk/cardano-sl/pull/3658)

- cardano-sl exposes a new package `x509` with tooling for defining a PKI infrastructure from pure Haskell. This is basically an export of the internals of the tool `cardano-sl-x509-generate`
    - [DEVOPS-992](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-992), [#3367](https://github.com/input-output-hk/cardano-sl/pull/3367)
    - [CO-388](https://iohk.myjetbrains.com/youtrack/issue/CO-388), [#3610](https://github.com/input-output-hk/cardano-sl/pull/3610)
    - [CO-389](https://iohk.myjetbrains.com/youtrack/issue/CO-389), [#3620](https://github.com/input-output-hk/cardano-sl/pull/3620), [#3658](https://github.com/input-output-hk/cardano-sl/pull/3658)

- Structured logging ([CBR-97](https://iohk.myjetbrains.com/youtrack/issue/CBR-97) [#3483](https://github.com/input-output-hk/cardano-sl/pull/3483) [#3645](https://github.com/input-output-hk/cardano-sl/pull/3645), [CBR-207](https://iohk.myjetbrains.com/youtrack/issue/CBR-207), [#3476](https://github.com/input-output-hk/cardano-sl/pull/3476) [#3477](https://github.com/input-output-hk/cardano-sl/pull/3477), [CBR-211](https://iohk.myjetbrains.com/youtrack/issue/CBR-211) [#3507](https://github.com/input-output-hk/cardano-sl/pull/3507), [CBR-213](https://iohk.myjetbrains.com/youtrack/issue/CBR-213), [#3481](https://github.com/input-output-hk/cardano-sl/pull/3481), [DEVOPS-1097](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1097), [#3764](https://github.com/input-output-hk/cardano-sl/pull/3764), [#3395](https://github.com/input-output-hk/cardano-sl/pull/3395), [#3443](https://github.com/input-output-hk/cardano-sl/pull/3443), [DEVOPS-1109](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1109), [#3785](https://github.com/input-output-hk/cardano-sl/pull/3785), [CBR-275](https://iohk.myjetbrains.com/youtrack/issue/CBR-275) [#3533](https://github.com/input-output-hk/cardano-sl/pull/3533) [#3534](https://github.com/input-output-hk/cardano-sl/pull/3534) [#3655](https://github.com/input-output-hk/cardano-sl/pull/3655), [CBR-345](https://iohk.myjetbrains.com/youtrack/issue/CBR-345) [#3526](https://github.com/input-output-hk/cardano-sl/pull/3526) [#3613](https://github.com/input-output-hk/cardano-sl/pull/3613) [#3632](https://github.com/input-output-hk/cardano-sl/pull/3632) [#3633](https://github.com/input-output-hk/cardano-sl/pull/3633) [#3709](https://github.com/input-output-hk/cardano-sl/pull/3709), [CBR-348](https://iohk.myjetbrains.com/youtrack/issue/CBR-348) [#3523](https://github.com/input-output-hk/cardano-sl/pull/3523), [CBR-430](https://iohk.myjetbrains.com/youtrack/issue/CBR-430) [#3603](https://github.com/input-output-hk/cardano-sl/pull/3603), [CBR-423](https://iohk.myjetbrains.com/youtrack/issue/CBR-423) [#3609](https://github.com/input-output-hk/cardano-sl/pull/3609), [RCD-42](https://iohk.myjetbrains.com/youtrack/issue/RCD-42) [#3816](https://github.com/input-output-hk/cardano-sl/pull/3816))

- New data layer for wallet ([CBR-150](https://iohk.myjetbrains.com/youtrack/issue/CBR-150), [#3245](https://github.com/input-output-hk/cardano-sl/pull/3245),
[CBR-227](https://iohk.myjetbrains.com/youtrack/issue/CBR-227),
[#3393](https://github.com/input-output-hk/cardano-sl/pull/3393)
)


- Enable new data layer in Docker images for exchanges ([DEVOPS-1037](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1037), [#3545](https://github.com/input-output-hk/cardano-sl/pull/3545), [DEVOPS-1046](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1046), [#3594](https://github.com/input-output-hk/cardano-sl/pull/3594)).

- #### Address Discrimination
    - Set up supporting tests ([CDEC-500](https://iohk.myjetbrains.com/youtrack/issue/CDEC-500): [#3403](https://github.com/input-output-hk/cardano-sl/pull/3403), [#3405](https://github.com/input-output-hk/cardano-sl/pull/3405))
    - Fix JSON golden test helpers ([CDEC-506](https://iohk.myjetbrains.com/youtrack/issue/CDEC-506): [#3415](https://github.com/input-output-hk/cardano-sl/pull/3415))
    - Adjust the address format to distinguish testnet and mainnet addresses ([CO-410](https://iohk.myjetbrains.com/youtrack/issue/CO-410): [#3715](https://github.com/input-output-hk/cardano-sl/pull/3715), [#3733](https://github.com/input-output-hk/cardano-sl/pull/3733), [#3756](https://github.com/input-output-hk/cardano-sl/pull/3756), [#3775](https://github.com/input-output-hk/cardano-sl/pull/3775))

- New `cluster` package with utility and CLI to start a full-fledged cluster of nodes
    - [CO-423](https://iohk.myjetbrains.com/youtrack/issue/CO-423), [#3732](https://github.com/input-output-hk/cardano-sl/pull/3732)
    - [CO-371](https://iohk.myjetbrains.com/youtrack/issue/CO-371), [#3605](https://github.com/input-output-hk/cardano-sl/pull/3605)
    - [CO-385](https://iohk.myjetbrains.com/youtrack/issue/CO-385), [#3608](https://github.com/input-output-hk/cardano-sl/pull/3608)
    - [CO-390](https://iohk.myjetbrains.com/youtrack/issue/CO-390), [#3629](https://github.com/input-output-hk/cardano-sl/pull/3629)

- Support query against some fields of the Account resource (balance, addresses) enabling client to fetch only the data they need ([CO-324](https://iohk.myjetbrains.com/youtrack/issue/CO-324), [#3210](https://github.com/input-output-hk/cardano-sl/pull/3210))

- Integration with the new data-layer
    - [CBR-437](https://iohk.myjetbrains.com/youtrack/issue/CBR-437), [#3621](https://github.com/input-output-hk/cardano-sl/pull/3621)
    - [CBR-446](https://iohk.myjetbrains.com/youtrack/issue/CBR-446), [#3650](https://github.com/input-output-hk/cardano-sl/pull/3650)
    - [CBR-462](https://iohk.myjetbrains.com/youtrack/issue/CBR-462) [#3704](https://github.com/input-output-hk/cardano-sl/pull/3704)
    - [CBR-464](https://iohk.myjetbrains.com/youtrack/issue/CBR-464) [#3710](https://github.com/input-output-hk/cardano-sl/pull/3710)
    - [CBR-471](https://iohk.myjetbrains.com/youtrack/issue/CBR-471) [#3786](https://github.com/input-output-hk/cardano-sl/pull/3786)
    - [CO-367](https://iohk.myjetbrains.com/youtrack/issue/CO-367) [#3661](https://github.com/input-output-hk/cardano-sl/pull/3661)
    - [CBR-366](https://iohk.myjetbrains.com/youtrack/issue/CBR-366) [#3469](https://github.com/input-output-hk/cardano-sl/pull/3469)
    - [RCD-44](https://iohk.myjetbrains.com/youtrack/issue/RCD-44) [#3875](https://github.com/input-output-hk/cardano-sl/pull/3875)
    - [RCD-45](https://iohk.myjetbrains.com/youtrack/issue/RCD-45) [#3875](https://github.com/input-output-hk/cardano-sl/pull/3875)
    - [RCD-46](https://iohk.myjetbrains.com/youtrack/issue/RCD-46) [#3875](https://github.com/input-output-hk/cardano-sl/pull/3875)
    - [RCD-47](https://iohk.myjetbrains.com/youtrack/issue/RCD-47) [#3876](https://github.com/input-output-hk/cardano-sl/pull/3876)
    - [CBR-495](https://iohk.myjetbrains.com/youtrack/issue/CBR-495) [#3943](https://github.com/input-output-hk/cardano-sl/pull/3943)
    - [CBR-496](https://iohk.myjetbrains.com/youtrack/issue/CBR-496) [#3947](https://github.com/input-output-hk/cardano-sl/pull/3947)

- Finalize port of API V0 to V1
    - [CO-334](https://iohk.myjetbrains.com/youtrack/issue/CO-334) [#3197](https://github.com/input-output-hk/cardano-sl/pull/3197)

- Expose ntp client api for makeing forceful ntp checks and a review of the ntp client code base [CDEC-355](https://iohk.myjetbrains.com/youtrack/issue/CDEC-355)
    - [#3323](https://github.com/input-output-hk/cardano-sl/pull/3323)
    - [#3264](https://github.com/input-output-hk/cardano-sl/pull/3264)

### Fixes

- #### Make productionReporter more robust
  Add exception handling code in reporting exception handler, to prevent IOExceptions from killing
  the main thread. This was noticed when the network connection was interrupted, and the reporter
  died when it tried to report over the down network. ([CDEC-470](https://iohk.myjetbrains.com/youtrack/issue/CDEC-470), [#3365](https://github.com/input-output-hk/cardano-sl/pull/3365))

- Improve type safety (and as a consequence, API documentation) of account indexes ([CBR-306](https://iohk.myjetbrains.com/youtrack/issue/CBR-306), [#3086](https://github.com/input-output-hk/cardano-sl/pull/3086))

- The Swagger specification had names with illegal characters. These names
  where changed to be URL friendly. [PR #3595](https://github.com/input-output-hk/cardano-sl/pull/3595)

- The creation of mnemonic doesn't throw anymore when provided words outside of the BIP39 English dictionnary.
  Instead, it returns an error value gracefully (CO-325)

- Response from `JSONValidationError` are now also encoded inline (instead of a pretty-encoding with newlines) ([DDW-318](https://iohk.myjetbrains.com/youtrack/issue/DDW-318), [#3619](https://github.com/input-output-hk/cardano-sl/pull/3619))

- **[API BREAKING CHANGE]** The behavior of `/api/v1/addresses/{address}` has been adjusted to reflect more accurately
  the meaning of ownership regarding addresses.
  The previous version of this endpoint failed with an HTTP error when the given address was unknown to the wallet.
  This was misleading since an address that is unknown to the wallet may still belong to the wallet. To reflect this,
  the V1 endpoint does not fail anymore as it used to when an address is not recognised and returns instead a new field
  'is-ours' which indicates either that an address is ours, or that it is 'not-recognised'. ([CBR-401](https://iohk.myjetbrains.com/youtrack/issue/CBR-401), [#3646](https://github.com/input-output-hk/cardano-sl/pull/3646))

 - **[API BREAKING CHANGE]** A DELETE request to `/api/v1/wallets/{wallet}` now correctly fails with 404 if the wallet doesn't exist. Previously it incorrectly responded with 204.

- Fix `commitAndReleaseBuffer: invalid argument (invalid character)` error in Docker image ([DEVOPS-877](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-877), [#3173](https://github.com/input-output-hk/cardano-sl/pull/3173))

- Fix deadlock triggered by update/shutdown ([CBR-424](https://iohk.myjetbrains.com/youtrack/issue/CBR-424) [#3731](https://github.com/input-output-hk/cardano-sl/pull/3731))

- Fix space leak during network syncing ([CBR-454](https://iohk.myjetbrains.com/youtrack/issue/CBR-454) [#3679](https://github.com/input-output-hk/cardano-sl/pull/3679))

- Fix benchmark on OSX ([CSL-2517](https://iohk.myjetbrains.com/youtrack/issue/CSL-2517) [#3348](https://github.com/input-output-hk/cardano-sl/pull/3348))

- Fix logger implementation, enabling pure logger to be used without side-effects ([CO-409](https://iohk.myjetbrains.com/youtrack/issue/CO-409), [#3697](https://github.com/input-output-hk/cardano-sl/pull/3697))

- Crash host node when the underlying wallet dies ([CBR-263](https://iohk.myjetbrains.com/youtrack/issue/CBR-263), [#3584](https://github.com/input-output-hk/cardano-sl/pull/3584))

- Ensure correct file permissions are set when generate x509 certificates ([CBR-470](https://iohk.myjetbrains.com/youtrack/issue/CBR-470), [#3773](https://github.com/input-output-hk/cardano-sl/pull/3773))

- Fix checksum verification in BIP-39 implementation ([CO-298](https://iohk.myjetbrains.com/youtrack/issue/CO-298), [#3013](https://github.com/input-output-hk/cardano-sl/pull/3013))

- Fix wallet starting bug introduced by [CDEC-509](https://iohk.myjetbrains.com/youtrack/issue/CDEC-509) ([CBR-400](https://iohk.myjetbrains.com/youtrack/issue/CBR-400): [#3486](https://github.com/input-output-hk/cardano-sl/pull/3486))

- Fix Haddock errors ([CDEC-585](https://iohk.myjetbrains.com/youtrack/issue/CDEC-585): [#3614](https://github.com/input-output-hk/cardano-sl/pull/3614))

- Fix restoration ignoring new accounts in legacy data layer ([DEVOPS-1153](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1153): [#3911](https://github.com/input-output-hk/cardano-sl/pull/3911))

- Tweaks to [Cardano Explorer](https://cardanoexplorer.com/) for [Testnet](https://testnet.iohkdev.io/cardano/) ([DEVOPS-1094](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1094), [#3817](https://github.com/input-output-hk/cardano-sl/pull/3817), [DEVOPS-1121](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1121), [#3831](https://github.com/input-output-hk/cardano-sl/pull/3831), [RCD-48](https://iohk.myjetbrains.com/youtrack/issue/RCD-48), [#3883](https://github.com/input-output-hk/cardano-sl/pull/3883))

- Fix bug in Windows launcher where upgrading to 2.0.0 would terminate any other Daedalus wallets that were running ([RCD-52](https://iohk.myjetbrains.com/youtrack/issue/RCD-52), [#3926](https://github.com/input-output-hk/cardano-sl/pull/3926))

### Improvements

- Friendly error mistakes from deserializing invalid addresses instead of brutal 500 ([CBR-283](https://iohk.myjetbrains.com/youtrack/issue/CBR-283))

- **[API BREAKING CHANGE]** Add `walletId` to `WalletAlreadyExists` WalletLayerError ([CBR-254](https://iohk.myjetbrains.com/youtrack/issue/CBR-254))

- Small refactor of wallet Errors implementation to be more maintainable ([CBR-26](https://iohk.myjetbrains.com/youtrack/issue/CBR-26), [#3429](https://github.com/input-output-hk/cardano-sl/pull/3429))

- Content-Type parser is now more lenient and accepts `application/json`, `application/json;charset=utf-8` and no Content-Type at all (defaulting to `application/json`) ([CO-369](https://iohk.myjetbrains.com/youtrack/issue/CO-369), [#3596](https://github.com/input-output-hk/cardano-sl/pull/3596))

- The codebase now relies on the package `cryptonite` (instead of `ed25519`) for Ed25519 implementation (CO-325)

- **[API BREAKING CHANGE]** Improve diagnostic for `NotEnoughMoney` error ([CBR-461](https://iohk.myjetbrains.com/youtrack/issue/CBR-461), [#3702](https://github.com/input-output-hk/cardano-sl/pull/3702))

- When Content-Type's main MIME-type cannot fall back to 'application/json' then UnsupportedMimeTypeError is returned ([CO-416](https://iohk.myjetbrains.com/youtrack/issue/CO-416), [#3727](https://github.com/input-output-hk/cardano-sl/pull/3727))

- Add `cardano-node --no-tls` option to wallet ([DEVOPS-879](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-879), [#3074](https://github.com/input-output-hk/cardano-sl/pull/3074))

- Improve error reporting when a worker thread in cardano dies ([DEVOPS-1063](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1063), [#3664](https://github.com/input-output-hk/cardano-sl/pull/3664))

- Add failure injection options to wallet for Daedalus testing ([DEVOPS-1086](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1086), [#3787](https://github.com/input-output-hk/cardano-sl/pull/3787))

- Benchmarking support ([PB-20](https://iohk.myjetbrains.com/youtrack/issue/PB-20) [PB-95](https://iohk.myjetbrains.com/youtrack/issue/PB-95) [#3670](https://github.com/input-output-hk/cardano-sl/pull/3670))

- #### Reorganize and clean up `core` packages
    - Move code from `infra` to `sinbin` so that `infra` can be moved "up" in the dependency graph ([CDEC-416](https://iohk.myjetbrains.com/youtrack/issue/CDEC-416): [#3185](https://github.com/input-output-hk/cardano-sl/pull/3185), [#3202](https://github.com/input-output-hk/cardano-sl/pull/3202), [#3209](https://github.com/input-output-hk/cardano-sl/pull/3209))
    - Move code from `networking` to `core`, so that `networking` can be moved "up" ([CDEC-432](https://iohk.myjetbrains.com/youtrack/issue/CDEC-432): [#3238](https://github.com/input-output-hk/cardano-sl/pull/3238), [#3261](https://github.com/input-output-hk/cardano-sl/pull/3261), [#3266](https://github.com/input-output-hk/cardano-sl/pull/3266))
    - Remove unnecessary `Mockable` typeclass ([CDEC-451](https://iohk.myjetbrains.com/youtrack/issue/CDEC-451): [#3285](https://github.com/input-output-hk/cardano-sl/pull/3285))
    - Switch uses of `pipes` to `conduit`, for consistency ([CDEC-464](https://iohk.myjetbrains.com/youtrack/issue/CDEC-464): [#3305](https://github.com/input-output-hk/cardano-sl/pull/3305))
    - Remove partial-function record accessors using `deriveIndexedBi` TH function ([CDEC-385](https://iohk.myjetbrains.com/youtrack/issue/CDEC-385): [#3153](https://github.com/input-output-hk/cardano-sl/pull/3153))
    - Create a script to visualize the package dependency graph within `cardano-sl` ([CDEC-429](https://iohk.myjetbrains.com/youtrack/issue/CDEC-429): [#3227](https://github.com/input-output-hk/cardano-sl/pull/3227))
    - Remove `Blockchain` class and clean up `Block` modules ([CDEC-333](https://iohk.myjetbrains.com/youtrack/issue/CDEC-333): [#3615](https://github.com/input-output-hk/cardano-sl/pull/3615))
    - Remove `HasProtocolConstants` reflection constraint in favour of explicit parameters ([CDEC-369](https://iohk.myjetbrains.com/youtrack/issue/CDEC-369): [#3482](https://github.com/input-output-hk/cardano-sl/pull/3482))
    - Remove remaining reflection constraints from `core` configuration ([CDEC-509](https://iohk.myjetbrains.com/youtrack/issue/CDEC-509): [#3437](https://github.com/input-output-hk/cardano-sl/pull/3437), [#3505](https://github.com/input-output-hk/cardano-sl/pull/3505), [#3522](https://github.com/input-output-hk/cardano-sl/pull/3522), [#3549](https://github.com/input-output-hk/cardano-sl/pull/3549), [#3550](https://github.com/input-output-hk/cardano-sl/pull/3550), [#3570](https://github.com/input-output-hk/cardano-sl/pull/3570))
    - Add `stylish-haskell` enforcement in CI to keep code conformant ([CDEC-383](https://iohk.myjetbrains.com/youtrack/issue/CDEC-383): [#3142](https://github.com/input-output-hk/cardano-sl/pull/3142))
    - Move `Block` datatypes from `core` to `chain` ([CDEC-485](https://iohk.myjetbrains.com/youtrack/issue/CDEC-485): [#3351](https://github.com/input-output-hk/cardano-sl/pull/3351))
    - Move chain-related `core` types to `chain` package ([CDEC-505](https://iohk.myjetbrains.com/youtrack/issue/CDEC-505): [#3412](https://github.com/input-output-hk/cardano-sl/pull/3412), [#3593](https://github.com/input-output-hk/cardano-sl/pull/3593), [#3600](https://github.com/input-output-hk/cardano-sl/pull/3600), [#3601](https://github.com/input-output-hk/cardano-sl/pull/3601), [#3611](https://github.com/input-output-hk/cardano-sl/pull/3611))
    - Add golden tests for `Undo` type ([CDEC-623](https://iohk.myjetbrains.com/youtrack/issue/CDEC-623): [#3735](https://github.com/input-output-hk/cardano-sl/pull/3735))
    - Reunite orphan instances
        - in `txp` ([CDEC-418](https://iohk.myjetbrains.com/youtrack/issue/CDEC-418): [#3179](https://github.com/input-output-hk/cardano-sl/pull/3179))
        - in `delegation` ([CDEC-423](https://iohk.myjetbrains.com/youtrack/issue/CDEC-423): [#3200](https://github.com/input-output-hk/cardano-sl/pull/3200))
        - in `chain` ([CDEC-484](https://iohk.myjetbrains.com/youtrack/issue/CDEC-484): [#3357](https://github.com/input-output-hk/cardano-sl/pull/3357))
        - in `lib` - `SafeCopy` instances ([CDEC-377](https://iohk.myjetbrains.com/youtrack/issue/CDEC-377): [#3135](https://github.com/input-output-hk/cardano-sl/pull/3135))
    - Remove `-fno-warn-orphans` from `ghc-options` in `update-test` ([CDEC-455](https://iohk.myjetbrains.com/youtrack/issue/CDEC-455): [#3296](https://github.com/input-output-hk/cardano-sl/pull/3296))
    - Move `Arbitrary` instances from `wallet` to `wallet-test` ([CDEC-437](https://iohk.myjetbrains.com/youtrack/issue/CDEC-437): [#3259](https://github.com/input-output-hk/cardano-sl/pull/3259))
    - Move `Pos.Core.Genesis.Canonical` to `Pos.Util.Json.Canonical` ([CDEC-513](https://iohk.myjetbrains.com/youtrack/issue/CDEC-513): [#3445](https://github.com/input-output-hk/cardano-sl/pull/3445))
    - Weed out unused package dependencies ([CDEC-425](https://iohk.myjetbrains.com/youtrack/issue/CDEC-425): [#3240](https://github.com/input-output-hk/cardano-sl/pull/3240))

- Improve readability and execution of various integration tests
    - [CO-398](https://iohk.myjetbrains.com/youtrack/issue/CO-398), [#3678](https://github.com/input-output-hk/cardano-sl/pull/3678)
    - [CO-400](https://iohk.myjetbrains.com/youtrack/issue/CO-400), [#3680](https://github.com/input-output-hk/cardano-sl/pull/3680)
    - [CO-424](https://iohk.myjetbrains.com/youtrack/issue/CO-424), [#3751](https://github.com/input-output-hk/cardano-sl/pull/3751), [#3780](https://github.com/input-output-hk/cardano-sl/pull/3780)
    - [CO-436](https://iohk.myjetbrains.com/youtrack/issue/CO-436), [#3791](https://github.com/input-output-hk/cardano-sl/pull/3791)
    - [CBR-408](https://iohk.myjetbrains.com/youtrack/issue/CBR-408), [#3571](https://github.com/input-output-hk/cardano-sl/pull/3571)
    - [CBR-417](https://iohk.myjetbrains.com/youtrack/issue/CBR-417), [#3568](https://github.com/input-output-hk/cardano-sl/pull/3568)
    - [CO-357](https://iohk.myjetbrains.com/youtrack/issue/CO-357), [#3573](https://github.com/input-output-hk/cardano-sl/pull/3573), [#3639](https://github.com/input-output-hk/cardano-sl/pull/3639)

- Add integration tests to test redemption of certificates ([CBR-398](https://iohk.myjetbrains.com/youtrack/issue/CBR-398), [#3525](https://github.com/input-output-hk/cardano-sl/pull/3525))

- Removal of partial field accessors from `HandlerSpec`, `InductiveValidationError`, `InvariantViolation` and `ValidationResult` data types.([CDEC-403](https://iohk.myjetbrains.com/youtrack/issue/CDEC-403): [#3263](https://github.com/input-output-hk/cardano-sl/pull/3263))

- Remove partial field accessors for `FakeTxsHistory` data type ([CDEC-285](https://iohk.myjetbrains.com/youtrack/issue/CDEC-285): [#3168](https://github.com/input-output-hk/cardano-sl/pull/3168))

- Review implementation of the BIP39 (Mnemonic Words) implementation
    - [CBR-288](https://iohk.myjetbrains.com/youtrack/issue/CBR-288) [#3128](https://github.com/input-output-hk/cardano-sl/pull/3128)
    - [CBR-289](https://iohk.myjetbrains.com/youtrack/issue/CBR-289) [#3043](https://github.com/input-output-hk/cardano-sl/pull/3043)

- Add a test which checks if the configuration can be correctly parsed
    - [CDEC-405](https://iohk.myjetbrains.com/youtrack/issue/CDEC-405) [#3175](https://github.com/input-output-hk/cardano-sl/pull/3175)

- Add Utxo not enough fragmentation error handling and multi-output transaction tests
    - [cardano-wallet#190](https://github.com/input-output-hk/cardano-wallet/issues/190) [#4058](https://github.com/input-output-hk/cardano-sl/pull/4058)

### Documentation

- Make an inventory of existing wallet errors and exceptions ([CBR-307](https://iohk.myjetbrains.com/youtrack/issue/CO-307))

- Various API documentation / guides fixes
    - [CO-327](https://iohk.myjetbrains.com/youtrack/issue/CO-327), [#3215](https://github.com/input-output-hk/cardano-sl/pull/3215)
    - [CO-328](https://iohk.myjetbrains.com/youtrack/issue/CO-328), [#3212](https://github.com/input-output-hk/cardano-sl/pull/3212)
    - [CO-351](https://iohk.myjetbrains.com/youtrack/issue/CO-351) [#3391](https://github.com/input-output-hk/cardano-sl/pull/3391)

- Documentation updates for Nix 2.0 ([DEVOPS-976](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-976), [#3311](https://github.com/input-output-hk/cardano-sl/pull/3311), [#3343](https://github.com/input-output-hk/cardano-sl/pull/3343))

### Continuous Integration (CI)

- Add a nightly test which syncs mainnet from scratch ([DEVOPS-1016](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1016), [#3487](https://github.com/input-output-hk/cardano-sl/pull/3487), [DEVOPS-1052](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1052), [#3626](https://github.com/input-output-hk/cardano-sl/pull/3626))
- CI speed improvements ([DEVOPS-1032](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1032), [#3544](https://github.com/input-output-hk/cardano-sl/pull/3544))
- Fixes for regeneration of `pkgs/default.nix` ([DEVOPS-1045](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1045), [#3677](https://github.com/input-output-hk/cardano-sl/pull/3677))
- Ensure the nix-shell environment is built and cached by Hydra ([DEVOPS-1059](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1059), [#3653](https://github.com/input-output-hk/cardano-sl/pull/3653))
- Add support for building cardano-sl with `cabal new-build` ([DEVOPS-1061](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1061), [#3662](https://github.com/input-output-hk/cardano-sl/pull/3662), [#3729](https://github.com/input-output-hk/cardano-sl/pull/3729), [#3734](https://github.com/input-output-hk/cardano-sl/pull/3734), [DEVOPS-1060](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1060), [#3675](https://github.com/input-output-hk/cardano-sl/pull/3675))
- Refactor nix files, add comments and documentation ([DEVOPS-1083](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1083), [#3728](https://github.com/input-output-hk/cardano-sl/pull/3728), [#3760](https://github.com/input-output-hk/cardano-sl/pull/3760), [#3761](https://github.com/input-output-hk/cardano-sl/pull/3761), [#3763](https://github.com/input-output-hk/cardano-sl/pull/3763), [DEVOPS-1004](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1004), [#3400](https://github.com/input-output-hk/cardano-sl/pull/3400), [DEVOPS-1067](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1067), [#3690](https://github.com/input-output-hk/cardano-sl/pull/3690))
- Add stylish-haskell tests to the Hydra build ([DEVOPS-936](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-936), [#3166](https://github.com/input-output-hk/cardano-sl/pull/3166), [#3189](https://github.com/input-output-hk/cardano-sl/pull/3189), [#3222](https://github.com/input-output-hk/cardano-sl/pull/3222))
- Improve nixpkgs pinning ([DEVOPS-779](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-779), [#3180](https://github.com/input-output-hk/cardano-sl/pull/3180))
- Fix caching of nix builds ([DEVOPS-810](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-810), [#3174](https://github.com/input-output-hk/cardano-sl/pull/3174))
- Ensure CI builds all targets (including benchmarks), and runs all tests ([DEVOPS-908](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-908), [#3130](https://github.com/input-output-hk/cardano-sl/pull/3130), [#3150](https://github.com/input-output-hk/cardano-sl/pull/3150))
- Build fixes for cardano-sl-explorer-frontend ([DEVOPS-916](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-916), [#3137](https://github.com/input-output-hk/cardano-sl/pull/3137), [#3146](https://github.com/input-output-hk/cardano-sl/pull/3146), [DEVOPS-999](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-999), [#3472](https://github.com/input-output-hk/cardano-sl/pull/3472))
- Fix `nix-build` when run from a `git` worktree ([DEVOPS-949](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-949), [#3691](https://github.com/input-output-hk/cardano-sl/pull/3691))
- Windows build fixes ([DEVOPS-957](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-957), [#3272](https://github.com/input-output-hk/cardano-sl/pull/3272), [DEVOPS-1003](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1003), [#3398](https://github.com/input-output-hk/cardano-sl/pull/3398))
- macOS build fixes (`clang: Argument list too long`) ([DEVOPS-1005](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1005), [#3432](https://github.com/input-output-hk/cardano-sl/pull/3432), [DEVOPS-1050](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1050), [#3606](https://github.com/input-output-hk/cardano-sl/pull/3606))
- Improve wallet integration tests ([DEVOPS-980](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-980), [#3325](https://github.com/input-output-hk/cardano-sl/pull/3325), [#3380](https://github.com/input-output-hk/cardano-sl/pull/3380), [DEVOPS-988](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-988), [#3346](https://github.com/input-output-hk/cardano-sl/pull/3346))
- Demo cluster and launch script fixes ([DEVOPS-985](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-985), [#3342](https://github.com/input-output-hk/cardano-sl/pull/3342), [DEVOPS-1062](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1062), [#3665](https://github.com/input-output-hk/cardano-sl/pull/3665), [DEVOPS-1101](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1101), [#3769](https://github.com/input-output-hk/cardano-sl/pull/3769))
- Add timing information to nix builds ([DEVOPS-1013](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1013), [#3457](https://github.com/input-output-hk/cardano-sl/pull/3457), [DEVOPS-1027](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1027), [#3509](https://github.com/input-output-hk/cardano-sl/pull/3509), [DEVOPS-1048](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1048), [#3706](https://github.com/input-output-hk/cardano-sl/pull/3706))
- Better code linting in CI ([DEVOPS-1031](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1031), [#3527](https://github.com/input-output-hk/cardano-sl/pull/3527), [DEVOPS-1057](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1057), [#3649](https://github.com/input-output-hk/cardano-sl/pull/3649), [DEVOPS-1100](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-1100), [#3762](https://github.com/input-output-hk/cardano-sl/pull/3762))


## Cardano SL 1.3.2

### Fixes

- Upgrade block version to 0.2.0 to limit max block size to 32 KB.

- Override max block size for epoch boundary blocks (also called
  genesis blocks) to have a limit of 1.953 MB.


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
