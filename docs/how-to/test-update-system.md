# Testing update system

## Prepare initial version of `cardano-sl`

THIS SECTION IS OUTDATED AND MOST PROBABLY CAN BE SKIPPED

These steps should be done to prepare configuration for QA cluster, suitable for US testing. There is no need to perform these steps every time you want to launch a cluster, only if you want to launch it form different release branch used as base

1. Branch out `csl-xx` (`csl-xx` is reference to task in YT) out of release branch `cardano-sl-Y.Z`
2. Change `core/constants-prod.yaml`, `core/constants-wallet-prod.yaml`:
     1. `k: 9`
     2. `genesisSlotDurationSec: 15`
     3. `networkDiameter: 10`
3. Generate `genesis.bin` for US testing cluster:

   (!) Ensure you compiled branch `csl-xx` in prod mode, e.g. with `./util-scripts/build.sh --prod`

   ```
M=4 N=200 ./util-scripts/generate-genesis.sh
cp genesis-qanet-<current_date>/genesis.* .
   ```
4. Commit changes:
```
git add genesis.info genesis.bin core/constants*prod.yaml
git commit -m "Updated constants, genesis"
git push --set-upstream origin csl-xx
```

5. Build `csl-xx` branch in `--prod` mode

   This is needed for later steps.

## Cluster for US testing, installers

You need launch cluster with name `csl-xx` on `staging` user of cardano-deployer. This process is described in different place (**insert_link_here**).

Use HEAD of `csl-xx` branch as commit for `cardano-sl`.

After launching deployment, you will have:
* `system-start` value
* `ip-dht-mappings` file

After build of `csl-xx` branch is finished in CI, you need to update `daedalus` for installers to be built:
1. Use same branch `cardano-sl-Y.Z` of daedalus, branch out `csl-xx`
2. Change branch from `cardano-sl-Y.Z` to `csl-xx` in `.travis.yml`, `appveyor.yaml`
3. Update `system-start` in `installers/Launcher.hs`
4. Replace `ip-dht-mappings` in `installers/data/`
5. Commit & push changes

Then wait for CI to build installers for base version.

## Test base version

1. Download installers for `csl-xx`
2. Install & launch daedalus
3. Check:
    * blocks are retrieved
    * transactions are sent

## Prepare updated version

1. Update `constants.yaml`:
    * increment `lastKnownBVMinor`
    * increment `applicationVersion`
2. Commit changes
3. Wait for `cardano-sl` to be built
4. Trigger `daedalus` build (installers)

## Propose update & vote for it

### Prepare node keys

1. Take genesis deployed to `csl-xx` cluster
2. ```
cp -R <genesis_directory>/nodes nodes2
cd nodes2
for i in {0..4}; do stack exec -- cardano-keygen --rearrange-mask key$i.sk; done
```

### Cardano-wallet

Launch `cardano-wallet` with CLI:

```
stack exec -- cardano-wallet --system-start 1498572075 --log-config log-config-prod.yaml --logs-prefix "logs/abc4" --db-path db-abc4 --peer 52.57.33.126:3000 --peer 52.57.215.206:3000 --peer 34.253.40.70:3000 --peer 52.211.96.220:3000 repl
```

(!) Replace:

1. `--peer` parameters with ips of the `csl-xx` cluster nodes
2. `--system-start` parameter with system start for `csl-xx` cluster

Then you will appear in `repl` mode and would need to perform few actions:

#### Import keys

```
add-key nodes2/key1.sk
add-key nodes2/key2.sk
add-key nodes2/key3.sk
add-key nodes2/key4.sk
```

Then if you execute

```
listaddr
```

You should see something like:

```
> listaddr
Available addresses:
    #0:   1feg76nzCcJrhwX4Z9NXc9iwNwo1UxUdkaF3XwdyFhtDdst (PK: eMrhrnbwqmRmeHNQW6kyrD9gs4sMtUyGRqPuLsiZaaTLhHY4uojgp7imLNX6tsECechdqLUB92APfs6Er1BJv6E)
    #1:   1gE3qzmCe4cjReAWwJRv6iHHmHDzoejG9Mkp9sGF5qv6SQb (PK: 3E36aezxjRFHz8viGTTpu7us5mPej5d83195GFoHcSnWKCcVpduxKVLuAxxNWN2w9sRsDzdxfVeQpqWkumEJGYUN)
    #2:   1gK2AvmDBSTYJgqLaJbmY3r1qgC55SyKeQCTjYDPLTxpyRN (PK: 2vCzGFduRi6obJaDitkz1DNqckGerE2oos7ocLRzeaCV28zvvkcKRsofDDWwpML5JaHUjeLKjFGnjmCZnhu2XjGJ)
    #3:   1fNkZ1oyrV9xGaPizPHMM2hzwdwERyDPQrcLqLz76mqNxC6 (PK: 4856H9Czc65Y7ueCMdhnPtDwUZvH6g5uBEi1TkoN9CqE75ENbSx8jx8ebDgSDFX7A869qNjBe4n9SgZ74NSyr7xx)
```

#### Propose update

```
propose-update 0 0.1.0 1 15 2000000 csl-daedalus:1 win64 daedalus1.exe
```

Replace second argument, `0.1.0`, with actual block version from `core/constants-prod.yaml` (i.e. `lastKnownBVMajor.lastKnownBVMinor.lastKnownBVAlt`).

Replace third argument, `1` with actual script version.

Replace `csl-daedalus:1` with `applicationName:applicationVersion` as for `core/constants-prod.yaml`.

Note that currently `cardano-wallet` is suitable for testing with only one system tag provided along installer for it (it's `win64` and `daedalus1.exe` here).

After launching `propose-update` command you'll see output like this:

```
> propose-update 0 0.1.0 1 15 2000000 cardano-sl:1 win64 daedalus1.exe                               
Read file succesfuly, its hash: e0dae787e163a973ef4e1260dfcf094431046ae3e17d67d601bce4d92eb7da27
[smart-wallet:INFO:ThreadId 168] [2017-05-11 02:17:20 MSK] Announcing proposal with id b66ae7e037ca3503224e8d5b716443b6480df97be114c899f3e7397419e897c1               [smart-wallet:INFO:ThreadId 170] [2017-05-11 02:17:20 MSK] Announcing proposal with id b66ae7e037ca3503224e8d5b716443b6480df97be114c899f3e7397419e897c1
[smart-wallet:INFO:ThreadId 172] [2017-05-11 02:17:20 MSK] Announcing proposal with id b66ae7e037ca3503224e8d5b716443b6480df97be114c899f3e7397419e897c1
[smart-wallet:INFO:ThreadId 174] [2017-05-11 02:17:20 MSK] Announcing proposal with id b66ae7e037ca3503224e8d5b716443b6480df97be114c899f3e7397419e897c1
Update proposal submitted, upId: b66ae7e037ca3503224e8d5b716443b6480df97be114c899f3e7397419e897c1 
```

Note `e0dae787e163a973ef4e1260dfcf094431046ae3e17d67d601bce4d92eb7da27`, it's hash of installer. It would be referenced later as `installer_hash`.

Note `b66ae7e037ca3503224e8d5b716443b6480df97be114c899f3e7397419e897c1`, it's id of proposed updated and later would be referenced as `upId`.

#### Vote for update

Wait for 30-60 seconds.

Execute
```
vote 1 y <upId>
vote 2 y <upId>
vote 3 y <upId>
```

This will result in output like:

```
> vote 1 y b66ae7e037ca3503224e8d5b716443b6480df97be114c899f3e7397419e897c1
Submitted vote
> vote 2 y b66ae7e037ca3503224e8d5b716443b6480df97be114c899f3e7397419e897c1
Submitted vote
> vote 3 y b66ae7e037ca3503224e8d5b716443b6480df97be114c899f3e7397419e897c1
Submitted vote
```

### Check proposal, votes got to blocks

1. Retrieve logs: 
`./CardanoCSL.hs -c production.yaml dumplogs` in the `csl-xx`'s deployment directory
2. `cd experiments/<current_date>`
3. `../../scripts/blocks.sh`
3. Check latest blocks contain one proposal and three votes (not four because three is already enough for proposal to be confirmed)

Example excerpt from `blocks.sh` output:

```
[2017-05-10_23:17:22] node3.log: MainBlock:
  MainBlockHeader:
    hash: c844a900d356ae6019782dabc2e6e446876beb74475cb99557ff655925af1048
    previous block: 52552cfc21e2e42697831dedb02d2b6cc4b96c4c0e948e9047b2186eb9b9fb45
    slot: 41st slot of 11th epoch
    leader: pub:77d84faa
    difficulty: 983
    block: v0.0.0
    software: cardano-sl:0
  transactions (0 items): []
  proxy signing keys (0 items): []
  no GodTossing payload
  update payload: cardano-sl:1 { block v0.1.0, UpId: b66ae7e0, { scripts v1, slot duration: 15000 mcs, block size limit: 1.907 MiB, header size limit: 195.313 KiB, tx
 size limit: 4 KiB, proposal size limit: 700 B, mpc threshold: 20000000000000/1000000000000000 (approx. 0.02), heavyweight delegation threshold: 300000000000/1000000000000000 (approx. 0.0003), update vote threshold: 1000000000000/1000000000000000 (approx. 0.001), update proposal threshold: 100000000000000/1000000000000000 (approx.
 0.1), update implicit period: 10000 slots, update softfork threshold: 750000000000000/1000000000000000 (approx. 0.75) }, tags: [win64], no attributes  }
    votes: [(6a77dec1 for b66ae7e0)]
  no extra data

[2017-05-10_23:24:22] node0.log: MainBlock:
  MainBlockHeader:
    hash: 38990d7299638f950bddd9643d2185ac52fc357823b2c973d667ef14a18754c1
    previous block: b5beed9ad8d53d8ea4b389a17e9466189bd18fd0387737b69114f270b016b084
    slot: 69th slot of 11th epoch
    leader: pub:b4cbfbed
    difficulty: 1011
    block: v0.0.0
    software: cardano-sl:0
  transactions (0 items): []
  proxy signing keys (0 items): []
  no GodTossing payload
  update payload: no proposal
    votes: [(653d9bf1 for b66ae7e0)]
  no extra data
[2017-05-10_23:24:37] node1.log: MainBlock:
  MainBlockHeader:
    hash: 4bdfbddbba37b1715d5c5e298e4184dca75e8d508c5dc12e273609a73903e7ca
    previous block: 38990d7299638f950bddd9643d2185ac52fc357823b2c973d667ef14a18754c1
    slot: 70th slot of 11th epoch
    leader: pub:150d631e
    difficulty: 1012
    block: v0.0.0
    software: cardano-sl:0
  transactions (0 items): []
  proxy signing keys (0 items): []
  no GodTossing payload
  update payload: no proposal
    votes: [(4c2211c6 for b66ae7e0)]
  no extra data
```

### Wait till appropriate epoch starts

Let `(e, s)` be slot of block including last vote.

If `s < 72`, then wait till slot `(e + 1, 0)`.
Otherwise till `(e + 2, 0)`.

This is needed for all nodes and wallets to consider proposal confirmed and valid to be applied.

## Check update taken by wallet

### Prepare installer mirror

1. Launch simple static HTTP server, e.g. `Fenix` for Windows
2. Configure it to serve contents of particular `installers` directory on port 8080
3. Copy *updated* installer to `installers/<installer_hash>`

### Prepare bat file

Edit `daedalus.bat` file:
1. Add `-n --update-server http://localhost:8080/` before `-n --system-start`
2. Add  `-n --update-with-package` to the end

Bat file would look like:

```

SET DAEDALUS_DIR=%~dp0
start /D "%DAEDALUS_DIR%" cardano-launcher.exe --node "%DAEDALUS_DIR%\cardano-node.exe" --node-log-path "%APPDATA%\Daedalus\Logs\cardano-node.log" --wallet "%DAEDALUS_DIR%\Daedalus.exe" --updater "%APPDATA%\Daedalus\Installer.exe" --updater-windows-runner "%APPDATA%\Daedalus\Updater.bat" --node-timeout 5  -n --listen -n 127.0.0.1:12100 -n --report-server -n http://52.59.7.118:8080 -n --log-config -n log-config-prod.yaml -n --update-latest-path -n "%APPDATA%\Daedalus\Installer.exe" -n --keyfile -n "%APPDATA%\Daedalus\Secrets\secret.key" -n --logs-prefix -n "%APPDATA%\Daedalus\Logs" -n --db-path -n "%APPDATA%\Daedalus\DB-0.4" -n --wallet-db-path -n "%APPDATA%\Daedalus\Wallet-0.4" -n --peers-file -n ip-dht-mappings -n --update-server -n "http://localhost:8080/" -n --system-start -n 1494442772 -n --wallet -n --explicit-initial -n --update-with-package

```

### Launch Daedalus on desktop

Should be installed with *old* installers !!!

Launch, wait for it to retrieve latest blockchain.

Check logs, there should be lines like:

**TODO: post excerpt from logs**

```
[DEBUG] Downloading update...
```

Installer should be put to `%APPDATA%\Daedalus\Installer.exe`.

Daedalus should exit and let launcher execute installer, though as for *11 May 2017* this is not working. This way you can close Daedalus and launch it again, it should attempt to launch installer.
