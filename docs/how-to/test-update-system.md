# Testing update system

## Prepare CSL binaries, cluster

* Launch cluster using `devnet_shortep_full` config, on:
   * 4 core nodes (full connectivity)
   * 1 relay (connected to all core nodes)
* Generate *before* installer, use `devnet_shortep_wallet`
   * set appropriate `system-start`, `ipdhtmappings` for daedalus
* Generate *after* installer, use `devnet_shortep_updated_wallet`
   * set appropriate `system-start`, `ipdhtmappings` for daedalus
* Build cardano-sl with `devnet_shortep_full` (`./scripts/build/cardano-sl.sh --dn`)

## Test base version

1. Download *before* installers
2. Install & launch daedalus
3. Check:
    * blocks are retrieved
    * transactions are sent

## Propose update & vote for it

### Prepare node keys

1. Take genesis folder corresponding to `devnet_shortep_full`
Should have name like `genesis-dn-2017-08-31_15-28.tgz`
2.

```
cp -R <genesis_directory>/keys-testnet/rich nodes2
cd nodes2
for i in {0..4}; do stack exec -- cardano-keygen rearrange --mask testnet$i.key; done
```

### Cardano-wallet

Launch `cardano-wallet` with CLI:

```
stack exec -- cardano-wallet --system-start 0 --log-config log-config-prod.yaml --logs-prefix "logs/abc4" --db-path db-abc4 --peer {relayHost}:3000 repl
```

(!) Replace:

`{relayHost}` with address of actual relay node

Then you will appear in `repl` mode and would need to perform few actions:

#### Import keys

```
add-key nodes2/testnet1.key
add-key nodes2/testnet2.key
add-key nodes2/testnet3.key
add-key nodes2/testnet4.key
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

Replace second argument, `0.1.0`, with actual block version from config (i.e. `lastKnownBVMajor.lastKnownBVMinor.lastKnownBVAlt`), if needed.

Replace third argument, `1` with actual script version.

Replace `csl-daedalus:1` with `applicationName:applicationVersion` as for config.

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
Note that currently `cardano-wallet` is suitable for testing with only one system tag provided along installer for it (it's `win64` and `daedalus1.exe` here).

In blockchain should be:

```
43992 [node.worker.block:INFO:ThreadId 27399] [2017-09-01 10:02:07 UTC] Created a new block:
43993 MainBlock:
43994   MainBlockHeader:
43995     hash: 21cc9a0a0400d81bc20c54d5a404d6b5080069d2c15a229191a1a4d29023cda8
43996     previous block: 100050c72eeffa5fa6a78f899aa214255e16f1940f5b209f34939cff243db1d2
43997     slot: 89th slot of 3rd epoch
43998     difficulty: 360
43999     leader: pub:2c434a37
44000     signature: BlockSignature: <signature>
44001     block: v0.0.0
44002     software: cardano-sl:0
44003   transactions (0 items): []
44004   proxy signing keys (0 items): []
44005     no GodTossing payload
44006   update payload: csl-daedalus:1 { block v0.1.0, UpId: f3fe3a62, { scripts v1, slot duration: 15000 mcs, block size limit: 1.907 MiB, header size limit: 195.313 KiB, tx size limit: 4 KiB, proposal size           limit: 700 B, mpc threshold: 20000000000000/1000000000000000 (approx. 0.02), heavyweight delegation threshold: 300000000000/1000000000000000 (approx. 0.0003), update vote threshold: 1000000000000/                1000000000000000 (approx. 0.001), update proposal threshold: 100000000000000/1000000000000000 (approx. 0.1), update implicit period: 10000 slots, no softfork rule, no tx fee policy, unlock stake epoch:  },       tags: [win64], no attributes } 
44007     votes: [(c9a26b08 for f3fe3a62)]
44008   no extra data
```

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

In logs should be:
```
[node.worker.block:INFO:ThreadId 3393] [2017-09-01 10:14:52 UTC] Created a new block:
MainBlock:
  MainBlockHeader:
    hash: 05a80146a0cd07fb4bb807cdeb39bda9df2d2ebd38317b4eba2f749471af2a67
    previous block: d35a5aaf59a19c44f5f4a4673b571d05d49935b071b34271af3c383b816ecc0a
    slot: 50th slot of 4th epoch
    difficulty: 411
    leader: pub:80b70572
    signature: BlockSignature: <signature>
    block: v0.0.0
    software: cardano-sl:0
  transactions (0 items): []
  proxy signing keys (0 items): []
    no GodTossing payload
  update payload: no proposal
    votes: [(18e62bbf for f3fe3a62)]
  no extra data
```

### Check proposal, votes got to blocks

1. Retrieve logs from cluster (`io --no-component-check -c csl-1583.yaml get-journals`)
2. Check latest blocks contain one proposal and 2 votes (not four because three is already enough for proposal to be confirmed)

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
