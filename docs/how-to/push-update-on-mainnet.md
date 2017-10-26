Overview
========

Software update is a protocol mechanism that allows nodes to agree on software changes without altering protocol constants and update to this software version.
1. Software update is proposed. Proposal is a datatype that gets into
   the blockchain. It contains information about version changes and
   hashes of update files.
2. Update files are uploaded to the S3 bucket.
3. Software update is confirmed by voting from majority of nodes.
4. Nodes that see an update try to download and apply it
   automatically.

Now, again, step-by-step.

Prerequisites
=============

0. For better understanding of update system, read cardanodocs:
  * https://cardanodocs.com/cardano/update-mechanism/
  * https://cardanodocs.com/technical/updater/
1. Cluster nodes' keys. They are crucial to have since cluster nodes
   are the only nodes that have stake in the system so they are the
   only nodes that can propose an update and vote for it. It's enough
   to have keys for majority of stake. In mainnet we have 7 core nodes
   with equal stake, so 4 keys should be enough.
2. Access to the S3 bucket
3. Software version
   - Single integer, must be greater by 1 than the last confirmed
     version. Initially `csl-daedalus` application has version 0. The
     first update should have version 1, the next one 2, etc. If you
     try to propose an update with version 3 while the last confirmed
     one is 1, your update proposal won't be accepted by the nodes.
   - Node knows its software version from `configuration.yaml` file,
     `applicationVersion` field. When you propose an update to version
     X, make sure that all attached installers use configuration with
     `applicationVersion` set to `X`.
4. Installers corresponding to software version we're pushing to cluster
   - Those are two executables (exe for win, pkg for mac) which are to
     be provided by QA (after QA procedures passed). They are
     installers provided by our CI.
   - Advice: after providing installers, ask QA to confirm hashes are
     same, we don't want to ship users incorrect installers to users.
   - IMPORTANT: pushing an installer of version N with an update with version
     M > N will result in the infinite installer loop (Daedalus thinks
     it is N, it downloads M, updates to it, but N is installed repeats), so
     versions should be double-checked.


Proposing the update
====================

Preparations
------------

You need to build packages `cardano-sl-tools` and `cardano-sl-auxx`. Branch's code should be compatible with cluster (take release branch, e.g. `cardano-sl-1.0`).

Ensure your keys are somewhere around and named, say, `keyX.sk`, where `X ∈ [0..6]`.

Before running all commands below, set command-line options appropriately. You need to change `CONFIG_KEY`, set `RELAY_PEER` (a host of any relay node) and possibly setting `--log-config`, `--logs-prefix` to real paths. Note that `--system-start 0` is perfectly valid and you don't need to change it.

The following variables will be used in all following commands:

```
CONFIG_KEY=mainnet_dryrun_full  # Should be the same as on the running nodes!
RELAY_PEER=<IP>:3000            # A host of any relay node.

COMMONOPTS="--system-start 0 --configuration-file ../cardano-sl/node/configuration.yaml --configuration-key ${CONFIG_KEY}"

AUXXOPTS="--log-config ../cardano-sl/scripts/log-templates/log-config-qa.yaml --logs-prefix logs/aux-update.1.0.1 --db-path aux-update-1.0.1 --peer ${RELAY_PEER}"
```

Sending an update proposal
------------------

Let's say that you want to push an update with the following Windows/macOS installers (note: it's important that the installers might be in current directory, i.e. there must be no slashes):

```
WIN64_INSTALLER=daedalus-win64-1.0.3350.0-installer.exe
DARWIN_INSTALLER=Daedalus-installer-1.0-rc.3202.pkg
```

First of all you have to import secret keys. Import secret keys: only 4 of 7 is needed (extra votes will be ignored). Cluster nodes all have equal stake and more than a half stake's votes is needed to make a decision about update (approve/dismiss). Imported keys are stored locally in `secret.key` which you **must** delete after completing this guide (unless there's a good valid reason not to).

```
stack exec -- cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "add-key key0.sk primary, add-key key1.sk primary, add-key key2.sk primary, add-key key3.sk primary, listaddr"
```

Flag `primary` indicates that signing secret key will be added.

If the listaddr command hasn't printed the addresses belonging to the four keys, you did something wrong.

To create and send an update proposal with votes from all imported secret keys, you need to run this command:

```
stack exec -- cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "propose-update 0 vote-all 0.0.0 0 20 2000000 csl-daedalus:1 win64 ${WIN64_INSTALLER} none macos64 ${DARWIN_INSTALLER} none"
```

You also can combine importing and sending a proposal in one command for your convenience:


```
stack exec -- cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "add-key key0.sk primary, add-key key1.sk primary, add-key key2.sk primary, add-key key3.sk primary, propose-update 0 vote-all 0.0.0 0 20 2000000 csl-daedalus:1 win64 ${WIN64_INSTALLER} none macos64 ${DARWIN_INSTALLER} none"
```

Let's break down the invocation of `propose-update`. First come arguments that you almost certainly won't need to modify:

* `0` is the index of key that will be used to sign the update.
* `0.0.0` is block version to be used after the update. Currently it's `0.0.0` as well and so the update won't change the block version.
* `0` is new script version. Again, it's the same as the current one.
* `20` is new slot duration (in seconds). Unchanged.
* `2000000` is new maximum block size (in bytes). Unchanged.

The next argument (`csl-daedalus:1`) is software version description. You should substitute `1` (version) with the integer provided along with installers (see *Prerequisites* section).

Next follow an arbitrary number of _triples_ (two in our case, one for Windows and one for macOS). Each triple is `<system tag> <installer filename> none`. Once again, those are filenames in the current dir, not arbitrary paths. `none` stands for binary diff (this feature is not used for now).

Successfull command output looks like this:

```
[smart-wallet:DEBUG] [2017-09-21 15:13:24 MSK] Proposing update...
Read file daedalus-win64-1.0.3350.0-installer.exe succesfuly, its hash: 01abf1c8b881c2f8ea4d1349a700f29d4088e68dc04b6bf4680ea7e14638373e
Read file installer062macos64.pkg succesfuly, its hash: 3bc1084841fb99fff03ef92bc35eef9a80a20aeca688dfc1b50a4aa6dd6f7c73
[smart-wallet:INFO] [2017-09-21 15:13:25 MSK] Announcing proposal with id 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6
Update proposal submitted, upId: 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6
```

Note:

```
Read file daedalus-win64-1.0.3350.0-installer.exe succesfuly, its hash: 01abf1c8b881c2f8ea4d1349a700f29d4088e68dc04b6bf4680ea7e14638373e
Read file Daedalus-installer-1.0-rc.3202.pkg succesfuly, its hash: 3bc1084841fb99fff03ef92bc35eef9a80a20aeca688dfc1b50a4aa6dd6f7c73
```

These hashes should be used to form URLs for installers on S3
buckets. In this example the URL `<update
server>/01abf1c8b881c2f8ea4d1349a700f29d4088e68dc04b6bf4680ea7e14638373e`
should respond with the contents of `daedalus-win64-1.0.3350.0-installer.exe` and the URL
`<update
server>/3bc1084841fb99fff03ef92bc35eef9a80a20aeca688dfc1b50a4aa6dd6f7c73`
should respond with the contents of `Daedalus-installer-1.0-rc.3202.pkg`.

These hashes are Blake2b_256 hashes of CBOR-encoded contents of the files.
There is a simple command to calculate the hash of an installer:

```
stack exec -- cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "hash-installer <FILEPATH>"
```

Note:

```
Update proposal submitted, upId: 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6
```

Value `upId` is used on one of next steps, when we'll vote for update. You may also look the first six characters of the hash on Papertrail to see whether the proposal was accepted by nodes or not.

Uploading update files
======================

Upload installers to S3 bucket:
* For staging:
    * URL: https://s3.eu-central-1.amazonaws.com/update-system-testing/
    * S3 Bucket: `update-system-testing`
* For mainnet:
    * URL: https://update.cardano-mainnet.iohk.io
    * S3 Bucket: `update.cardano-mainnet.iohk.io` in mainnet AWS role

Download server URL is passed to the launcher/node in the following way:
https://github.com/input-output-hk/daedalus/blob/eb713a66eb2c0445fbe8c2faa59f0884edd83712/installers/Launcher.hs#L68

Voting for proposal
===================

Variables:

```
PROPOSAL_ID=4a57dd56563149eb4429024e51709807b88d6306b81eb2f9aa5fa303bc7bbf44
```

Command:

```
for idx in {1..3}; do
  stack exec -- cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "vote ${idx} y ${PROPOSAL_ID}"
done
```

`y` stands for “vote ‘yes’”. Successfull output ends in "submitted a vote".
Votes will be sent to the network, software update will apply soon (after
`k` blocks, for more details read cardanodocs, links are in prerequisites).

Deleting artefacts
===================

After everything is done don't forget to destroy `secret.key` file
because it contains private information (secret keys of core nodes).
