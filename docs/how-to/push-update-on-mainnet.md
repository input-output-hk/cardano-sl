Overview
========

TODO CSL-1819 This document is outdated!

Software update is a protocol mechanism that allows nodes to agree on
software changes without altering protocol constants and update to
this software version.

Protocol update is a different protocol mechanism that allows nodes to
agree on protocol changes without breaking compatibility with older
versions. Protocol update must be done with software update
(i. e. it's impossible to make a protocol update without a software
update). Note, however, that software update can be done for only one
software. Usually it will be `cardano-sl` which is used by core and
relay nodes and differs from `csl-daedalus` which is used by end users.

Currently we support two different applications (from the standpoint
of update system): `cardano-sl` and `csl-daedalus`. The former is used
by core and relay nodes, the latter is used by end-users
(wallets). One of the goals of update system is to provide a way to
know whether a particular software is approved by major stakeholders
by looking at blockchain. For `csl-daedalus` it makes perfect sense:
from blockchain we know hashes of installers which are approved. But
for `cardano-sl` it doesn't make sense, because we don't even have a
notion of _installer_ for it. So when we update `cardano-sl` it
doesn't make sense to attach any data about installers into update
proposal. There are few reasons to propose an update of `cardano-sl`:
1. It allows us to make a protocol update without updating end
   users. For example we may want to change some protocol parameter
   and not bother users with updates. Note that we can also propose an
   update of non-existing application, but it doesn't make much sense
   to pollute the namespace.
2. We can permanently register updates of software used by core nodes
   in blockchain to be able to review this history in future (not
   essential, probably doesn't deserve spending time on it).

Flow for an update is the following (brief sketch, no details):
1. Software update is proposed. Proposal is a datatype that gets into
   the blockchain. It contains information about version changes and
   hashes of update files (can be empty). It also contains
   `BlockVersion` and `BlockVersionData`. They define protocol version
   and its parameters. They can be the same as the previous ones (in
   which case there is no protocol update) or can differ (then there
   is also a protocol update).
2. New software is prepared, some values in configuration are
   changed. If we want to update `csl-daedalus`, new installers are
   prepared. If we want to update `cardano-sl`, everything should be
   setup to redeploy core and relay nodes.
3. If wallets should be updated, installers are uploaded to the S3 bucket.
4. An update is confirmed by voting from majority of nodes.
5. Nodes that see an update try to download and apply it
   automatically. Currently it's enabled only for wallets
   (`csl-daedalus`), core and relay nodes are controlled by us.
6. (Protocol update case). Core and relay nodes are restarted with
   newer configuration and start using newer `BlockVersion`. Their
   `BlockVersion`s are included into created blocks.
7. (Protocol update case). When almost all nodes (90% in mainnet, but
   this value is configurable and slowly decreases over time after an update
   is confirmed) create a block with newer `BlockVersion`, it gets
   adopted and all nodes in the network adhere to the newest protocol rules.

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
2. Access to the S3 bucket (if wallets are updated, i. e. `csl-daedalus`).
3. Software version
   - Single integer, must be greater by 1 than the last confirmed
     version. Initially `csl-daedalus` application has version 0. The
     first update should have version 1, the next one 2, etc. If you
     try to propose an update with version 3 while the last confirmed
     one is 1, your update proposal won't be accepted by the nodes.
   - Node knows its software version from `configuration.yaml` file,
     `applicationVersion` field. When you propose an update to version
     X, make sure that all attached installers use configuration with
     `applicationVersion` set to `X` (described below).
4. Block version and block version data. Block version consists of
   three integers. Block version data contains various parameters of the
   protocol. Currently the best way to figure out these values is to ask
   in Slack. To be improved…
5. If `csl-daedalus` is updated, installers corresponding to software
     version we're pushing to cluster should be prepared.
   - Those are two executables (exe for win, pkg for mac) which are to
     be provided by QA (after QA procedures passed). They are
     installers provided by our CI.
   - Advice: after providing installers, ask QA to confirm hashes are
     same, we don't want to ship users incorrect installers to users.
   - **IMPORTANT**: pushing an installer of version N with an update with version
     M > N will result in the infinite installer loop (Daedalus thinks
     it is N, it downloads M, updates to it, but N is installed repeats), so
     versions should be double-checked.
6. If protocol update is performed, it's necessary to restart core
   nodes, because protocol update takes place only after blocks are
   created by nodes with new protocol version (and core nodes are
   the only nodes which can create blocks). This is true regardless
   of whether software update is done for `cardano-sl` or
   `csl-daedalus`. Note: even if no logic should be updated for core
   nodes, at least configuration must be changed
   (`lastKnownBlockVersion`).

Proposing the update
====================

Configuration
-------------

You need to have software (with proper configuration) which is ready
to be used as soon as update proposal is confirmed. Pay very close
attention to the configuration and check that `applicationVersion` and
`lastKnownBlockVersion` are the same as the values you are going to
use in update proposal (for the corresponding configuration key)! If
they are not, set by yourself or ask someone to do it.

Environment preparations
------------------------

You need to build `cardano-sl-auxx` package. Branch's code should be
compatible with cluster (take release branch, e.g. `cardano-sl-1.0`).

Ensure your keys are somewhere around and named, say, `keyX.sk`, where `X ∈ [0..6]`.

Before running all commands below, set command-line options appropriately. You need to change `CONFIG_KEY`, set `RELAY_PEER` (a host of any relay node) and possibly setting `--log-config`, `--logs-prefix` to real paths. Note that `--system-start 0` is perfectly valid and you don't need to change it.

The following variables will be used in all following commands:

```
CONFIG_KEY=mainnet_dryrun_full  # Should be the same as on the running nodes!
RELAY_PEER=<IP>:3000            # A host of any relay node.

COMMONOPTS="--system-start 0 --configuration-file ../cardano-sl/node/configuration.yaml --configuration-key ${CONFIG_KEY}"

AUXXOPTS="--log-config ../cardano-sl/scripts/log-templates/log-config-qa.yaml --logs-prefix logs/aux-update.1.0.1 --db-path aux-update-1.0.1 --peer ${RELAY_PEER}"
```

Feel free to change the paths above (ending with `aux-update-1.0.1`)
to desired locations.

Sending an update proposal
------------------

Let's say that you want to push an update with the following Windows/macOS installers (note: it's important that the installers must be in current directory, i.e. there must be no slashes):

```
WIN64_INSTALLER=daedalus-win64-1.0.3350.0-installer.exe
DARWIN_INSTALLER=Daedalus-installer-1.0-rc.3202.pkg
```

Or you may want to update `cardano-sl` application, in this case you
don't need any installers.

First of all you have to import secret keys. Import secret keys: only 4 of 7 is needed (extra votes will be ignored). Cluster nodes all have equal stake and more than a half stake's votes is needed to make a decision about update (approve/dismiss). Imported keys are stored locally in `secret.key` which you **must** delete after completing this guide (unless there's a good valid reason not to).

It's assumed that you are doing everything from scratch and don't have
`secret.key` file before using the command below.

```
stack exec -- cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "add-key key0.sk primary, add-key key1.sk primary, add-key key2.sk primary, add-key key3.sk primary, listaddr"
```

Flag `primary` indicates that signing secret key will be added.

If the `listaddr` command hasn't printed the addresses belonging to the four keys, you did something wrong.

To create and send an update proposal with votes from all imported secret keys, you need to run this command:

```
stack exec -- cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "propose-update 0 vote-all 0.1.0 65536 70000 <software-version> [win64 ${WIN64_INSTALLER} none macos64 ${DARWIN_INSTALLER} none]"
```

Notice that the last part of this command is optional, its presence
depends on whether you want to update `cardano-sl` or `csl-daedalus`.

Examples:

```
stack exec -- cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "propose-update 0 vote-all 0.1.0 65536 70000 csl-daedalus:1 win64 ${WIN64_INSTALLER} none macos64 ${DARWIN_INSTALLER} none"
stack exec -- cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "propose-update 0 vote-all 0.1.0 65536 70000 cardano-sl:1"
```

You also can combine importing keys and sending an update proposal in
one command for your convenience. Example:

```
stack exec -- cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "add-key key0.sk primary, add-key key1.sk primary, add-key key2.sk primary, add-key key3.sk primary, propose-update 0 vote-all 0.1.0 65536 70000 csl-daedalus:1 win64 ${WIN64_INSTALLER} none macos64 ${DARWIN_INSTALLER} none"
```

Let's break down the invocation of `propose-update`. First come arguments that you almost certainly won't need to modify:

* `0` is the index of key that will be used to sign the update.
* `vote-all` flag means that update proposal will be submitted along
  with votes from all our keys.
* `0.1.0` is block version to be used after the
  update. See [Prerequisites](#prerequisites), you should know it in
  advance. `0.1.0` is used as an example.
* `65536` is maximal tx size. Should be set to this value (`65536`) in `0.1.0`.
* `70000` is maximal size of update proposal. Should be set to this
  value (`70000`) in `0.1.0`.

The next argument (`<software-version>`, e. g. `csl-daedalus:1`) is
software version description. You should substitute `1` (version) with
the integer provided along with installers (see *Prerequisites*
section). Recall that currently we are maintaining two softwares:
`cardano-sl` (used by core and relay nodes) and `csl-daedalus` (used
by wallets).

Further arguments are optional, should be provided in `csl-daedalus`
case and omitted in `cardano-sl` case. Those arguments are an arbitrary number
of _triples_ (two in `csl-daedalus` case, one for Windows and one for
macOS). Each triple is `<system tag> <installer filename> none`. Once
again, those are filenames in the current dir, not arbitrary
paths. `none` stands for binary diff (this feature is not used for
now). System tags should corresponds to `systemTag` values in
configuration file.

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

Value `upId` can be used later to know what happened with the proposal
from logs. E. g. to know whether the proposal has been confirmed.

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

Deleting artefacts
===================

After everything is done don't forget to destroy `secret.key` file
because it contains private information (secret keys of core nodes).

Restarting core nodes (protocol update)
==============================================

If protocol update is being performed, it's necessary to restart core
nodes, as was stated earlier.

**IMPORTANT**: restart must happen **after** software update is
**confirmed**. It happens `k` blocks after the update proposal is
approved. When proposal is confirmed, you will see such message:

> Proposal 4c827d6fe is confirmed

There is no rush to restart nodes as soon as proposal is confirmed (it
only delays block version adoption), but don't do it too early. Don't
forget to update `lastKnownBlockVersion` in configuration (be careful
to do it for appropriate configuration key used by core nodes) before
restart. After that you should restart core nodes with newer
configuration. When 90% of nodes (by stake) create blocks with never
`BlockVersion` it will be adopted. When it's adopted, there should be
such message in logs:

> BlockVersion is adopted: «version»; winning block was <hash>
