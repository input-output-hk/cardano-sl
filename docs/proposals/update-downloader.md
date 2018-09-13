# Update downloader design overview

This document describes the problem of downloading and installing
software updates in cardano-sl and suggests possible solution.

## Overview of the problem

In cardano-sl we have Update System (sometimes abbreviated to US)
which allows stakeholders to propose software updates and vote for
them. Update proposals and votes are stored in the blockchain and are publicly visible by all nodes. An update proposal can become
**confirmed** if it gets enough votes (which
are stored in blocks) and there are enough blocks to make those blocks
irrevertible. We are primarily interested when an
update proposal becomes **confirmed**, because it means that the software
for which that update is proposed should be updated.

An `UpdateProposal` contains, among other things, `HashMap SystemTag
UpdateData`. `SystemTag` is used to distinguish operating systems,
currently `win64` and `macos64`. `UpdateData` contains various hashes,
but we are primarily interested in only one of them: hash of the
installer which can be used to install the updated software.

Let's assume that our software is called `csl-daedalus` for simplicty,
because currently only this software uses this feature (automatic
downloading of updates). The following requirements apply:
1. If after processing a block an update proposal for `csl-daedalus`
   becomes **confirmed** and it has an update for our `SystemTag`:
   * We should:
      1. Download the installer corresponding to this update proposal and
         save it on disk so that it can be launched later. The URL to
         download the installer is formed from the predefined template and
         the hash of the installer.
      2. Verify the hash of the downloaded installer.
      3. Notify the frontend about the update (if previous steps are successful).

2. If we process a lot of blocks and multiple update proposals get
   confirmed, only the last installer should be downloaded.
3. It should be possible to specify multiple servers and if one fails
   (or hangs), try another one.
4. The update download procedure must not hang indefinitely. If the
   download takes too much time, it should be restarted after the delay
   or another server should be used (see above).
5. If the installer is downloaded successfully, the installer should be launched automatically
   the next time the user opens Daedalus and an update should be performed.

There are two options for (1):

* 1.1. If we have already downloaded an update we can ignore all new
updates until the downloaded one is installed (which happens on restart).
* 1.2. [Optional] Ideally we should not ignore new updates, but
instead utilize the newest update if possible. Though this approach is questionable.

## Current approach

We have a worker defined in the `Pos.Update.Worker` module
(`checkForUpdate`) which checks whether there is a new **confirmed**
update for our application and tries to download it if it exists. This
check happens once per slot and only when we are not in recovery
mode.
* First we obtain all confirmed proposals for our application from
  GState. We only consider updates for our `SystemTag`.
* We pick the proposal with the greatest numeric software version and
  try to download it.
* Download procedure is guarded by `MVar ()` which allows only one
  thread to download an update.
* We store information about updates that have already been downloaded in `MVar`
  (`ucDownloadedUpdate`). If we have already downloaded one, we do not
  download anything. We assume that on startup there is no update to download.
* We also check whether the update we are about to download has been
  installed already. This information is stored in Misc DB.
* There is a predefined path to where an update should be downloaded. If
  there is a file with this path already defined, we do not download the update.
* If we reach this step, we try to actually download the update. We
  know servers from which we can download the update and try them
  one-by-one until we download the installer. When we download it, we
  also check that its hash is the expected one.
     * Note: Server paths are provided to the nodes via command line.
* Finally, information about downloaded updates is stored in `MVar`
  (`ucDownloadedUpdate`) and Daedalus is notified.

Maintenance of already downloaded updates (launch
and removal functions) is handled by
`cardano-launcher`.
* Launcher knows the path where the installer should be stored.
* Before launching the node launcher checks whether the file with the
  aforementioned path exists.
* If the file exists, the installer will be launched. The way to
  launch the installer is not described in this document.
* If the installer exits successfully, Misc DB
  is updated to reflect that the installer has been successfully launched with hash
  `h` and removed.

The following requirements are satisfied by this approach:
1. The first one is satisfied if we consider option 1.1.
2. The second one is satisfied because we do not use `checkForUpdate`
   worker in recovery mode and because there cannot be more than one
   confirmed proposal for our application in `k` blocks.
3. The third one is satisfied, except that we do not have timeouts (see
   below).
4. The fourth one is not satisfied. We do not have timeouts and we once
   observed that update download never finished (after several hours
   or maybe even days). See CSL-1786.
5. The fifth one is satisfied completely.

So the only missing strict requirement is the fourth one (lack of
timeouts). Also we miss optional requirement 1.2.

Apart from that, there are several drawbacks in the implementation:
* We may download an update, save it on disk and then an exception may
  be thrown before we fill `MVar` (which is used to notify
  middleware). When we try to download an update again, we will not
  succeed, because we think that we have already downloaded one. So
  we will never notify Daedalus until restart.
* It can happen that a user's computer is turned off during the process
  of writing the update on disk (or the cardano-node is stopped). In such scenarios, an update will stop working for the user (we will
  see there is an update, but will always fail to apply it). It is very unlikely to happen in practice, because
  writing the update on disk is simply calling `BSL.writeFile` with
  tens of megabytes and it should be quite fast. However, it is still
  possible and it would be good to completely prevent it from
  happening. Also it can happen that the user accidentally modifies
  the update file (also unlikely, but possible).

In terms of general design, these two drawbacks stem from two
more general problems:
1. We basically assume that writing the installer on disk and updating
   `MVar` is a single action which happens atomically. But we do not
   have any guarantees about correspondence between the file on disk
   and the contents of `MVar`.
2. When we have an installer on disk, we do not know which installer
   it is supposed to be. We just assume that it is the one we want to
   install.

## Suggested improvements

To address the drawbacks of the current approach, I propose to do the
following changes:
1. Change the already downloaded check for the installer
   in the `downloadUpdateDo` function. If the file does not exist, proceed with the download. If the file exists, there are two
   cases:
   * It contains the installer we want to download (it can be checked
     by hashing the file). In this case we do not download
     anything. We only need to ensure that `ucDownloadedUpdate` contains the proper value.
   * The hash of the file is not the one we want to download. In this
     case we delete this file immediately (because we do not need it
     anyway) and download the update we want.
2. [Optional] In order to solve the second drawback of the existing
   approach, we can store the hash of the installer we assume to be
   stored on disk in Misc DB. In this case we always check
   the installer's integrity. When we write the installer on disk, we
   first update the hash in Misc DB and only then do
   `BSL.writeFile`. When we want to check the launcher's integrity, we
   compare its hash with the one in Misc DB. If it differs, we delete the
   installer and assume it does not exist.
3. Impose timeout on downloading the function (`downloadUri`). If a timeout
  occurs, try another server. If all servers fail, try again later.
4. Do not even start `checkForUpdateWorker` if the list of servers is
  empty (which is the case for core nodes).

## Open questions

1. Do we want to satisfy requirement 1.2? Probably not, 1.1 is enough.
2. Do we want to make improvement (2)? It's optional, because the
   drawback (2) is unlikely to happen in practice, but it looks easy
   to do, so I would do it.
