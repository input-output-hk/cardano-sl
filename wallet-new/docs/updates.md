<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Introduction</a></li>
<li><a href="#sec-2">2. Abstract</a></li>
<li><a href="#sec-3">3. Motivation</a></li>
<li><a href="#sec-4">4. Current design</a>
<ul>
<li><a href="#sec-4-1">4.1. V0 Swagger spec</a></li>
</ul>
</li>
<li><a href="#sec-5">5. Specification</a>
<ul>
<li><a href="#sec-5-1">5.1. New update system</a></li>
<li><a href="#sec-5-2">5.2. Determining the severity</a></li>
<li><a href="#sec-5-3">5.3. User stories</a></li>
<li><a href="#sec-5-4">5.4. Installers retention</a></li>
<li><a href="#sec-5-5">5.5. Storage &amp; data races</a></li>
<li><a href="#sec-5-6">5.6. V1 Swagger spec</a></li>
</ul>
</li>
</ul>
</div>
</div>

# Introduction<a id="sec-1" name="sec-1"></a>

# Abstract<a id="sec-2" name="sec-2"></a>

The purpose of this document is to review how updates are fetched and delivered to
the end user in Daedalus.

# Motivation<a id="sec-3" name="sec-3"></a>

When a new update is released to mainnet, Daedalus is able to inform the users
about such update and guide them in the upgrade process. However, the current
system suffers from a number of limitations, both in terms of functionality and
ergonomics:

-   Updates are silently downloaded from the network, unbeknownst to the users.
    Users do not have visibility on this process up until the very last moment,
    when an update prompt is presented to them.

-   Although uploads can be postponed, they will be applied anyway when Daedalus
    restarts, betraying users expectations.

-   Users cannot dynamically query for updates, or really postpone them,
    meaning that they are not in control of the update process.

# Current design<a id="sec-4" name="sec-4"></a>

This summarises (roughly) how the current update process works.

-   The update is voted and it enters the blockchain.

-   Nodes broadcasts this update between each other.

-   An edgenode monitors the state of the blockchain, and when an update
    is found, it's signalled to the wallet backend by populating the
    [ucDownloadedUpdate](https://github.com/input-output-hk/cardano-sl/blob/master/update/Pos/Update/Download.hs#L130) variable.

-   The wallet constantly polls this variable and when a new update is
    found is stored in a list of updates. The list sorts the updates by
    oldest first, so adding a new update is a `O(n)` operation where
    `n` is the number of updates (which is small anyway).

-   Whilst this DB operation is happening, we also download
    from the network an archive (without extension and whose name
    is just an hash) which contains the installers to perform the
    update. This download happens in background without the user
    noticing it or without the user being warned.

-   When we want to apply an update, we query `GET /api/update`
    and this performs a `O(1)` `head` lookup in the list of updates to
    grab the first (and te oldest), because updates are incremental
    and need to be performed in a sequence. If the first (and the oldest)
    of the updates has `version <= current_version_of_the_code`,
    then it's dismissed and the process repeats for next update.
    If there are no updates left, the endpoint reports this information accordingly.

-   If the user applies the update, `POST /api/update/apply` is called.
    We then apply the update using the downloaded binary and we
    remove the \`Update\` object from the list and the process repeats.

-   If the user dismisses the update, `POST /api/update/postpone` is
    called, which removes the \`Update\` object from the list **but**
    due to the fact the update was already download, if the user
    restarts Daedalus the update will be applied **anyway**.

## V0 Swagger spec<a id="sec-4-1" name="sec-4-1"></a>

For completeness, this is the part of the Swagger spec implementing
the (**current**) update system:

    swagger: '2.0'
    info:
      version: 1.0.2
      title: V0 Update API
      description: This is a subset of the API for Cardano SL wallet.
    host: 'localhost:8090'
    paths:
      /api/update:
        get:
          description: Get information about the next update.
          produces:
            - application/json;charset=utf-8
          responses:
            '200':
              schema:
                $ref: '#/definitions/CUpdateInfo'
              description: ''
      /api/update/postpone:
        post:
          description: Postpone last update.
          produces:
            - application/json;charset=utf-8
          responses:
            '200':
              description: ''
      /api/update/apply:
        post:
          description: Apply last update.
          produces:
            - application/json;charset=utf-8
          responses:
            '200':
              description: ''
    definitions:
      CCoin:
        required:
          - getCCoin
        properties:
          getCCoin:
            type: string
        type: object
      CUpdateInfo:
        required:
          - cuiSoftwareVersion
          - cuiBlockVesion
          - cuiScriptVersion
          - cuiImplicit
          - cuiVotesFor
          - cuiVotesAgainst
          - cuiPositiveStake
          - cuiNegativeStake
        properties:
          cuiSoftwareVersion:
            $ref: '#/definitions/SoftwareVersion'
          cuiBlockVesion:
            $ref: '#/definitions/BlockVersion'
          cuiScriptVersion:
            maximum: 65535
            minimum: 0
            type: integer
          cuiImplicit:
            type: boolean
          cuiVotesFor:
            maximum: 9223372036854776000
            minimum: -9223372036854776000
            type: integer
          cuiVotesAgainst:
            maximum: 9223372036854776000
            minimum: -9223372036854776000
            type: integer
          cuiPositiveStake:
            $ref: '#/definitions/CCoin'
          cuiNegativeStake:
            $ref: '#/definitions/CCoin'
        type: object
      SoftwareVersion:
        required:
          - svAppName
          - svNumber
        properties:
          svAppName:
            $ref: '#/definitions/ApplicationName'
          svNumber:
            maximum: 4294967295
            minimum: 0
            type: integer
        type: object
      ApplicationName:
        required:
          - getApplicationName
        properties:
          getApplicationName:
            type: string
        type: object
      BlockVersion:
        required:
          - bvMajor
          - bvMinor
          - bvAlt
        properties:
          bvMajor:
            maximum: 65535
            minimum: 0
            type: integer
          bvMinor:
            maximum: 65535
            minimum: 0
            type: integer
          bvAlt:
            maximum: 255
            minimum: 0
            type: integer
        type: object

# Specification<a id="sec-5" name="sec-5"></a>

## New update system<a id="sec-5-1" name="sec-5-1"></a>

**Summary**

`We propose changes to the wallet backend data model and the way the installers interact with the cardano launcher.`

To begin with, we will stop downloading an update in the background, but rather we will give the user full control.

**When requested, an update will be downloaded in a transient directory and when the user is ready to apply it,
the downloaded installer will be moved to the folder the Cardano launcher currently watches, so that the
installation process can be reused without changes to the underlying infrastructure.**

We propose adding some extra fields to what used to be called `CUpdateInfo` (which here we rename for
clarity):

```haskell
data WalletSoftwareUpdate = WalletSoftwareUpdate
    { wsuSoftwareVersion :: !SoftwareVersion
    , wsuBlockVesion     :: !BlockVersion
    , wsuScriptVersion   :: !ScriptVersion
    , wsuImplicit        :: !Bool
    , wsuVotesFor        :: !Int
    , wsuVotesAgainst    :: !Int
    , wsuPositiveStake   :: !CCoin
    , wsuNegativeStake   :: !CCoin
    , wsuSeverity        :: !UpdateSeverity
    -- ^ The severity of the update (i.e. optional or mandatory).
    , wsuState     :: !UpdateState
    -- ^ The state of this update from users point of view.
    , wsuDownloadProgress  :: !Word8
    -- ^ A progress in percentage (from 0 to 100).
    -- N.B. In the real implementation this can be refined to a better type.
    , wsuId  :: !Hash
    -- ^ The update id, which is also the checksum expected for this installer,
    -- and correspond to the @Blake2b256@ hash of the installer.
    } deriving (Eq, Show, Generic, Typeable)
```

where `UpdateSeverity` is defined as an enumeration indicating how compulsory this update is:

```haskell
data UpdateSeverity = Mandatory
                    | Optional
```

For security-critical issues we might want to set the severity
to `Mandatory`, for other non-critical updates we can label an update as `Optional`, which is
the default anyway.

-   `Optional` updates can be rejected by the user, as well as applied to will at a later stage;
-   `Mandatory` updates will need to be applied immediately and won't allow the user to proceed
    in using Daedalus.

On the other hand, `wsuState` records the users' decision towards the update:

```haskell
data UpdateState = Applied
                 -- ^ The update has been fully applied.
                 | Downloading
                 -- ^ The update is downloading.
                 | Downloaded !Hash !FilePath
                 -- ^ The update has been downloaded but not applied yet. A Checksum
                 -- can be stored to verify the integrity of the download.
                 | Skipped
                 -- ^ The update has been skipped by the user.
                 | Available
                 -- ^ The update is available to the user.
```

Note how we could add an intermediate `Downloading` state, but this would complicate state-handling
in case a user hard-quits Daedalus whilst the upgrade is being downloaded. In this case, we would
need to catch the exception and update the state back to `Available`, but that case be more error
prone than not updating the state in the first place. Having the state back to `Available` is
important so that the wallet is left in a clean state and upon re-launched Daedalus the update can
be downloaded again with no harm being done.

## Determining the severity<a id="sec-5-2" name="sec-5-2"></a>

For the proposed schema to succeed, we would need (eventually) a function like the following:

```haskell
updateSeverity :: UpdateProposal -> UpdateSeverity
```

Such function would gather evidence from an `UpdateProposal` that this is a non-negotiable update.

Note how we don't need to commit just yet whether or not we will treat hard-forks as `Mandatory`, as
there are good arguments for people wanting to stick with an old version of the chain.

## User stories<a id="sec-5-3" name="sec-5-3"></a>

In order to check the validity of the proposed plan, let's try to run trough some of the common
user stories for the update system.

-   (1) As a Daedalus user, I want the wallet to prompt me if there is a new update available.

At startup Daedalus would make a call to `GET /api/v1/updates` to fetch the updates known to this
node, which are no more than two (the current one and the next available). If no update is
available (for example if this is the first time we are configuring this Daedalus instance),
an empty JSON array will be returned.

In case of an empty answer or a `WalletSoftwareUpdate` which has the `wsuState` field set to
`Applied`, Daedalus would consider itself updated. In case the fetched `WalletSoftwareUpdate`
had `Mandatory` severity (but is not yet applied), Daedalus would freeze the UI and prevent the
user from doing anything but update. At this stage, closing Daedalus won't have any effect,
as upon the next startup the RESTful call would be performed again.

**N.B** A very fair point could be made that we are giving clients too many responsibility and we are offloading
too much logic to them. If that's the case, we can easily create a new endpoint such as `GET /api/v1/updates/severity`
which would reply with a JSON `{ "updateSeverity": "mandatory" }` in case an update is available, is not fully
applied and is marked as `Mandatory` (names and payloads might vary to make them more fitting).

-   (1.1) As a Daedalus user, I want to be able to download an update.

After having grabbed a `WalletSoftwareUpdate` via a successful `GET /api/v1/updates` Daedalus is now in the position to
react to user's input. In this story, the user would choose to continue with the update process. To do so, the update
would need to be downloaded. More specifically, the user would call `POST /api/v1/updates/{updateId}` passing the state
he want the update to transition to, in this case `Downloading`, which would begin the download process.

-   (1.2) As a Daedalus user, I want to be able to see the download progress for an update.

To accomplish this, the user would call `GET /api/v1/updates/{updateId}` periodically and grab the value of
`wsuDownloadProgress`. For this to work, we would need to store the total size of the binary we need to download somewhere,
as well of how much we download up until now, in order to compute a progress percentage by doing simple math.

Once the process is complete, the backend would update the `UpdateState` of a `WalletSoftwareUpdate`
to remember the fact this update was already downloaded, also recording the `FilePath` where this binary lives and
the `Hash` to be used later on in case an update is downloaded but not applied. This will allow the update
process to skip a new download, whilst ensuring integrity of the data.

-   (2) As a Daedalus user, I want to be able to dismiss an update, unless this is a critical one;

In this scenario, Daedalus would prompt the user that a new update is available (and is non critical) but
the user would dismiss the update, for example closing a modal in Daedalus. As a consequence, Daedalus
would call `POST /api/v1/updates/{updateId}` to skip the update by passing `{"state": "skipped"}` in
the JSON body. The backend would register this information by updating the `UpdateState` to `Skipped`.

-   (2.1) As a Daedalus user, I want to be sure that once an update is dismissed, it won't be silently applied
    next time I restart the program;

This will be ensured by design, as it's only when the user decides to apply the update that the installer is
moved from the download directory into the folder the Cardano Launcher monitors and uses for the update process.

-   (3) As a Daedalus user, I want to be able to check for new updates by clicking a button in Daedalus.

Under this scenario, the user must be able to fetch updates which have been dismissed (or downloaded) but not
applied yet. This is easy to accomplish by querying `GET /api/v1/updates` and filtering by `state`. For example
`GET /api/v1/updates?state=skipped` would returned all the skipped updates.

-   (4) As a Daedalus user, I want to be able to install an update immediately.

Once an update has been downloaded, users can choose whether or not install it. In case Daedalus is abruptly closed (but
the download completed) it will always be possible to recover the installation as per User Story 3.
Applying an upgrade entails calling `POST /api/v1/updates/{updatesId}` with `{"state": "applied"}` as JSON
body, which conceptually "creates an update" and performs a side effect (thus the use of post and the
lack of extra names/resources in the URL).

The wallet backend would react to this request by simply moving the downloaded binary into the directory where
the Cardano Launcher expects it, and finally updating the state of the `WalletSoftwareUpdate` to be marked as
`Applied`. These two operation do **not** need to be atomic: closing the Wallet whilst the copy is happening or
prior to the change of state won't cause any harm, as Daedalus won't mark the update as happened and the process
can resume via User Story 3.

-   (4.1) As a Daedalus user, I want to be able to install an update after a manual restart.

This trivially follows from (4), with the difference that after downloading & moving the binary, we won't
restart Daedalus.

-   (5) As a Daedalus user, I want to be able to issue an update proposal.

To do so, we would call `POST /api/v1/update-proposals`, passing a `UpdateProposal` as input in the JSON
body. We have chosen to split update proposals from the `updates` API, because conceptually they represents
two different resources. One is an already confirmed update proposal (`WalletSoftwareUpdate`) which can be
installed safely, whereas the latter is something which needs to undergo a democratic voting process.
After a successful request, the system returns a `ProposedUpdate`, which is a type which bundles an `UpId`
and an `UpdateProposal` together.

-   (6) As a Daedalus user, I want to be able to vote an update proposal.

In order to vote for an update proposal, users have two choices:

- If they just proposed a new update, they can grab its `UpId` and use it to vote;
- Users can vote an arbitrary non-expired `UpdateProposal` in the voting center. In this case Daedalus
  would ask for the full list of `UpdateProposal`(s) by querying `GET /api/v1/update-proposals`.

Either way, the users are expected to pass as input the *stake key*, because the stake they have delegated
must be taken into account for voting.

## Installers retention<a id="sec-5-4" name="sec-5-4"></a>

Once the installer has been moved into the folder the Cardano Launcher expects it, my understanding (adinapoli)
is that the Launcher is also responsible for deleting the installer from disk, so this part have been left out
by the present proposal.

## Storage & data races<a id="sec-5-5" name="sec-5-5"></a>

At the moment of writing (2017-11-08), the updates are stored in the `wallet-db` as a list:

```haskell
data WalletStorage = WalletStorage
    { _wsWalletInfos     :: !(HashMap (CId Wal) WalletInfo)
    , _wsAccountInfos    :: !(HashMap AccountId AccountInfo)
    , _wsProfile         :: !CProfile
    , _wsReadyUpdates    :: [CUpdateInfo]
    , _wsTxHistory       :: !(HashMap (CId Wal) (HashMap CTxId CTxMeta))
    , _wsHistoryCache    :: !(HashMap (CId Wal) (Map TxId TxHistoryEntry))
    , _wsUtxo            :: !Utxo
    -- @_wsBalances@ depends on @_wsUtxo@,
    -- it's forbidden to update @_wsBalances@ without @_wsUtxo@
    , _wsBalances        :: !WalletBalances
    , _wsUsedAddresses   :: !CustomAddresses
    , _wsChangeAddresses :: !CustomAddresses
    } deriving (Eq)
```

It is my understanding (adinapoli) that this is necessary because more than one update can be found on the blockchain
and we need to ensure a linear sequence of patches. I also believe, though, than this restriction was an historical
vestige of the fact we were using binary patching (bsdiff) on the executables, but this is no longer true
as we ship full installers on S3.

**If my reasoning holds, we should be able to maintain in memory only two pieces of state: the current update (i.e.
the one for which the user started an upgrade process) and the latest update (as in the most recent) we got from
the blockchain**. Therefore, the updated `WalletStorage` would look like:

```haskell
data WalletStorage = WalletStorage
    { _wsWalletInfos     :: !(HashMap (CId Wal) WalletInfo)
    , _wsAccountInfos    :: !(HashMap AccountId AccountInfo)
    , _wsProfile         :: !CProfile
    , _wsCurrentUpdate   :: !(Maybe WalletSoftwareUpdate)
    , _wsNextUpdate      :: !(Maybe WalletSoftwareUpdate)
    , _wsTxHistory       :: !(HashMap (CId Wal) (HashMap CTxId CTxMeta))
    , _wsHistoryCache    :: !(HashMap (CId Wal) (Map TxId TxHistoryEntry))
    , _wsUtxo            :: !Utxo
    , _wsBalances        :: !WalletBalances
    , _wsUsedAddresses   :: !CustomAddresses
    , _wsChangeAddresses :: !CustomAddresses
    } deriving (Eq)
```

Separating these two information apart ensures that new updates can be retrieved from the network
whilst not overriding the *current* update. As a practical example, consider the following scenario:

    |  user (Daedalus)                                wallet backend                          update notifier
    |   |                                                |                                                |
    |   |                   GET /api/v1/update           |                                                |
    |   +------------------------------------------------>                                                |
    |   |                                                |                                                |
    |   |       returns optional update                  |                                                |
    |   <------------------------------------------------+                                                |
    |t  |                                                |    mandatory update arrives, data overwritten  |
    |i  |                                                |                        ------------------------+
    |m  |                                                <-----------------------/                        |
    |e  |                                                |                                                |
    |   |       PUT /api/v1/update/skip                  |                                                |
    |   +------------------------------------------------> the user just skipped a mandatory update!      |
    |   |                                                |                                                |
    |   |                                                |                                                |
    |   |                                                |                                                |
    |   |                                                |                                                |
    |   |                                                |                                                |
    |   |                                                |                                                |
    |   |                                                |                                                |
    |   |                                                |                                                |
    |   |                                                |                                                |
    |   |                                                |                                                |
    |   |                                                |                                                |
    |   |                                                |                                                |
    V   |                                                |                                                |

If we kept only one single field to store this data, such field might be sensible to data races and *ghost updates*,
where we could accidentally override data in a way which would cause our internal invariants to be violated.

The way updates will transition from the *next* to the *current* can be the following:

- When a new update arrives, store it as the _next_ update. If there is already an
  update designed as next, replace it.

- When `GET /api/v1/updates` is called:
   - If the _current_ update is `Nothing` (no update was ever seen by this node), and a _next_ update
     is available, the replace _current_ with _next_.

   - If the _current_ update is in the `Skipped` or `Applied` state, and a _next_ update is available,
     then replace _current_ with _next_. *In-transit* `POST/PUT` http-requests won't
     cause state inconsistencies by the virtue of the fact the input
     `WalletSoftwareUpdate` won't match anymore.

   - If the _current_ update is in the `Available` state, and a _next_ update is available,
     then replace _current_ with _next_. *In-transit* `POST/PUT` http-requests won't
     cause state inconsistencies by the virtue of the fact the input
     `WalletSoftwareUpdate` won't match anymore.

   - If the _current_ update is in the `Downloaded` state, and a _next_ update is available,
     chances are we want to return the current (downloaded but not applied update) update so
     that Daedalus can warn the user it has a downloaded (but not applied) update.

- When `POST /api/v1/updates` is called:
    - If the input `WalletSoftwareUpdate` doesn't match the _current_ update, ignore the request
      and return a [422](https://httpstatuses.com/422) error code together with a proper
      domain-specific error.

    - If the input `WalletSoftwareUpdate` does match the _current_ update, carry on as per
      user story (4) and (4.1), then set the _current_ status to `Applied`.

        - If a _next_ update is available, replace _current_ with _next_.

## V1 Swagger spec<a id="sec-5-6" name="sec-5-6"></a>

A mockup for the new update API might look like this (some HTTP errors and uninteresting
types omitted for brevity):

swagger: '2.0'
info:
  version: 1.0.2
  title: V1 Update API
  description: This is a subset of the API for Cardano SL wallet.
host: 'localhost:8090'
paths:
  /api/v1/updates:
    get:
      summary: "Get all the confirmed updates the system knows about."
      description: >
        Returns the **confirmed** updates known to this wallet, **ordered by age**.
        It's user's responsibility to check whether the next suitable update is
        mandatory (at startup) and stop the user by using the Wallet. Note: this
        might not be enough in case of clients different from Daedalus, as this is
        an invariant which is not imposed anywhere if not on paper.
        For more information about the various `UpdateProposal` states, check
        [this link](https://github.com/input-output-hk/cardano-sl/blob/master/docs/block-processing/us.md).
      produces:
        - application/json;charset=utf-8
      parameters:
        - name: state
          in: query
          type: string
          required: false
          enum: [applied, downloaded, available,skipped]
          description: "Filter by update state."
      responses:
        '200':
          schema:
            type: array
            items:
              $ref: '#/definitions/WalletSoftwareUpdate'
          description: 'The updates, which can be available, downloaded, skipped or applied already.'
  /api/v1/updates/{updateId}:
    get:
      summary: "Get a specific update given its Id."
      description: >
        Get the information about a specific update.
      parameters:
        - name: updateId
          required: true
          in: path
          type: string
          description: "The update ID."
      produces:
        - application/json;charset=utf-8
      responses:
        '200':
          description: "The requested update."
          schema:
            $ref: '#/definitions/WalletSoftwareUpdate'
    post:
      summary: "Apply, download or skip an update, given its Id."
      description: >
        Modify the status of an update, which allows the user to either
        skip, apply or download it. It's a post as downloading or applying an
        update are not idempotent.
      produces:
        - application/json;charset=utf-8
      parameters:
        - required: true
          name: updateId
          in: path
          description: "The update identifier (a Blake2b-256 hash)."
          type: string
        - required: true
          name: body
          in: body
          description: "The update status-change."
          schema:
            $ref: '#/definitions/UpdateStateChange'
      responses:
        '204':
          description: 'The update state has been changed.'
        '403':
          description: 'Invalid state transition attempted.'
  /api/v1/updates-proposals:
    get:
      summary: "Get all the available update proposals."
      description: >
        Get all the available update proposals this node knows about.
      responses:
        '200':
          description: 'The list of update proposals'
          schema:
            $ref: '#/definitions/ProposedUpdate'
    post:
      summary: "Propose a new update."
      description: >
        Proposes an update.
      parameters:
        - name: body
          in: body
          required: true
          schema:
            $ref: '#/definitions/UpdateProposal'
      consumes:
        - application/json;charset=utf-8
      produces:
        - application/json;charset=utf-8
      responses:
        '200':
          description: ''
          schema:
            $ref: '#/definitions/ProposedUpdate'
        '400':
            description: 'Not enough stake'
  /api/v1/updates-proposals/{proposalId}/votes:
    post:
      summary: "Vote on an update."
      description: >
        Vote with stake on the update proposal. Voting must
        abide to the [votes verification rules](https://github.com/input-output-hk/cardano-sl/blob/master/docs/block-processing/us.md#votes-verification)
        and issuing the same vote twice, or changing the vote more than one
        time is not allowed.
      parameters:
        - in: path
          name: proposalId
          type: string
          required: true
          description: Proposal ID to vote on
        - name: body
          in: body
          required: true
          schema:
            $ref: '#/definitions/UpdateProposalVote'
      consumes:
        - application/json;charset=utf-8
      responses:
        '200':
          description: 'Returns the proposal the user voted on.'
          schema:
            $ref: '#/definitions/ProposedUpdate'
        '403':
          description: 'Invalid stake key.'
        '404':
          description: 'Voted proposal is not active anymore.'
        '400':
          description: 'Not enough stake to vote on this proposal.'
definitions:
  Installer:
    description: "A Daedalus installer."
    type: object
    properties:
      platform:
        type: string
        example: "linux"
      url:
        type: string
        example: "https://s3.iohk.io/installers/daedalus.zip"
      checksum:
        description: "A Blake2b-256 hash."
        example: "A8ADD4BDDDFD93E4877D2746E62817B116364A1FA7BC148D95090BC7333B3673"
        type: string
  ProtocolUpgrade:
    type: object
    properties:
      maxHeaderSize:
        type: integer
        maximum: 65535
        minimum: 0
  Installers:
    type: array
    items:
      $ref: '#/definitions/Installer'
  UpdateProposal:
    type: object
    description:
      An update proposal.
    properties:
      blockVersion:
         $ref: '#/definitions/BlockVersion'
      protocolUpgrade:
         $ref: '#/definitions/ProtocolUpgrade'
      installers:
         $ref: '#/definitions/Installers'
      softwareVersion:
         $ref: '#/definitions/SoftwareVersion'
    required:
       - blockVersion
       - protocolUpgrade
       - installers
       - softwareVersion
  ProposedUpdate:
    type: object
    description: >
      Bundles an `UpdateId` and an `UpdateProposal`, as well as the
      original stakeholder which issued the update.
    properties:
      proposalId:
          type: string
          description: "The hash of the UpdateProposal (Blake2b_256)."
          example: "EFC9C8BABCFA93E49F1D2946E628173C7"
      issuerId:
          $ref: '#/definitions/StakeholderId'
      updateProposal:
        $ref: '#/definitions/UpdateProposal'
    required:
      - proposalId
  UpdateProposalVote:
    type: object
    properties:
      vote:
        $ref: '#/definitions/Vote'
    required:
      - vote
  WalletSoftwareUpdate:
    required:
      - softwareVersion
      - blockVesion
      - scriptVersion
      - implicit
      - votesFor
      - votesAgainst
      - positiveStake
      - negativeStake
      - severity
      - state
      - downloadProgress
      - id
    properties:
      softwareVersion:
        $ref: '#/definitions/SoftwareVersion'
      blockVesion:
        $ref: '#/definitions/BlockVersion'
      scriptVersion:
        maximum: 65535
        minimum: 0
        type: integer
      implicit:
        type: boolean
      votesFor:
        maximum: 9223372036854776000
        minimum: -9223372036854776000
        type: integer
      votesAgainst:
        maximum: 9223372036854776000
        minimum: -9223372036854776000
        type: integer
      positiveStake:
        type: integer
      negativeStake:
        type: integer
      severity:
        $ref: '#/definitions/UpdateSeverity'
      state:
        $ref: '#/definitions/UpdateState'
      downloadProgress:
        minimum: 0
        maximum: 100
        type: integer
      id:
        description: >
          A update identifier, which also correspond to the
          checksum for data integrity (it technically correspond to the
          hash of the installer).
        type: string
        example: "A8ADD4BDDDFD93E4877D2746E62817B116364A1FA7BC148D95090BC7333B3673"
    type: object
  SoftwareVersion:
    required:
      - applicationName
      - version
    properties:
      applicationName:
        type: string
        example: "cardano-sl"
      version:
        maximum: 4294967295
        minimum: 0
        type: integer
    type: object
  BlockVersion:
    required:
      - major
      - minor
      - alt
    properties:
      major:
        maximum: 65535
        minimum: 0
        type: integer
      minor:
        maximum: 65535
        minimum: 0
        type: integer
      alt:
        maximum: 255
        minimum: 0
        type: integer
    type: object
  UpdateSeverity:
    type: string
    enum: [mandatory, optional]
  UpdateState:
    type: string
    enum: [applied, downloaded, available, skipped]
  Vote:
    type: string
    enum: [approve, reject]
    description: "User's will towards an update proposal."
  UpdateStateChange:
    type: object
    properties:
      state:
        $ref: '#/definitions/UpdateState'
  StakeholderId:
    type: string
    example: "A8ADD4BDDDFD93E4877D2746E62817B116"
    description: >
      The Stakeholder Id, modeled as a Blake-224 hash of
      its public key.
