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
    , wsuUpdateState     :: !UpdateState
    -- ^ The state of this update from users point of view.
    , wsuSha256Checksum  :: !Sha256Checksum
    -- ^ The checksum expected for this installer. It will probably
    -- need to be passed as part of the `UpdateProposal` (maybe in an attribute?),
    -- or we could reuse the hash which is part of the name, if possible.
    } deriving (Eq, Show, Generic, Typeable)
```

where `UpdateSeverity` is defined as an enumeration indicating how compulsory this update is:

```haskell
data UpdateSeverity = Mandatory
                    | Optional
```

For hard forks updates (or security-critical issues) we might want to set the severity
to `Mandatory`, for soft forks and other non-critical updates we can label an update as `Optional`.

-   `Optional` updates can be rejected by the user, as well as applied to will at a later stage;
-   `Mandatory` updates will need to be applied immediately and won't allow the user to proceed
    in using Daedalus.

On the other hand, `wsuUpdateState` records the users' decision towards the update:

```haskell
data UpdateState = Applied
                 -- ^ The update has been fully applied.
                 | Downloaded !Sha256Checksum !FilePath
                 -- ^ The update has been downloaded but not applied yet. A Checksum
                 -- can be stored to verify the integrity of the download.
                 | Skipped
                 -- ^ The update has been skipped by the user.
                 | Proposed
                 -- ^ The update has been just proposed and is waiting for users' feedback.
```

Note how we could add an intermediate `Downloading` state, but this would complicate state-handling
in case a user hard-quits Daedalus whilst the upgrade is being downloaded. In this case, we would
need to catch the exception and update the state back to `Proposed`, but that case be more error
prone than not updating the state in the first place. Having the state back to `Proposed` is
important so that the wallet is left in a clean state and upon re-launched Daedalus the update can
be downloaded again with no harm being done.

## Determining the severity<a id="sec-5-2" name="sec-5-2"></a>

For the proposed schema to succeed, we would need a function like the following:

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

At startup Daedalus would make a call to `GET /api/v1/update` to fetch the last available update. If
no update is available (for example if this is the first time we are configuring this Daedalus instance),
404 will be returned.

In case of a 404 answer or a `WalletSoftwareUpdate` which has the `wsuUpdateState` field set to
`Applied`, Daedalus would consider itself updated. In case the fetched `WalletSoftwareUpdate`
had `Mandatory` severity (but is not yet applied), Daedalus would freeze the UI and prevent the
user from doing anything but update. At this stage, closing Daedalus won't have any effect,
as upon the next startup the RESTful call would be performed again.

**N.B** A very fair point could be made that we are giving clients too many responsibility and we are offloading
too much logic to them. If that's the case, we can easily create a new endpoint such as `GET /api/v1/update/severity`
which would reply with a JSON `{ "updateSeverity": "mandatory" }` in case an update is available, is not fully
applied and is marked as `Mandatory` (names and payloads might vary to make them more fitting).

-   (1.1) As a Daedalus user, I want to be able to download an update.

After having grabbed a `WalletSoftwareUpdate` via a successful `GET /api/v1/update` Daedalus is now in the position to
react to user's input. In this story, the user would choose to continue with the update process. To do so, the update
would need to be downloaded. More specifically, the user would call `POST /api/v1/update/download` which
would begin the download process. No state change would be necessary in the backend yet. Story `1.2.` assumes
that this endpoint has been called and it details its behaviour.

-   (1.2) As a Daedalus user, I want to be able to see the download progress for an update.

To accomplish this, one possible idea would be to use [chunked transfer encoding](https://en.wikipedia.org/wiki/Chunked_transfer_encoding)
to send Daedalus information about the progress of the update. For this to work, we would need to store
the total size of the binary we need to download somewhere, so that Daedalus could compute a progress
percentage by doing simple math. (**Note**: it has to be investigated how much painful it is on the Servant side).

Once the process is complete, the backend would update the `UpdateState` of a `WalletSoftwareUpdate`
to remember the fact this update was already downloaded, also recording the `FilePath` where this binary lives and
the `Sha256Checksum` to be used later on in case an update is downloaded but not applied. This will allow the update
process to skip a new download, whilst ensuring integrity of the data.

-   (2) As a Daedalus user, I want to be able to dismiss an update, unless this is a critical one;

In this scenario, Daedalus would prompt the user that a new update is available (and is non critical) but
the user would dismiss the update, for example closing a modal in Daedalus. As a consequence, Daedalus
would call `PUT /api/v1/update/skip` to skip the update. The backend would register this information by
updating the `UpdateState` to `Skipped`. This endpoint is idempotent, and can be called more than once.

-   (2.1) As a Daedalus user, I want to be sure that once an update is dismissed, it won't be silently applied
    next time I restart the program;

This will be ensured by design, as it's only when the user decides to apply the update that the installer is
moved from the download directory into the folder the Cardano Launcher monitors and uses for the update process.

-   (3) As a Daedalus user, I want to be able to check for new updates by clicking a button in Daedalus.

Under this scenario, the user must be able to fetch updates which have been dismissed (or downloaded) but not
applied yet. This is now easy to accomplish as querying `GET /api/v1/update` would return either an applied update,
(in which case "No Updates Available" should be shown), a 404 (which entails the former) or an update which
has been downloaded already, or skipped. Either way, the user will be in the position to move
forward in the update process.

-   (4) As a Daedalus user, I want to be able to install an update immediately.

Once an update has been downloaded, users can choose whether or not install it. In case Daedalus is abruptly closed (but
the download completed) it will always be possible to recover the installation as per User Story 3.
Applying an upgrade entails calling `POST /api/v1/update` which conceptually "creates an update" and performs
a side effect (thus the use of post and the lack of extra names/resources in the URL).

The wallet backend would react to this request by simply moving the downloaded binary into the directory where
the Cardano Launcher expects it, and finally updating the state of the `WalletSoftwareUpdate` to be marked as
`Applied`. These two operation do **not** need to be atomic: closing the Wallet whilst the copy is happening or
prior to the change of state won't cause any harm, as Daedalus won't mark the update as happened and the process
can resume via User Story 3.

-   (4.1) As a Daedalus user, I want to be able to install an update after a manual restart.

This trivially follows from (4), with the difference that after downloading & moving the binary, we won't
restart Daedalus.

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

- When `GET /api/v1/update` is called:
   - If the _current_ update is `Nothing` (no update was ever seen by this node), and a _next_ update
     is available, the replace _current_ with _next_.

   - If the _current_ update is in the `Skipped` or `Applied` state, and a _next_ update is available,
     then replace _current_ with _next_. *In-transit* `POST/PUT` http-requests won't
     cause state inconsistencies by the virtue of the fact the input
     `WalletSoftwareUpdate` won't match anymore.

   - If the _current_ update is in the `Proposed` state, and a _next_ update is available,
     then replace _current_ with _next_. *In-transit* `POST/PUT` http-requests won't
     cause state inconsistencies by the virtue of the fact the input
     `WalletSoftwareUpdate` won't match anymore.

   - If the _current_ update is in the `Downloaded` state, and a _next_ update is available,
     chances are we want to return the current (downloaded but not applied update) update so
     that Daedalus can warn the user it has a downloaded (but not applied) update.

- When `POST /api/v1/update` is called:
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
      title: V0 Update API
      description: This is a subset of the API for Cardano SL wallet.
    host: 'localhost:8090'
    # Note how we still keep the singular name for the resource, as
    # described here: http://www.restapitutorial.com/media/RESTful_Best_Practices-v1_1.pdf
    # in the section "Pluralization (singleton resources)".
    paths:
      /api/v1/update:
        get:
          description: >
            Returns the latest available update.
            It's Daedalus' responsibility to check whether the returned update is
            mandatory (at startup) and stop the user by using the Wallet. Note: this
            might not be enough in case of clients different from Daedalus, as this is
            an invariant which is not imposed anywhere if not on paper.
          produces:
            - application/json;charset=utf-8
          responses:
            '200':
              schema:
                $ref: '#/definitions/WalletSoftwareUpdate'
              description: 'The latest available update, which can be available, downloaded, skipped or applied already.'
            '404':
              description: 'There are no updates available (this node never saw one)'
        post:
          description: >
            Applies this update. It takes as input in the body
            a JSON representing the `WalletSoftwareUpdate` to perform.
            In case the update has been already performed, the request
            is simply ignored.
          parameters:
            - name: body
              in: body
              required: true
              schema:
                $ref: '#/definitions/WalletSoftwareUpdate'
          consumes:
            - application/json;charset=utf-8
          produces:
            - application/json;charset=utf-8
          responses:
            '204':
              description: 'The update has been applied.'
            '422':
              description: 'Invalid update provided.'
      /api/v1/update/download:
        post:
          description: >
           Download the requested update. If the input `WalletSoftwareUpdate`
           is not the current update anymore, an error is returned.
           Streams the progress as a chunked HTTP response.
          parameters:
            - name: body
              in: body
              required: true
              schema:
                $ref: '#/definitions/WalletSoftwareUpdate'
          consumes:
            - application/json;charset=utf-8
          produces:
            - application/octet-stream
          responses:
            '204':
              description: 'The download has started.'
            '422':
              description: 'Invalid update provided.'
      /api/v1/update/skip:
        put:
          description: >
            Skip the update. If the input `WalletSoftwareUpdate`
            is not the current update anymore, the request is simply
            ignored.
          produces:
            - application/json;charset=utf-8
          responses:
            '204':
              description: 'The update has been skipped.'
    definitions:
      CCoin:
        required:
          - getCCoin
        properties:
          getCCoin:
            type: string
        type: object
      WalletSoftwareUpdate:
        required:
          - wsuSoftwareVersion
          - wsuBlockVesion
          - wsuScriptVersion
          - wsuImplicit
          - wsuVotesFor
          - wsuVotesAgainst
          - wsuPositiveStake
          - wsuNegativeStake
          - wsuSeverity
          - wsuUpdateState
          - wsuSha256Checksum
        properties:
          wsuSoftwareVersion:
            $ref: '#/definitions/SoftwareVersion'
          wsuBlockVesion:
            $ref: '#/definitions/BlockVersion'
          wsuScriptVersion:
            maximum: 65535
            minimum: 0
            type: integer
          wsuImplicit:
            type: boolean
          wsuVotesFor:
            maximum: 9223372036854776000
            minimum: -9223372036854776000
            type: integer
          wsuVotesAgainst:
            maximum: 9223372036854776000
            minimum: -9223372036854776000
            type: integer
          wsuPositiveStake:
            $ref: '#/definitions/CCoin'
          wsuNegativeStake:
            $ref: '#/definitions/CCoin'
          wsuSeverity:
            $ref: '#/definitions/UpdateSeverity'
          wsuUpdateState:
            $ref: '#/definitions/UpdateState'
          wsuSha256Checksum:
            description: "A checksum for data integrity."
            type: string
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
      UpdateSeverity:
        type: string
        enum: [mandatory, optional]
      UpdateState:
      # Just a stub for now.
        type: object
