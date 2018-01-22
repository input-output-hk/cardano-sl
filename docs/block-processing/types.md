# Types that are parts of blocks

## Table of contents

  + [Prerequisites](#prerequisites)
  + [Genesis block](#genesis-block)
    * [Genesis block header](#genesis-block-header)
    * [Genesis block payload](#genesis-block-payload)
    * [Genesis block extra](#genesis-block-extra)
  + [Main block](#main-block)
    * [Main block header](#main-block-header)
    * [Main block signature](#main-block-signature)
    * [Main block payload+proof](#main-block-payload-proof)
      + [Transactions payload+proof](#transactions-payload-proof)
      + [SSC payload+proof](#ssc-payload-proof)
      + [Delegation payload+proof](#delegation-payload-proof)
      + [Update system payload+proof](#update-system-payload-proof)
  + [Transaction-related types](#transaction-related-types)
    * [Transaction](#transaction)
      + [Address](#address)
      + [Witness](#witness)
      + [Script](#script)
    * [Merkle tree](#merkle-tree)
  + [SSC-related types](#ssc-related-types)
    * [VSS keys](#vss-keys)
    * [Certificate, certificates map](#certificate-certificates-map)
    * [Commitment, commitments map](#commitment-commitments-map)
    * [Opening, openings map](#opening-openings-map)
    * [Encrypted/decrypted share, shares map](#encrypted-decrypted-share-shares-map)
  + [Delegation-related types](#delegation-related-types)
    * [Proxy key](#proxy-key)
    * [Proxy signature](#proxy-signature)
  + [Update system related types](#update-system-related-types)
    * [Update data](#update-data)
    * [Update proposal](#update-proposal)
    * [Update vote](#update-vote)

## Prerequisites

  * You should have read the
    [Ouroboros paper](https://eprint.iacr.org/2016/889.pdf).

  * You should be familiar with basic Haskell types (e.g. `Int`) and with
    some less common Haskell types (e.g. `NonEmpty`); when in doubt,
    use [Hoogle](https://hoogle.haskell.org).

  * Basic knowledge of the concept of *secret sharing* is required, see the
    section [SSC-related types](#ssc-related-types) for some links.

  * We assume that you know about hashing and public key cryptography used
    in Cardano SL – e.g. `Hash`, `PublicKey`, `Signature`,
    `RedeemPublicKey`. One additional thing that can be noted here is that a
    `StakeholderId` is a hash (`AddressHash`) of a public key.

  * We also assume that you know about `AsBinary`. If you don't, a
    one-sentence explanation is that `AsBinary a` contains a serialized `a`.
    It's intended to be used for types like public keys, where
    deserialization can't fail (unless the key has a wrong length) but still
    takes significant time and so we'd like to do it lazily.

  * Finally, we assume that you've
    read [Unknown data handling](overall.md#unknown-data-handling), which
    explains things like `TxInUnknown` and `Attributes`. Whenever you see
    words “attributes map” or “constructor provided for backwards
    compatibility”, the linked document is probably relevant.

## Genesis block

*keywords: `GenesisBlock`*

Genesis blocks are blocks that are created at epoch boundary by nodes. They
don't contain any data that can't be deduced from the blockchain, but they
are sent over the network anyway because that's more convenient.

A genesis block is implemented as a `GenericBlock`:

```haskell
type GenesisBlock = GenericBlock GenesisBlockchain
```

It contains the following fields:

  * `_gbHeader :: GenesisBlockHeader` – a [header](#genesis-block-header)

  * `_gbBody :: Body GenesisBlockchain` –
    a [payload](#genesis-block-payload) consisting of a list of slot leaders
    chosen for the current epoch (i.e. the one which begins with this block)

  * `_gbExtra :: GenesisExtraBodyData` – an attributes map, currently empty
    (`Attributes ()`)

Invariants:

  * Body proof in the header must be valid.

### Genesis block header

*keywords: `GenesisBlockHeader`; `ConsensusData GenesisBlockchain`,
`GenesisConsensusData`; `ExtraHeaderData GenesisBlockchain`,
`GenesisExtraHeaderData`, `GenesisHeaderAttributes`*

A genesis block header is a `GenericBlockHeader`:

```haskell
type GenesisBlockHeader = GenericBlockHeader GenesisBlockchain
```

It contains the following fields:

  * `_gbhPrevBlock :: HeaderHash` – a hash of the previous block's header

  * `_gbhBodyProof :: BodyProof GenesisBlockchain` – a hash of `_gbLeaders`
    from the payload

  * `_gbhConsensus :: ConsensusData GenesisBlockchain` – meta-information
    about the block:

      + `_gcdEpoch :: EpochIndex` – the epoch which the block belongs to (a
        genesis block technically is at the very beginning of an epoch)
      + `_gcdDifficulty :: ChainDifficulty` – difficulty of the chain ending
        in this block (i.e. number of main blocks between the first block
        ever and this block, inclusive)

  * `_gbhExtra :: GenesisExtraHeaderData` – an attributes map, currently
    empty (`Attributes ()`)

### Genesis block payload

*keywords: `Body GenesisBlockchain`, `GenesisBody`; `SlotLeaders`*

The body contains a single non-empty list of slot leaders that were chosen
for the current epoch:

```haskell
data Body GenesisBlockchain = GenesisBody
    { _gbLeaders :: SlotLeaders
    }

type SlotLeaders = NonEmpty StakeholderId
```

### Genesis block extra

*keywords: `ExtraBodyData GenesisBlockchain`, `GenesisExtraBodyData`;
`GenesisBodyAttributes`*

The extra data stored in a genesis block (`_gbExtra`) currently contains
only an empty attributes map:

```haskell
data GenesisExtraBodyData = GenesisExtraBodyData
    { _gebAttributes :: GenesisBodyAttributes
    }

type GenesisBodyAttributes = Attributes ()
```

It might be extended later.

## Main block

*keywords: `MainBlock`*

Main blocks are blocks with actual blockchain-related data (e.g.
transactions). Every slot, a single main block is generated by the slot
leader and sent to other nodes.

A main block is implemented as a `GenericBlock`:

```haskell
type MainBlock = GenericBlock MainBlockchain
```

It contains the following fields:

  * `_gbHeader :: MainBlockHeader` – a [header](#main-block-header)

  * `_gbBody :: Body MainBlockchain` – several payloads
    ([transactions](#transactions-payload-proof),
     [SSC](#ssc-payload-proof),
     [delegation](#delegation-payload-proof), and
     [update system](#update-system-payload-proof))

  * `_gbExtra :: MainExtraBodyData` – an attributes map, currently empty
    (`Attributes ()`)

Invariants:

  * Header:
      + Body proof (`_gbhBodyProof`) must be valid.
      + Extra body hash (`_mehEBDataProof`) must be valid.

  * SSC payload:
      + Certificates must have valid time-to-live – the protocol defines
        values `(vssMinTTL, vssMaxTTL)`, and we check that for each
        certificate, `certExpiryEpoch - blockEpoch + 1` lies in the interval
        `[vssMinTTL; vssMaxTTL]`.
      + Commitments must have right signatures. To verify commitment's
        signature, we must know the epoch – therefore it can't be checked as
        a part of the commitment invariants.
      + The commitments should each contain at least one share, and the
        shares in commitments should be deserializable.
      + The payload type should correspond to the slot of the block
        (commitments are only allowed in slots `[0;2k)`, openings – in slots
        `[4k;6k)`, and shares – in slots `[8k;10k)`).

  * Delegation payload:
      + All proxy keys must have the same epoch as the block itself.

### Main block header

*keywords: `MainBlockHeader`; `ConsensusData MainBlockchain`,
`MainConsensusData`; `MainExtraHeaderData`*

A main block header is a `GenericBlockHeader`:

```haskell
type MainBlockHeader = GenericBlockHeader MainBlockchain
```

It contains the following fields:

  * `_gbhPrevBlock :: HeaderHash` – a hash of the previous block's header.

  * `_gbhBodyProof :: BodyProof MainBlockchain` – proofs (e.g. hashes) of
    payloads from the block body; they will be discussed in the section
    about [block payload](#main-block-payload-proof).

  * `_gbhConsensus :: ConsensusData MainBlockchain` – meta-information about
    the block:

      + `_mcdSlot :: SlotId` – the slot for which the block was generated
      + `_mcdLeaderKey :: PublicKey` – public key of the slot leader (which
        may be different from the block issuer, because of delegation; if
        the issuer isn't the slot leader, the signature will contain the
        actual issuer)
      + `_mcdDifficulty :: ChainDifficulty` – difficulty of the chain ending
        in this block (i.e. number of main blocks between the first block
        ever and this block, inclusive)
      + `_mcdSignature :: BlockSignature` – a signature of the block by its
        issuer (if the issuer isn't the slot leader, it will be a delegated
        signature confirming issuer's right to issue the block in this slot)

  * `_gbhExtra :: MainExtraHeaderData` – more information about the block:

      + `_mehBlockVersion :: BlockVersion` – the block version; see
        [Software and block versions](us.md#software-and-block-versions).
        Block version can be associated with a set of protocol rules. Rules
        associated with `_mehBlockVersion` from a block are the rules used to
        create that block (i.e. the block must adhere to these rules).
      + `_mehSoftwareVersion :: SoftwareVersion` – the software version (see
        the same link); the version of software that created the block
      + `_mehEBDataProof :: Hash MainExtraBodyData` – a hash of the extra
        data in the block (since ultimately a header needs to checksum *all*
        data in the block)
      + `_mehAttributes :: BlockHeaderAttributes` – an attributes map to
        extend the header with more fields, currently empty

Invariants:

  * The signature must be valid.

  * If the signature is a delegated signature (`BlockPSignatureLight` or
    `BlockPSignatureHeavy`), it must not be self-signed – i.e. the
    `pskIssuerPk` and `pskDelegatePk` must be different.

### Main block signature

*keywords: `BlockSignature`, `MainToSign`*

A `BlockSignature` verifies that the block was issued by someone who had a
right to issue it, and also verifies that the block hasn't been tampered
with:

```haskell
data BlockSignature
    = BlockSignature (Signature MainToSign)
    | BlockPSignatureLight (ProxySigLight MainToSign)
    | BlockPSignatureHeavy (ProxySigHeavy MainToSign)
```

There are three kinds of signatures:

  * `BlockSignature` – simply a signature of the block by its issuer

  * `BlockPSignatureLight` – a light delegation signature (not discussed in
    this document); this signature is used when the right to issue blocks
    has been transferred on a temporary basis

  * `BlockPSignatureHeavy` – a heavy delegation signature, used when the
    right to issue blocks has been transferred “until further notice”

The thing signed by a `BlockSignature` isn't the whole block, but basically
`MainBlockHeader` without the `BlockSignature`. We avoid signing the whole
block because signing is an expensive process and we'd like to sign as few
bytes as we can get away with; since a `MainBlockHeader` already contains
hashes of all parts of the block, it works out. The exact parts that are
signed are specified by `MainToSign`:

```haskell
data MainToSign = MainToSign
    { _msHeaderHash  :: HeaderHash                -- previous block's header
    , _msBodyProof   :: BodyProof MainBlockchain  -- hashes of block's body
    , _msSlot        :: SlotId                    -- current slot
    , _msChainDiff   :: ChainDifficulty           -- difficulty
    , _msExtraHeader :: MainExtraHeaderData       -- extra data from the header
    }
```

The choice of signed data ensures that the whole block can be verified given
a `BlockSignature`.

### Main block payload+proof

*keywords: `Body MainBlockchain`, `MainBody`; `BodyProof MainBlockchain`,
`MainProof`*

The block body consists of four payloads – transactions, SSC, delegation,
and update system. In further sections we discuss each of those payloads, as
well as corresponding proofs (stored in the header).

```haskell
data Body MainBlockchain = MainBody
    { _mbTxPayload     :: TxPayload
    , _mbSscPayload    :: SscPayload
    , _mbDlgPayload    :: DlgPayload
    , _mbUpdatePayload :: UpdatePayload
    }
```

#### Transactions payload+proof

*keywords: `TxPayload`, `TxProof`*

The transaction payload consists of a [Merkle tree](#merkle-tree) with
transactions, and a list of witnesses corresponding to those transactions:

```haskell
data TxPayload = UnsafeTxPayload
    { _txpTxs       :: MerkleTree Tx         -- transactions
    , _txpWitnesses :: [TxWitness]           -- witnesses
    }
```

The invariant is that the tree of transactions and the list of witnesses
have the same number of elements.

The reason we separate transactions and their witnesses is that light
clients might want to request transactions but not witnesses (since they
don't have enough information to verify the witnesses anyway), and- in order
to be able to verify the acquired list of transactions given only the block
header, we need a hash of transactions (separated from witnesses) in the
header.

----------------------------------------

The proof of `TxPayload` looks like this:

```haskell
data TxProof = TxProof
    { txpNumber        :: Word32             -- number of transactions
    , txpRoot          :: MerkleRoot Tx      -- root of the transaction tree
    , txpWitnessesHash :: Hash [TxWitness]   -- hash of the witnesses list
    }
```

  * The integrity of the transaction tree is ensured by recording the size
    and root of the tree; the root of a Merkle tree is a hash which depends
    on all other elements, and the shape of the tree is uniquely determined
    by the number of elements in the tree.

  * The integrity of the witnesses list is ensured by recording its hash.

#### SSC payload+proof

*keywords: `SscPayload`, `SscProof`, `VssCertificatesHash`*

The SSC payload always contains a certificates map (`VssCertificatesMap`)
and can optionally contain *exactly one* of these: a commitments map, an
openings map, or a shares map. You should see the section
about [SSC-related types](#ssc-related-types) if you want to know what
purpose they serve.

```haskell
data SscPayload
    = CommitmentsPayload
        { spComms    :: CommitmentsMap
        , spVss      :: VssCertificatesMap }
    | OpeningsPayload
        { spOpenings :: OpeningsMap
        , spVss      :: VssCertificatesMap }
    | SharesPayload
        { spShares   :: SharesMap
        , spVss      :: VssCertificatesMap }
    | CertificatesPayload
        { spVss      :: VssCertificatesMap }
```

This type has no invariants.

----------------------------------------

The proof of `SscPayload` simply consists of hashes of its fields:

```haskell
data SscProof
    = CommitmentsProof
        { sprComms    :: Hash CommitmentsMap
        , sprVss      :: VssCertificatesHash }
    | OpeningsProof
        { sprOpenings :: Hash OpeningsMap
        , sprVss      :: VssCertificatesHash }
    | SharesProof
        { sprShares   :: Hash SharesMap
        , sprVss      :: VssCertificatesHash }
    | CertificatesProof
        { sprVss      :: VssCertificatesHash }
```

Instead of `Hash VssCertificatesMap` we use a `VssCertificatesHash`, which
is defined differently:

```haskell
type VssCertificatesHash = Hash (HashMap StakeholderId VssCertificate)
```

The reason for that is that hashing is done after serialization, and at some
point the serialization format for `VssCertificatesHash` was changed from a
map to a set. Since we can't change the protocol easily at this point, for
hashing we still use the map representation (where the map key corresponding
to each certificate is a `StakeholderId` derived from its the certificate's
`vcSigningKey`).

#### Delegation payload+proof

*keywords: `DlgPayload`*

The delegation payload is a list of proxy keys. The invariant is that no two
proxy keys have the same issuer (`pskIssuerPk`).

```haskell
newtype DlgPayload = UnsafeDlgPayload [ProxySKHeavy]
```

The proof used for this type is simply a `Hash DlgPayload`.

#### Update system payload+proof

*keywords: `UpdatePayload`, `UpdateProof`*

The update system payload contains a list of votes and (optionally) an
update proposal.

```haskell
data UpdatePayload = UpdatePayload
    { upProposal :: Maybe UpdateProposal
    , upVotes    :: [UpdateVote]
    }
```

This type has no invariants.

The proof used for this type is simply a `Hash UpdatePayload`.

## Transaction-related types

### Transaction

*keywords: `Tx`, `TxId`, `TxIn`, `TxOut`, `TxAttributes`*

A transaction (`Tx`) can be thought of as a command to destroy several
*unspent outputs* and create new unspent outputs, possibly with different
owners. In this fashion, money can be transferred from one person to another
by destroying money belonging to the first person and creating money that
would belong to the second person. (See [Transactions in Cardano SL][] for
more details.)

[Transactions in Cardano SL]: https://cardanodocs.com/cardano/transactions/

Here is the definition of a transaction:

```haskell
data Tx = UnsafeTx
    { _txInputs     :: NonEmpty TxIn         -- inputs
    , _txOutputs    :: NonEmpty TxOut        -- outputs
    , _txAttributes :: TxAttributes          -- attributes
    }
```

The invariant is that all outputs must have value bigger than 0. Two
additional invariants (“there's at least one input” and “there's at least
one output”) are always satisfied thanks to using `NonEmpty`.

Transactions are referred to by their hashes:

```haskell
type TxId = Hash Tx
```

----------------------------------------

Inputs are represented like this: since currently unspent outputs can only
originate from transactions, any unspent output can be referred to by
specifying a transaction ID and index in the list of that transaction's
outputs:

```haskell
data TxIn
    = TxInUtxo                      -- an output of some other transaction
        { txInHash  :: TxId
        , txInIndex :: Word32
        }
    | TxInUnknown Word8 ByteString  -- for backwards compatibility
```

Outputs themselves are represented like this:

```haskell
data TxOut = TxOut
    { txOutAddress :: Address  -- the owner of the output
    , txOutValue   :: Coin     -- amount of money
    }
```

#### Address

*keywords: `Address`, `AddrType`, `AddrSpendingData`*

An address (`Address`) is something that can be an output of a transaction
(i.e. something we can send coins to). The structure of an `Address` is
complicated due to several constraints placed on its design. This section
starts with discussing several smaller types, and then looks at `Address'`
and finally `Address`.

First of all, there are three kinds of addresses:

```haskell
data AddrType
    = ATPubKey              -- pay to public key
    | ATScript              -- pay to script
    | ATRedeem              -- pay to redeem public key
    | ATUnknown Word8       -- (for backwards compatibility)
```

For each of those kinds, there is a corresponding constructor of
`AddrSpendingData` – a type that specifies the condition which somebody has
to satisfy in order to spend the funds:

```haskell
data AddrSpendingData
    = PubKeyASD PublicKey
    | ScriptASD Script
    | RedeemASD RedeemPublicKey
    | UnknownASD Word8 ByteString    -- for backwards compatibility, too
```

  * `PubKeyASD pubkey` – you have to provide a signature by given key

  * `ScriptASD validator` – you have to provide a redeemer script that will
    match the validating script (see [Witness](#witness) for more details
    about what it means)

  * `RedeemASD pubkey` – you have to provide a signature by given redeem key

These two types are stored in an `Address'` along with address attributes:

```haskell
newtype Address' =
    Address' (AddrType, AddrSpendingData, Attributes AddrAttributes)
```

Address's attributes store the derivation path (not explained in this
document) and stake distribution. When money is sent to an address, we take
only the distribution into account for the purpose of choosing slot leaders;
it doesn't matter who the money actually belongs to (as determined by
`AddrSpendingData`).

```haskell
data AddrAttributes = AddrAttributes
    { aaPkDerivationPath  :: Maybe HDAddressPayload
    , aaStakeDistribution :: AddrStakeDistribution
    }

data HDAddressPayload = HDAddressPayload ByteString

data AddrStakeDistribution
    = BootstrapEraDistr
    | SingleKeyDistr StakeholderId
    | UnsafeMultiKeyDistr (Map StakeholderId CoinPortion)
    -- ^ Stake distribution which gives stake to multiple
    -- stakeholders. 'CoinPortion' is a portion of an output (output
    -- has a value, portion of this value is stake). The constructor
    -- is unsafe because there are some predicates which must hold:
    --   • the sum of portions must be @maxBound@ (basically 1);
    --   • all portions must be positive;
    --   • there must be at least 2 items, because if there is only one item,
    --     'SingleKeyDistr' can be used instead (which is smaller).

newtype CoinPortion = CoinPortion Word64
```

There are three available distributions:

  * `BootstrapEraDistr` – stake is assigned to bootstrap era stakeholders

  * `SingleKeyDistr id` – all stake is assigned to the stakeholder with
    given ID

  * `UnsafeMultiKeyDistr map` – stake is divided between several
    stakeholders. For each stakeholder there's provided a `CoinPortion` –
    the dole of the stake that should be assigned to that stakeholder.

Invariants of `UnsafeMultiKeyDistr`:

  * The sum of portions must be 1.
  * All portions must be positive (i.e. not 0).
  * There must be at least two stakeholders.

A `CoinPortion` is a newtype for `Word64`, and is interpreted as the
numerator of `x / 10^15`. Its invariant is that the `x` must lie in the
interval `[0; 10^15]`.

----------------------------------------

An `Address'` provides enough information to receive funds. However, a
problem with `Address'` is that it reveals public keys, which is something
we don't want to do (to stay safe if public key cryptography gets broken in
the future). Therefore we wrap `Address'` into a structure that stores
`Address'`'s hash and all data from `Address'` except for
`AddrSpendingData`:

```haskell
type AddressHash = AbstractHash Blake2b_224

data Address = Address
    { addrRoot       :: AddressHash Address'
    , addrAttributes :: Attributes AddrAttributes
    , addrType       :: AddrType
    }
```

At this point a perceptive reader could note that we could accomplish the
same goal in a simpler manner – just store a hash of `AddrSpendingData` and
dispose of `Address'` entirely. However, it would mean that when you give
your address to someone, they can easily change its `AddrAttributes` and the
address would still be valid. This gives rise to certain legal problems. For
instance, if you are a shop and you've been paid to an address that has had
its `AddrAttributes` modified, you can still use the money but you might no
longer have stake assigned to you – does that constitute a valid payment or
not?

#### Witness

*keywords: `TxWitness`, `TxInWitness`, `TxSig`, `TxSigData`*

A transaction witness (`TxWitness`) is a proof which authorizes spending
each of the inputs of the transaction.
(See [Proofs of transaction legitimacy][] for more details.) A `TxWitness`
is a list of *input witnesses:*

[Proofs of transaction legitimacy]: https://cardanodocs.com/cardano/transactions/#proofs-of-transaction-legitimacy

```haskell
type TxWitness = Vector TxInWitness
```

There is a separate type of input witnesses (`TxInWitness`) for each
`AddrType`:

```haskell
data TxInWitness

    = PkWitness
        { twKey :: PublicKey        -- key corresponding to the address
        , twSig :: TxSig            -- a signature by that key
        }

    | ScriptWitness
        { twValidator :: Script     -- validator
        , twRedeemer  :: Script     -- redeemer
        }

    | RedeemWitness
        { twRedeemKey :: RedeemPublicKey              -- key
        , twRedeemSig :: RedeemSignature TxSigData    -- signature
        }

    | UnknownWitnessType Word8 ByteString
```

  * A `PkWitness` provides a public key (which is checked to correspond to
    the key hash stored in address) and a transaction signature by that key.
    Specifically, we sign a `Hash` of the whole transaction:
  
    ```haskell
    type TxSig = Signature TxSigData
   
    data TxSigData = TxSigData
        { txSigTxHash :: Hash Tx
        }
    ```

  * A `ScriptWitness` provides a pair of scripts – `twValidator` is checked
    to correspond to the hash in the address and then the pair is executed
    together and if it returns `True`, the input is considered valid.

  * A `RedeemWitness` is similar to a `PkWitness`, but it's only used for
    redemption.

  * An `UnknownWitnessType` is used for providing backwards compatibility.

#### Script

*keywords: `Script`, `ScriptVersion`*

A transaction script (`Script`) is a collection of definitions written
in [Plutus][]. It can be deserialized and loaded, and any definition in it
can be evaluated. 

[Plutus]: https://cardanodocs.com/technical/plutus/introduction/

```haskell
data Script = Script
    { scrVersion :: ScriptVersion   -- version of Plutus to use
    , scrScript  :: ByteString      -- serialized script
    }

type ScriptVersion = Word16
```

`scrScript` doesn't necessarily have to be a valid script (if it can't be
deserialized, it will be handled on later stages).

### Merkle tree

*keywords: `MerkleTree`, `MerkleNode`, `MerkleRoot`*

A [Merkle tree](https://en.wikipedia.org/wiki/Merkle_tree) is a binary tree
where each leaf contains a value, and each node contains a hash that depends
on hashes of its two children. For convenience, we define a newtype for
hashes used in Merkle trees:

```haskell
newtype MerkleRoot a = MerkleRoot (Hash Raw)
```

For the tree itself, we use two types:

* `MerkleNode` – a simple, recursively defined type

* `MerkleTree` – a wrapper that has a field for tree size and also provides
  a way to denote an empty tree

```haskell
data MerkleTree a
    = MerkleEmpty                          -- empty tree
    | MerkleTree Word32 (MerkleNode a)     -- non-empty tree, with size
```

```haskell
data MerkleNode a

    -- a branch with two children
    = MerkleBranch 
        { mRoot  :: MerkleRoot a           -- combined hash
        , mLeft  :: MerkleNode a           -- left subtree
        , mRight :: MerkleNode a           -- right subtree
        }

    -- a single leaf
    | MerkleLeaf
        { mRoot :: MerkleRoot a            -- hash of the element
        , mVal  :: a                       -- the element stored in the leaf
        }
```

----------------------------------------

Our implementation follows [RFC 6962][], except that we use a different hash
instead of SHA-256. Read it to understand how exactly a tree is built and
how branches' hashes are computed from children's hashes.

[RFC 6962]: https://tools.ietf.org/html/rfc6962

## SSC-related types

This section assumes that you are familiar with [Ouroboros][]. You may also
want to read [PVSS Implementation in Cardano SL][] to understand how the
internals of `Commitment`, `Opening`, etc are implemented, and see the
sources of the [`pvss-haskell`][] library.

[Ouroboros]: https://eprint.iacr.org/2016/889.pdf
[PVSS Implementation in Cardano SL]: https://cardanodocs.com/technical/pvss/
[`pvss-haskell`]: https://github.com/input-output-hk/pvss-haskell

### VSS keys

*keywords: `VssPublicKey`, `VssKeyPair`*

SSC uses a different public key cryptography scheme. Instead of `PublicKey`
and `SecretKey` it uses `VssPublicKey` and `VssKeyPair`:

```haskell
newtype VssPublicKey = VssPublicKey Scrape.PublicKey
newtype VssKeyPair = VssKeyPair Scrape.KeyPair
```

### Certificate, certificates map

*keywords: `VssCertificate`, `VssCertificatesMap`*

Since everywhere else in Cardano we use `PublicKey` and `SecretKey`, we need
to be able to establish a correspondence between stakeholders' public keys
and their VSS keys. This is done by publishing a certificate
(`VssCertificate`):

```haskell
data VssCertificate = UnsafeVssCertificate
    { vcVssKey      :: AsBinary VssPublicKey
    , vcExpiryEpoch :: EpochIndex
    , vcSignature   :: Signature (AsBinary VssPublicKey, EpochIndex)
    , vcSigningKey  :: PublicKey
    }
```

The fields mean:

  * `vcVssKey` – stakeholder's VSS key

  * `vcSigningKey` – stakeholder's public key

  * `vcExpiryEpoch` – the epoch up to which the certificate is considered
    valid

  * `vcSignature` – a signature of `(vcVssKey, vcExpiryEpoch)`

The invariant of `VssCertificate` is that the signature must be valid.

----------------------------------------

A certificates map (`VssCertificatesMap`) is a set of certificates, indexed
by stakeholder IDs (hashes of certificates' `vcSigningKey`s) for
performance. There are two invariants:

  * The IDs must indeed correspond to certificates' keys.
  * No two certificates can have the same `vcVssKey`.

```haskell
newtype VssCertificatesMap = 
    UnsafeVssCertificatesMap (HashMap StakeholderId VssCertificate)
```

### Commitment, commitments map

*keywords: `Commitment`, `CommitmentSignature`, `SignedCommitment`,
`CommitmentsMap`, `SecretProof`*

After an SSC participant generates a secret, it also generates a
`Commitment`:

```haskell
data Commitment = Commitment
    { commProof  :: SecretProof                             -- proof of secret
    , commShares :: HashMap (AsBinary VssPublicKey)         -- encrypted shares
                            (NonEmpty (AsBinary EncShare))
    }
```

The fields mean:

  * `commShares` – a number of encrypted shares, which can be decrypted and
    recovered back into a secret. For each participant's VSS key the
    `HashMap` lists one or
    more [shares](#encrypted-decrypted-share-shares-map) encrypted with that
    participant's key. They can only be decrypted with the corresponding
    `VssKeyPair`.

  * `commProof` – a proof, for which it is possible to check that a) it
    corresponds to the encrypted shares, and b) a certain secret corresponds
    or doesn't correspond to it.

By itself a commitment isn't signed – there's a separate type called
`SignedCommitment`:

```haskell
type SignedCommitment = (PublicKey, Commitment, CommitmentSignature)

type CommitmentSignature = Signature (EpochIndex, Commitment)
```

This is similar to how signing a `VssCertificate` works, except for one
thing: each commitment is only valid for one epoch, and we always know what
epoch it is, so we don't have to include the epoch into `SignedCommitment`.

----------------------------------------

A commitments map (`CommitmentsMap`) is a set of signed commitments, indexed
by stakeholder IDs. The invariant is that the IDs must correspond to
commitments' `PublicKey`s.


```haskell
newtype CommitmentsMap = 
    CommitmentsMap (HashMap StakeholderId SignedCommitment)
```

----------------------------------------

The implementation of `SecretProof` is given below, but it will not be
explained here as it's out of scope for this document.

```haskell
data SecretProof = SecretProof
    { spExtraGen       :: Scrape.ExtraGen
    , spProof          :: Scrape.Proof
    , spParallelProofs :: Scrape.ParallelProofs
    , spCommitments    :: [Scrape.Commitment]
    }
```

### Opening, openings map

*keywords: `Opening`, `Secret`, `OpeningsMap`*

An opening is sent at the second stage of SSC. It reveals the participant's
secret (which can be thought of as a simple bytestring).

```haskell
newtype Opening = Opening (AsBinary Secret)
```

----------------------------------------

An openings map (`OpeningsMap`) is a map from participants' IDs to their
openings. It has no invariants (since the opening doesn't have a public key
in it).

```haskell
type OpeningsMap = HashMap StakeholderId Opening
```

### Encrypted/decrypted share, shares map

*keywords: `EncShare`, `DecShare`; `SharesMap`, `InnerSharesMap`*

A share is a part of a secret (see Wikipedia on [secret sharing][]). With
more than a certain percentage of decrypted shares (`DecShare`) it is
possible to recover the secret that was used to create the shares. An
encrypted share (`EncShare`) can be decrypted with the corresponding VSS
keypair.

[secret sharing]: https://en.wikipedia.org/wiki/Secret_sharing

```haskell
newtype DecShare = DecShare Scrape.DecryptedShare

newtype EncShare = EncShare Scrape.EncryptedSi
```

----------------------------------------

A shares map (`SharesMap`) is a set of shares created by some participants
and decrypted by other participants:

  * outer key = who decrypted the share
  * inner key = who created the share

```haskell
type SharesMap = HashMap StakeholderId InnerSharesMap

type InnerSharesMap = HashMap StakeholderId (NonEmpty (AsBinary DecShare))
```

So, an `InnerSharesMap` contains all shares decrypted by a certain
participant, and keys of `InnerSharesMap` denote who created some particular
set of shares.

## Delegation-related types

Note: this section talks about heavy delegation only.

### Proxy key

*keywords: `ProxySecretKey`, `ProxyCert`, `ProxySKHeavy`*

A proxy key (`ProxySecretKey w`) is a `PublicKey`, stored in
`pskDelegatePk`, that comes equipped with three things:

  * `pskOmega` – a value of type `w`

  * `pskIssuerPk` – a key which delegated something (e.g. block issuing
    rights) to `pskDelegatePk`

  * `pskCert` – a proof that `pskIssuerPk` has delegated to `pskDelegatePk`

```haskell
data ProxySecretKey w = ProxySecretKey
    { pskOmega      :: w              -- auxiliary value
    , pskIssuerPk   :: PublicKey      -- delegation happens from this key
    , pskDelegatePk :: PublicKey      --   ...to this key
    , pskCert       :: ProxyCert w    -- a proof by 'pskIssuerPk'
    }
```

A `ProxyCert` is just a signature of `(pskDelegatePk, pskOmega)`:

```
newtype ProxyCert w = ProxyCert { unProxyCert :: CC.XSignature }
```

Note that “proxy secret key” is a misnomer – it really is a public key, not
a secret key. Also note that it might look like `ProxySecretKey` has an
invariant (“the certificate in it is valid”), but in reality this isn't an
invariant, it's simply something we check in code at certain points.

----------------------------------------

Now that we have `ProxySecretKey w`, we can specify different `w`s and get
different things. For heavy delegation we set `w = EpochIndex`:

```haskell
type ProxySKHeavy = ProxySecretKey EpochIndex
```

So, a proxy delegation key contains the epoch when it was issued. This is
needed to prevent replay attacks – if later on the proxy key gets revoked,
the attacker wouldn't be able to simply resend the original key.

### Proxy signature

*keywords: `ProxySignature`, `ProxySigHeavy`*

A `ProxySignature w a` is a signature of `a` by a `ProxySecretKey w`:

```haskell
data ProxySignature w a = ProxySignature
    { psigPsk :: ProxySecretKey w
    , psigSig :: XSignature
    }
```

Just like before, for heavy delegation we set `w = EpochIndex`:

```haskell
type ProxySigHeavy a = ProxySignature EpochIndex a
```

The reason `psigPsk` is included into a proxy signature is that we need to
know who was the actual signer to check the singature.

## Update system related types

### Update data

*keywords: `UpdateData`*

An `UpdateData` contains information that is needed to actually update from
one version of the application to another. For each supported OS we need a
different `UpdateData`.

```haskell
data UpdateData = UpdateData
    { udAppDiffHash  :: Hash Raw
    , udPkgHash      :: Hash Raw
    , udUpdaterHash  :: Hash Raw
    , udMetadataHash :: Hash Raw
    }
```

The fields mean:

  * `upAppDiffHash` – hash of binary diff between two applications. This
    diff can be used for directly patching binary files on user's computer.

  * `udPkgHash` – hash of an installer that can be used to install the new
    version from application from scratch instead of patching the existing
    application.

  * `udUpdaterHash` – currently unused.

  * `udMetadataHash` – currently unused.

(Note that an `UpdateData` contains only hashes – the diffs/installers can
be queried by hash from update servers or via P2P.)

### Update proposal

*keywords: `UpdateProposal`, `UpdateProposalToSign`, `UpAttributes`,
`BlockVersion`, `BlockVersionModifier`, `SoftwareVersion`, `SystemTag`*

An update proposal (`UpdateProposal`) is a complete description of a client
update published on the blockchain.

```haskell
data UpdateProposal = UnsafeUpdateProposal
    { upBlockVersion    :: BlockVersion
    , upBlockVersionMod :: BlockVersionModifier
    , upSoftwareVersion :: SoftwareVersion
    , upData            :: HashMap SystemTag UpdateData
    , upAttributes      :: UpAttributes
    , upFrom            :: PublicKey
    , upSignature       :: Signature UpdateProposalToSign
    }
```

The fields mean:

  * `upBlockVersion`, `upBlockVersionMod`, `upSoftwareVersion` –
    see [Software and block versions](us.md#software-and-block-versions)

  * `upData` – data required to perform the update on different systems;
    this map can be empty if the update proposal only changes some constants
    and doesn't require modifying the application's binaries

  * `upAttributes` — the attributes map, currently empty (i.e. `Attributes
    ()`)

  * `upFrom` – the public key of whoever proposed the `UpdateProposal`

  * `upSignature` – a signature of `UpdateProposalToSign` (a type containing
    all fields of `UpdateProposal` except for `upFrom` and `upSignature`) by
    the `upFrom` key

Invariant: `upSignature` is valid.

----------------------------------------

The `SystemTag` type is used for identifying different systems. It is a
synonym for `Text` (a sample value could be e.g. `"macos64"`):

```haskell
newtype SystemTag = SystemTag Text
```

There are two associated invariants:

  * It may only contain ASCII symbols.

  * It may not be longer than 10 characters.

### Update vote

*keywords: `UpdateVote`, `UpId`*

Stakeholders may vote for published proposals. A vote can be either positive
or negative. The type for votes is given below:

```haskell
data UpdateVote = UpdateVote
    { uvKey        :: PublicKey               -- public key of the voter
    , uvProposalId :: UpId                    -- proposal which is voted for
    , uvDecision   :: Bool                    -- whether the vote is positive
    , uvSignature  :: Signature (UpId, Bool)  -- a signature by 'uvKey'
    }
```

Proposals are identified simply by their hashes:

```haskell
type UpId = Hash UpdateProposal
```

The invariant of `UpdateVote` is that the signature is valid.
