<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Abstract</a></li>
<li><a href="#sec-2">2. Motivation</a></li>
<li><a href="#sec-3">3. Specification</a>
<ul>
<li><a href="#sec-3-1">3.1. Address structure</a></li>
<li><a href="#sec-3-2">3.2. Encoding of derivation paths</a></li>
</ul>
</li>
<li><a href="#sec-4">4. User stories</a></li>
<li><a href="#sec-5">5. Conclusion</a></li>
</ul>
</div>
</div>

    Layer(s): Core/Middleware
    Title: Backward-compatible protocol for changes in the cryptographic derivation scheme
    Author: Alfredo Di Napoli <alfredo.dinapoli@iohk.io>
    Status: Proposed
    Created: 2017-12-18

# Abstract<a id="sec-1" name="sec-1"></a>

The purpose of this document is to describe a way we can embed changes to the crypto scheme /
derivation path strategy in our `Address` structure without inflating its size and whilst preserving
backward compatibility.

# Motivation<a id="sec-2" name="sec-2"></a>

The cryptography in use today in our HD wallet structure might not be the one of tomorrow, and we
might want to upgrade it in the future without affecting wallet's ability to use Cardano.

**See [this PR](<https://github.com/input-output-hk/cardano-crypto/pull/9>) for a witness of the actual need.**

In the same vein, we might want to modify the way we perform the derivation of `Address` on our HD wallet tree.

# Specification<a id="sec-3" name="sec-3"></a>

**NOTE: We decided not to add an extra attribute to avoid inflating the overall Address size, as this is key for
short addresses.**

## Address structure<a id="sec-3-1" name="sec-3-1"></a>

At the time of writing (Dec 17) an `Address` in Cardano looks like this (**derived instances removed for brevity**):

``` haskell
data Address = Address
    { addrRoot       :: !(AddressHash Address')
    -- ^ Root of imaginary pseudo Merkle tree stored in this address.
    , addrAttributes :: !(Attributes AddrAttributes)
    -- ^ Attributes associated with this address.
    , addrType       :: !AddrType
    -- ^ The type of this address. Should correspond to
    -- 'AddrSpendingData', but it can't be checked statically, because
    -- spending data is hashed.
    }
```

Ignoring the extra fields, an `Address` also stores a bunch of `Attributes` which allows us to
attach extra "metadata" to an `Address` (incidentally, this is also why addresses in Cardano have
basically var-len size). `Attributes` are essentially a finite map, and are defined as:

``` haskell
-- In an `Address` we have `(Attributes AddrAttributes)`.
data AddrAttributes = AddrAttributes
    { aaPkDerivationPath  :: !(Maybe HDAddressPayload)
    , aaStakeDistribution :: !AddrStakeDistribution
    }

newtype UnparsedFields = UnparsedFields (Map Word8 ByteString)

data Attributes h = Attributes
    { -- | Data, containing known keys (deserialized)
      attrData   :: h
      -- | Remaining, unparsed fields.
    , attrRemain :: UnparsedFields
    }
```

In the current version of Cardano we have only two attributes: a derivation path and the stake distribution.

## Encoding of derivation paths<a id="sec-3-2" name="sec-3-2"></a>

Let's first recall how we serialise & deserialise attributes over the wire:

``` haskell
instance Bi (Attributes AddrAttributes) where
    encode attrs@(Attributes {attrData = AddrAttributes derivationPath stakeDistr}) =
        encodeAttributes listWithIndices attrs
      where
        listWithIndices :: [(Word8, AddrAttributes -> BS.ByteString)]
        listWithIndices =
            stakeDistributionListWithIndices <> derivationPathListWithIndices
        stakeDistributionListWithIndices =
            case stakeDistr of
                BootstrapEraDistr -> []
                _                 -> [(0, serialize' . aaStakeDistribution)]
        derivationPathListWithIndices =
            case derivationPath of
                Nothing -> []
                -- 'unsafeFromJust' is safe, because 'case' ensures
                -- that derivation path is 'Just'.
                Just _ ->
                    [(1, serialize' . unsafeFromJust . aaPkDerivationPath)]
    decode = decodeAttributes initValue go
      where
        initValue =
            AddrAttributes
            { aaPkDerivationPath = Nothing
            , aaStakeDistribution = BootstrapEraDistr
            }
        go n v acc =
            case n of
                0 -> (\distr -> Just $ acc {aaStakeDistribution = distr }    ) <$> deserialize' v
                1 -> (\deriv -> Just $ acc {aaPkDerivationPath = Just deriv }) <$> deserialize' v
                _ -> pure Nothing
```

The key insight is that each index associated with the `aaPkDerivationPath` and used during encoding/decoding
implicitly capture three things:

``` haskell
(encryption_scheme, derivation_path, crypto_scheme)
```

-   `encryption_scheme` represent which particular function was used to encrypt some of the attributes
    (for example it's ChaChaPoly today, it might change in the future);

-   `derivation_path` denotes the particular strategy used to derive an address from an HD-wallet path;

-   `crypto_scheme` refers to the particular cryptographic scheme in use.

This conceptually gives **different versions of a wallet the ability to conceptually recognise the
mechanism which was used in the first place.**

Practically speaking, we could release a new version of Cardano (say, `cardano-2.0`) which changes
the `Maybe HDAddressPayload` of a `aaPkDerivationPath` into (for example):

``` haskell
data AddressPayload =
    NoPayload
  | V1_Scheme HDAddressPayload
  | V2_Scheme HDAddressPayload
```

Then, we would encode this data like this (pseudocode):

``` haskell
derivationPathListWithIndices =
    case derivationPath of
        NoPayload -> []
        (V1_Scheme payload) -> [(1, serialize' . unsafeFromJust . aaPkDerivationPath)]
        (V2_Scheme payload) -> [(2, serialize' . unsafeFromJust . aaPkDerivationPath)]
```

User-facing code could then use the `AddressPayload` type to decide which [derivation scheme](<https://github.com/input-output-hk/cardano-crypto/pull/9/files>)
needs to be used.

Likewise, decoding could look like:

``` haskell
go n v acc =
    case n of
        0 -> (\distr -> Just $ acc {aaStakeDistribution = distr }    ) <$> deserialize' v
        1 -> (\deriv -> Just $ acc {aaPkDerivationPath = V1_Scheme deriv }) <$> deserialize' v
        2 -> (\deriv -> Just $ acc {aaPkDerivationPath = V2_Scheme deriv }) <$> deserialize' v
        _ -> pure NoPayload
```

## Note on update process

This change when introduced in new version of cardano shall be formed as a softfork update proposal.
(Otherwise addresses with new attribute will be rejected by non-updated nodes).

# User stories<a id="sec-4" name="sec-4"></a>

This section has the aim of prove the validity of the proposed design against a number of relevant user stories.
We will label `cardano-1.0` as the "old" version of a node, running the non-migrated code, and `cardano-2.0` as
the version running the updated node with the `V2` crypto scheme.

**(0). As a user of both versions, I want to generate a new `Address`, so that I can send & receive money.**

Under this scenario, the user would invoke `POST /api/addresses` (in the old API) and that would hook in the
underlying core/cardano-crypto, where we can use the pattern synonym [LatestScheme](<https://github.com/input-output-hk/cardano-crypto/pull/9/files#diff-87e4dd0ac655f1f7afddd94983ae60d6R16>)
to be sure we are always using the latest crypto scheme.

To the end user, the resulting `Address` will be just a base58-encoded string, so nothing will appear to have changed
on his side.

**(1). As a user running `cardano-1.0`, I want to send money to an address generated by a node running `cardano-2.0`.**

In order to do so, the user will need to input the `Address` into a Deadalus text field which will check its validity
by calling a `wallet-backend`'s endpoint, which will call `decodeTextAddress` under the hood. This will call `Bi.decodeFull`
on the base58-decoded `Address`. In this case, our decode code would look like:

``` haskell
-- This is the code running on the old node
go n v acc =
    case n of
        0 -> (\distr -> Just $ acc {aaStakeDistribution = distr }    ) <$> deserialize' v
        1 -> (\deriv -> Just $ acc {aaPkDerivationPath = Just deriv }) <$> deserialize' v
        _ -> pure Nothing -- <- This is the path which will be taken.
```

In this case, the attribute will be silently dropped but the payment will be allowed to go through, as the encrypted `aaPkDerivationPath` is
not needed to send a payment.

**(2). As a user running `cardano-1.0`, I want to receive money from a node running `cardano-2.0`.**

This is the dual of `(1).` Under this scenario, `cardano-2.0` will have to input `cardano-1.0` address
(which is in the old format), and `cardano-1.0` will need to look in the UTXO cache it maintains to understand
whether or not an address belongs to it. As the `Address` is in the old format, everything will work as before.

**(3). As a user running `cardano-2.0`, I want to send money to an address generated by a node runneng `cardano-1.0`.**

The only difference with `(1).` is the decoding code:

``` haskell
-- This is the code running on the new node
go n v acc =
    case n of
        0 -> (\distr -> Just $ acc {aaStakeDistribution = distr }    ) <$> deserialize' v
        1 -> (\deriv -> Just $ acc {aaPkDerivationPath = V1_Scheme deriv }) <$> deserialize' v -- <- This is the path taken
        2 -> (\deriv -> Just $ acc {aaPkDerivationPath = V2_Scheme deriv }) <$> deserialize' v
        _ -> pure NoPayload
```

This is not useful per-se during `Address` validation, but the attribute will be regularly recognised.

**(4). As a user running `cardano-2.0`, I want to receive money from a node running `cardano-1.0`.**

Similarly to `(2).` the wallet running `cardano-2.0` will scan the blockchain, try to decrypt each address 's `AddressPayload` and
succeed when the patter matching will hit \`2\`:

``` haskell
-- This is the code running on the new node
go n v acc =
    case n of
        0 -> (\distr -> Just $ acc {aaStakeDistribution = distr }    ) <$> deserialize' v
        1 -> (\deriv -> Just $ acc {aaPkDerivationPath = V1_Scheme deriv }) <$> deserialize' v
        2 -> (\deriv -> Just $ acc {aaPkDerivationPath = V2_Scheme deriv }) <$> deserialize' v -- <- This is the path taken
        _ -> pure NoPayload
```

**(5). As a user running `cardano-1.0`, I want to restore my wallet and my full balance.**

For this to work, the node will need to be able to scan the blockchain and to recognise addresses as being derived from
this wallet's root seed. To do so, it needs to decode & decrypt the signed attributes. Similar to story `(1).`, addresses in
the new format will cause the node to think there is no `aaPkDerivationPath` attribute attached. The decoding won't fail and
the address will be discarded. Conversely, addresses generated under `DerivationScheme1` will be recognised and checked as normal.

**(6). As a user running `cardano-2.0`, I want to restore my wallet and my full balance.**

In this circumstance, the node will need to face a slight extension to user story (5) deriving from the fact that now
the wallet addresses could be composed by a miscellanea of addresses, either generated with `DerivationScheme1` or with
`DerivationScheme2` (when it lands). It follow that is critical for this wallet node to be able to recognise both formats;
this is possible just by the virtue of the new `go` function:

``` haskell
-- This is the code running on the new node
go n v acc =
    case n of
        0 -> (\distr -> Just $ acc {aaStakeDistribution = distr }    ) <$> deserialize' v
        1 -> (\deriv -> Just $ acc {aaPkDerivationPath = V1_Scheme deriv }) <$> deserialize' v -- <- This is the path taken for old addresses (1.0)
        2 -> (\deriv -> Just $ acc {aaPkDerivationPath = V2_Scheme deriv }) <$> deserialize' v -- <- This is the path taken for new addresses (2.0)
        _ -> pure NoPayload
```

# Conclusion<a id="sec-5" name="sec-5"></a>

We have shown here a simple method to not add any extra attributes to an `Address` which would guarantee changes to the underlying crypto scheme
in a non-breaking setting.
