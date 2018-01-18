# Cardano SL HD Wallets

## General scheme
    
HD wallet has tree-based structure.
The root of the tree is user's public-secret key pair.
Each node in the tree corresponds to public-secret key pair.

Each child node is identified by an index of this child,
so child's public and secret keys can be _derived_ from parent
by this index using parent's public-secret key pair.
On the other hand, one can't learn anything about parent
in reasonable time, unless one has some extra info.

## Tree structure

In general case tree structure can be arbitrary but
we use specific structure.
We have two layered tree, nodes of the first layer are called "accounts"
and nodes of the second layer (leaves) are called "addresses" and are leaves of a tree.
There can be potentially 2³² accounts and each account can have 2³² addresses,
so wallet can have 2⁶⁴ addresses in total.

The overall scheme is represented on the picture below.

                                    (wallet's public key and secret key)
                                    /             |                  \
                (account[0] (pk, sk))    (account[1] (pk, sk)) ... (account[2³²-1] (pk, sk))
                /                  \                                    \
    (address[0][0] (pk, sk)) ... (address[0][2³²-1] (pk, sk)) ... (address[2³²-1][2³²-1] (pk, sk))

So each address belongs to one account.
Address can contain money, be used as change address and so on.

Balance of each account can be computed as sum of balances of addresses which belong to it,
you can have different accounts for different purposes and so on.

## Derivation process

To derive secret key, parent secret key and passphrase are necessary.
Derivation is done using `deriveHDSecretKey`.

The deriving of public key can be performed using just a public key of a parent node.
Derivation is done using `deriveHDPublicKey`.

It's not allowed to derive a public key from parent public key for
indices which are greater than or equal to 2³¹:
for these indicies parent secret key is required.
This feature is called "hardened derivation":

You can get description of underlying cryptography [here](https://cardanolaunch.com/assets/Ed25519_BIP.pdf).

Note: all these functions don't require derivation level number,
only credential information (public key or secret key with passphrase)
and index of a child.
Also we can derive public and secret keys for any arbitrary number of levels.

## HD address payload

"Account" and "address" entities were mentioned,
but only addresses appear in the blockchain.
When we create a transaction, we specify address and coins for each `TxOut`.
So neither transaction or block knows anything about accounts and wallets.

We want to be able to track our addresses in the blockchain
to compute balance of wallet and related accounts,
but as it is said in [General scheme](#general-scheme) section we're not able
to determine a parent without some particular info.
So we can't determine account and wallet which address belongs to.

For each leaf let's store path from the root to this leaf along with an address.
So path is a list of derivation indices on each level.
To hide path from other parties we will encrypt it.
This encrypted derivation path is called `HDAddressPayload`
and stored in attributes of `Address` datatype.

Note: length of derivation path in our case is two: `[account index, address index]`.

## Payload encryption

To encrypt derivation path let's use AEAD scheme using ChaCha20 and Poly1305.
More information is available [here](https://tools.ietf.org/html/rfc7539).
Hash of root public key is used as a symmetric key for ChaChaPoly1305.
This hash called `HDPassphrase` and function `deriveHDPassphrase` generates
it by root public key.

Encryption of payload performs `packHDAddressAttr` which takes `HDPassphrase` and
derivation path and returns `HDAddressPayload`.
It serializes derivation path using `Bi` instance for it and then encrypts produced bytes sequence.

## Payload decryption

To decrypt encrypted derivation path we have to derive `HDPassphrase` again
from root public key and then try to decrypt `HDAddressPayload`.
If it's successfully decrypted then it implies address belongs to our tree
and, vice versa, address doesn't belong to our tree if decryption is failed.

Note: publishing of root public key gives other parties opportunity to reveal
all your addresses in the blockchain.
So it is unsafe to share root pk (like it can be done with a usual pk)
because it implies user deanonymization.

## Recovery process

So if we have a root secret key (or even root public key),
we can iterate over the whole blockchain and try to decrypt all met
addresses to determine which of them belong to our wallet.
We retrieve addresses along with whole hierarhy of tree because
decrypted derivation paths describe it.

Note: we are interested only in addresses mentioned in blockchain,
and we consider not mentioned addresses having zero balance.
So we don't store not mentioned addresses anywhere.

## Transaction creation

To spend `TxIn` wallet application must sign them using secret key and provide
public key as a witness.
For HD wallets we do the same thing: we derive secret and public key of
address corresponding to `TxIn` from root secret key using user's passphrase
and sign `TxIn` by derived secret key and put public key as a witness.

Note: although we can reveal all addresses just knowing root public key,
to spend `TxIn`s root secret key and user's passphrase are needed.
