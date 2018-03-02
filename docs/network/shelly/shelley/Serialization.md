# Serialization

**Note**: there is an effort to decide on partial serialization/deserialization approach in [doc](https://github.com/input-output-hk/cardano-sl/blob/volhovm/csl1859-serialization-doc/docs/proposals/serialization.md) and [corresponding PR](https://github.com/input-output-hk/cardano-sl/pull/1903).

The time cost of (de)serialization appears in network latency: in order to
propagate a block throughout the network, each node will deserialize it from
the sender (publisher) or database, and then reserialize once for each
recipient (subscriber or requester). This looks very wasteful: we're just
moving a block around the network, there should be no need to reserialize
at every step.

The inefficiency comes from the time-warp typed API: we must give a `Block`,
rather than an arbitrary `ByteString`, because a value of the latter may not
be an encoding of a `Block`.

## Quick fix: unsafe send

A quick and dirty solution is to define and export
`sendRaw :: ConversationActions snd rcv m -> ByteString -> m ()`
and use this, rather than `send`, in cases where we already have the bytes,
for instance the handler for a get block request which reads the block from
a database. This change will help to fix [CSL-1695] about slow block retrieval
(the server's socket is starved because each chunk of the lazy bytestring takes
so long to produce).

## Retaining serialized input

But relaying without reserializing will require changes to the receive side
as well. We could give a raw receive function analagous to the raw send
function above
`recvRaw :: ConversationActions snd rcv m -> m ByteString`
in which the decoder of the `rcv` type is used to delimit the input and check
that it's well-formed, but not to produce the decoded value.

## Complication: composite message types

This unsafe API can work, but it's dodgy at best, even more so because the
input and output types of conversations are typically composite over the
actual data that we're interested in. As an example, blocks:

```Haskell
data MsgGetBlock = MsgBlock Block | MsgNoBlocks
```

To extract the serialized `Block` using
`recvRaw :: ConversationActions snd MsgGetBlock m -> m ByteString`
we have to make assumptions about the encoding of `MsgGetBlock`: take the
first byte and if it's `0`, then the `Block` is the rest of the `ByteString`.
GHC will not check these assumptions.

## A safer but more complicated approach

When decoding the network input, it should be possible to retain the
`ByteString` from which the thing was decoded, and some sort of proof or
evidence that this `ByteString` is a valid encoding. The typed API should
accept either the serializable value, or the serialization and the proof that
it's a valid encoding.

Comment: Duncan

> I don't see that this solves the problem. Take the example above of 
> 
>     data MsgGetBlock = MsgBlock Block | MsgNoBlocks
> 
> What we receive is one of these messages, and we want to decode it to find out which constructor it is (and in other examples we may need to decide other bits of the message) but we do not want to decode the block.
> 
> So getting the bytestring of the whole thing still isn't useful, it's the bytestring of the block we want. So the simplest approach is to use the CBOR-in-CBOR trick and have the `MsgBlock` message's binary encoding contain a bytestring, which itself is a further encoded thing. So this gives us an envelope/payload style thing.
> 
> That can be typed with some wrapper like:
> 
>     -- | A thing of type @a@, but still in encoded form.
>     newtype Payload a = Payload ByteString
> 
> And this can have a binary/bi instance that read the thing as a bytestring but does not decode it. So then we'd use:
> 
>     data MsgGetBlock = MsgBlock (Payload Block) | MsgNoBlocks
> 
> This trick does change the CBOR encoding, so it is not directly backwards compatible, but assuming we use the normal protocol versioning then that's ok.

Maybe something like this:

```Haskell
-- Give either a 'snd' or something that's known to be a serialization of it.
send :: ConversationActions snd rcv m -> Serialized snd -> m ()

-- Always produces the 'rcv' and the bytes from which it was deserialized,
-- but is not strict in the 'rcv' term so you can throw it away to avoid
-- extra work (building a merkle tree for example, which block deserialization
-- does).
recv :: ConversationActions snd rcv m -> m (Serialized rcv)

-- Constructors not exported.
-- Can only be constructed by 'decode' or 'encode'.
-- Relies on soundness of Serializable instance.
data Serialized t where
  Serialized :: Serializable t => ByteString -> Serialized t
  Deserialized :: Serializable t => t -> Serialized t
  Both :: Serializable t => ByteString -> t -> Serialized t
```

Comment: Philipp

> I like this approach.

```
decode :: Serializable t => ByteString -> Serialized t

encode :: Serializable t => t -> Serialized t

fromValue :: Serializable t => t -> Serialized t
fromValue = Deserialized

serialize :: Serialized t -> ByteString
serialize (Serialized bs) = bs
serialize (Deserialized t) = CBOR.serialise t
serialize (Both bs _) = bs
```

To deal with composites, we could have a type-level definition of how the
serialized thing is composed, i.e. a type family

```Haskell
-- A sum-of-products
type SerializedForm (it :: *) :: [[*]]

type instance SerializedForm MsgGetBlock =
  [ [ Word8 0, Block ] -- MsgBlock
  , [ Word8 1 ]        -- MsgNoBlocks
  ]
```

```Haskell
-- (if 'Fmap' were really a type)
type Decomposed = (Fmap . Fmap) Serialized SerializedForm

-- We can do surgery on a decomposed value. For MsgGetBlock:
--
--   case decompose msgGetBlock of
--     -- First disjunct: [ Word8 0, Block ] for MsgBlock constructor.
--     One (Cons _ block) -> Just block
--     -- Second disjunct: [ Word8 1 ] for MsgNoBlocks constructor.
--     Two Nil -> Nothing
--
decompose :: it -> SumOfProducts (Decomposed it)

-- Reserialize, using any already-serialized components to avoid extra work.
serialize :: SumOfProducts (Decomposed it) -> ByteString
```

This is just one approach I've been toying with. It's more complicated than
I'd like but maybe it will inspire some sort of compromise or all around better
solution.

Comment: Duncan

> Yeah, lets discuss the various options.

## Discussion

We need to talk about this and decide on a way forward. The quick and dirty
unsafe API could be a reasonable temporary solution but we really have to make
sure it's indeed temporary, as in software temporary solutions tend to become
permanent.

## Related work

[CSL-1399] makes big improvements in deserialization by removing validation
checks.

The proposed diffusion layer block cache will benefit from the work described
here. Serialized blocks could be kept in the cache to avoid the cost of
reserialization on every request.
