# Wallet layer

This module here is an abstraction layer which would allow us to mock responses from the wallet
and that would fit into the current work with the new data layer.

Important:

- We feel that the right insertion point for this is at the
  boundary between each servant handlers (& the BListener interface) and the
  persistence layer. Therefore, more than a `WalletDBLayer` we are calling
  this a "WalletLayer";

- The new data layer already introduced the concepts of `PassiveWallet` (a
  wallet which does not have a `DiffusionLayer` available, so it
  cannot send transactions) and an `ActiveWallet` (which already has) so it
  makes sense to piggyback on those.

The plan is to modify the current (opaque) implementation of a `PassiveWallet`
with an interface similar to the following:

```haskell
data PassiveWalletLayer m = PassiveWalletLayer {
    applyBlock :: ...
 ,  rollbackBlock :: ...
 ,  getAddresses :: WalletId -> m [WalletAddress]
 ,  ...
}

data ActiveWalletLayer m = ActiveWalletLayer {
    passiveLayer :: PassiveWalletLayer m
  , pay :: Payment -> m ()
  ...
}
```

Mind that we still need a `DiffusionLayer` to send transactions to the network,
so a subset of the wallet operations will live in the `ActiveWalletLayer` data type
rather than in the `PassiveWalletLayer` (as an example the `pay` operation
is provided with a possible type signature).

Being parametrised over an `m` means pure mocks can run in `Identity`
and other usage can allow for `IO` or any other monad. This means that,
practically-speaking, a servant handler would look like this:

```haskell
getAddresses :: ActiveWalletLayer -> ServantT ...
getAddresses awl = do
  let pwl = getPassiveLayer awl
  lift <$> getAddresses pwl
```

For the "new" Handlers we need to explicitly pass the `ActiveWalletLayer`
around due to the initialisation of the diffusion layer, which happens after
we setup the monadic context. For the `LegacyHandlers`, I _think_ this data
layer is available earlier, and so we could embed the active wallet directly
into the monad, and don't need to pass it as an explicit argument to the servant handlers.

In practical terms, this means we will end up with three _concrete_ implementations of `PassiveWalletLayer`: 
- Legacy - one for the _old_ data layer (which is currently what's being used for both V0 & V1)
- Kernel - the one for the _new_ data layer 
- QuickCheck - the one for mocking purposes

