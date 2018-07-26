module Test.Infrastructure.Genesis (
    GenesisValues(..)
  , genesisValues
  , overestimate
  ) where

import           Universum

import qualified Data.List (head)

import           UTxO.Context
import           UTxO.DSL

import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (estimateCardanoFee)
import           Pos.Core (TxSizeLinear, getCoin)

{-------------------------------------------------------------------------------
  Convenient access to some values in the Cardano genesis block
-------------------------------------------------------------------------------}

-- | Convenient access to some values in the Cardano genesis block
data GenesisValues h a = GenesisValues {
      -- | Initial balance of rich actor 0
      initBalR0  :: Value

      -- | Initial balance of rich actor 1
    , initBalR1  :: Value

      -- | Initial balance of poor actor 0
    , initBalP0  :: Value

      -- | Initial balance of poor actor 1
    , initBalP1  :: Value

      -- | Initial UTxO entry of rich actor 0
    , initUtxoR0 :: (Input h a, Output h a)

      -- | Initial UTxO entry of rich actor 1
    , initUtxoR1 :: (Input h a, Output h a)

      -- | Initial UTxO entry of poor actor 0
    , initUtxoP0 :: (Input h a, Output h a)

      -- | Initial UTxO entry of poor actor 1
    , initUtxoP1 :: (Input h a, Output h a)

      -- | Address of rich actor 0
    , r0         :: Addr

      -- | Address of rich actor 1
    , r1         :: Addr

      -- | Address of rich actor 2
    , r2         :: Addr

      -- | Address of poor actor 0
    , p0         :: Addr

      -- | Address of poor actor 1
    , p1         :: Addr

      -- | The bootstrap transaction
    , boot       :: Transaction h Addr

      -- | Fee policy
    , txFee      :: Int -> [Value] -> Value
    }

-- | Compute genesis values from the bootstrap transaction
genesisValues :: forall h. Hash h Addr
              => TxSizeLinear
              -> Transaction h Addr
              -> GenesisValues h Addr
genesisValues txSizeLinear boot = GenesisValues{..}
  where
    initUtxoR0 = initUtxoFor r0
    initUtxoR1 = initUtxoFor r1
    initUtxoP0 = initUtxoFor p0
    initUtxoP1 = initUtxoFor p1

    initBalR0 = outVal . snd $ initUtxoR0
    initBalR1 = outVal . snd $ initUtxoR1
    initBalP0 = outVal . snd $ initUtxoP0
    initBalP1 = outVal . snd $ initUtxoP1

    r0 = Addr (IxRich 0) 0
    r1 = Addr (IxRich 1) 0
    r2 = Addr (IxRich 2) 0
    p0 = Addr (IxPoor 0) 0
    p1 = Addr (IxPoor 1) 0

    txFee = estimateCardanoFee  txSizeLinear

    initUtxoFor :: Addr -> (Input h Addr, Output h Addr)
    initUtxoFor a = unsafeHead
                  . utxoToList
                  . utxoRestrictToAddr (== a)
                  $ trUtxo boot

-- | Over-estimate the total fee, by assuming the resulting transaction is
--   as large as possible for the given number of inputs and outputs.
overestimate :: (Int -> [Value] -> Value) -> Int -> Int -> Value
overestimate getFee ins outs = getFee ins (replicate outs (getCoin maxBound))


{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

unsafeHead :: [a] -> a
unsafeHead = Data.List.head
