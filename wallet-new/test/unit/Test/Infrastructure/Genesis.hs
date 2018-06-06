module Test.Infrastructure.Genesis (
    GenesisValues(..)
  , genesisValues
  ) where

import           Universum

import           Data.Maybe (fromJust)

import           UTxO.Context
import           UTxO.DSL

import           Pos.Core (HasGenesisBlockVersionData)
import           Test.Infrastructure.Generator (estimateCardanoFee)

{-------------------------------------------------------------------------------
  Convenient access to some values in the Cardano genesis block
-------------------------------------------------------------------------------}

-- | Convenient access to some values in the Cardano genesis block
data GenesisValues h = GenesisValues {
      -- | Initial balance of rich actor 0
      initR0   :: Value

      -- | Address of rich actor 0
    , r0       :: Addr

      -- | Address of rich actor 1
    , r1       :: Addr

      -- | Address of rich actor 2
    , r2       :: Addr

      -- | Hash of the bootstrap transaction
    , hashBoot :: h (Transaction h Addr)

      -- | Fee policy
    , txFee :: Int -> [Value] -> Value
    }

-- | Compute genesis values from the bootstrap transaction
genesisValues :: (HasGenesisBlockVersionData, Hash h Addr) => Transaction h Addr -> GenesisValues h
genesisValues boot@Transaction{..} = GenesisValues{..}
  where
    initR0 = unsafeHead [val | Output a val <- trOuts, a == r0]

      --11137499999752500

    r0 = Addr (IxRich 0) 0
    r1 = Addr (IxRich 1) 0
    r2 = Addr (IxRich 2) 0

    hashBoot = hash boot

    txFee = estimateCardanoFee

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

unsafeHead :: [a] -> a
unsafeHead = fromJust . head
