module Cardano.Wallet.Kernel.DB.HdWallet.Derivation (
      deriveIndex
    , HardeningMode(..)
    ) where

import           Universum

import           Cardano.Crypto.Wallet.Types (DerivationIndex)
import           Pos.Crypto.HD (firstHardened)

data HardeningMode = SoftDerivation
                   -- ^ Generates indexes in the range (0, maxBound @Word32)
                   | HardDerivation
                   -- ^ Generates indexes in the range (0x8000000, maxBound @Word32)

-- | Derives a new '
deriveIndex :: Monad m
            => ((DerivationIndex, DerivationIndex) -> m DerivationIndex)
            -- ^ A monadic computation which can pick a 'DerivationIndex' out
            -- of a range.
            -> (DerivationIndex -> a)
            -- How to build the final type out of the picked 'DerivationIndex'.
            -> HardeningMode
            -- ^ How we want to derive this index (@soft@ vs @hard@)
            -> m a
deriveIndex pickRange mkA hardeningMode =
    let range = case hardeningMode of
                     SoftDerivation -> (0, firstHardened - 1)
                     HardDerivation -> (firstHardened, maxBound)
    in mkA <$> pickRange range


