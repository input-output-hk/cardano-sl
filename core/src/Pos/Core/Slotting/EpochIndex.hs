{-# LANGUAGE RecordWildCards #-}

module Pos.Core.Slotting.EpochIndex
       ( EpochIndex (..)
       , HasEpochIndex (..)
       , isBootstrapEra
       ) where

import           Universum

import           Control.Lens (choosing)
import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import           Data.Ix (Ix)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, int, (%))
import qualified Formatting.Buildable as Buildable
import           Servant.API (FromHttpApiData)
import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..))

import           Pos.Binary.Class (Bi (..))
import           Pos.Util.Json.Canonical ()
import           Pos.Util.Some (Some, liftLensSome)

-- | Index of epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Ix, Integral, Real, Generic, Hashable, Bounded, Typeable, NFData)

instance Buildable EpochIndex where
    build = bprint ("#"%int)

instance Bi EpochIndex where
    encode (EpochIndex epoch) = encode epoch
    decode = EpochIndex <$> decode

deriving instance FromHttpApiData EpochIndex

-- Note that it will be encoded as string, because 'EpochIndex'
-- doesn't necessary fit into JS number.
instance Monad m => ToJSON m EpochIndex where
    toJSON = toJSON . getEpochIndex

deriving instance Aeson.FromJSON EpochIndex

deriving instance Aeson.ToJSON EpochIndex

class HasEpochIndex a where
    epochIndexL :: Lens' a EpochIndex

instance HasEpochIndex (Some HasEpochIndex) where
    epochIndexL = liftLensSome epochIndexL

instance (HasEpochIndex a, HasEpochIndex b) =>
         HasEpochIndex (Either a b) where
    epochIndexL = choosing epochIndexL epochIndexL



instance ReportSchemaErrors m => FromJSON m EpochIndex where
    fromJSON = fmap EpochIndex . fromJSON

-- | Bootstrap era is ongoing until stakes are unlocked. The reward era starts
-- from the epoch specified as the epoch that unlocks stakes:
--
-- @
--                     [unlock stake epoch]
--                             /
-- Epoch: ...  E-3  E-2  E-1   E+0  E+1  E+2  E+3  ...
--        ------------------ | -----------------------
--             Bootstrap era   Reward era
-- @
--
--
-- | This function has been stubbed out to always return True, since
-- this codebase will not be decentralized.
isBootstrapEra
    :: EpochIndex -- ^ Unlock stake epoch
    -> EpochIndex -- ^ Epoch in question (for which we determine whether it
                  --                      belongs to the bootstrap era).
    -> Bool
isBootstrapEra _unlockStakeEpoch _epoch = True

deriveSafeCopySimple 0 'base ''EpochIndex
