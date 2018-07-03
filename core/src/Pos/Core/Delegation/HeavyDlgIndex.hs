module Pos.Core.Delegation.HeavyDlgIndex
       ( HeavyDlgIndex (..)
       , ProxySigHeavy
       , ProxySKHeavy
       , ProxySKBlockInfo
       ) where

import           Universum

import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import qualified Data.Text.Buildable
import           Formatting (bprint, build)

import           Pos.Binary.Class (Bi (..))
import           Pos.Core.Slotting (EpochIndex)
import           Pos.Crypto (ProxySecretKey (..), ProxySignature, PublicKey)

-- | Witness for heavy delegation signature -- epoch in which
-- certificate starts being active. It is needed for replay attack
-- prevention (index should match epoch of the block PSK is announced
-- in).
newtype HeavyDlgIndex =
    HeavyDlgIndex { getHeavyDlgIndex :: EpochIndex }
    deriving (Show, Eq, Ord, Generic)

instance NFData HeavyDlgIndex
instance Hashable HeavyDlgIndex

instance Buildable HeavyDlgIndex where
    build (HeavyDlgIndex i) = bprint build i

instance Bi HeavyDlgIndex where
    encode = encode . getHeavyDlgIndex
    decode = HeavyDlgIndex <$> decode

instance SafeCopy HeavyDlgIndex where
    getCopy = contain $ HeavyDlgIndex <$> safeGet
    putCopy x = contain $ safePut $ getHeavyDlgIndex x

-- | Simple proxy signature without ttl/epoch index constraints.
type ProxySigHeavy a = ProxySignature HeavyDlgIndex a

-- | Heavy delegation PSK.
type ProxySKHeavy = ProxySecretKey HeavyDlgIndex

-- | Heavyweight PSK with real leader public key (because heavyweight
-- psks have redelegation feature, so pskIssuerPk hPsk /= leader in
-- general case). This is used to create a block header only.
type ProxySKBlockInfo = Maybe (ProxySKHeavy, PublicKey)
