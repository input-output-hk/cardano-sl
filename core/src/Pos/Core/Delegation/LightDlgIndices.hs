module Pos.Core.Delegation.LightDlgIndices
       ( LightDlgIndices (..)
       , ProxySigLight
       , ProxySKLight
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint)
import           Serokell.Util (pairF)

import           Pos.Binary.Class (Bi (..))
import           Pos.Core.Slotting (EpochIndex)
import           Pos.Crypto (ProxySecretKey (..), ProxySignature)

-- Notice: light delegation was removed as part of CSL-1856 and should
-- be reworked later. Though some parts of it are left to support
-- backward compatibility.

-- | Pair of indices for light delegation PSK that define start and
-- end epoch of cert usage. Block is valid if its epoch index is
-- inside this range.
newtype LightDlgIndices =
    LightDlgIndices { getLightDlgIndices :: (EpochIndex, EpochIndex) }
    deriving (Show, Eq, Ord, Generic)

instance NFData LightDlgIndices

instance Buildable LightDlgIndices where
    build (LightDlgIndices p) = bprint pairF p

instance Bi LightDlgIndices where
    encode = encode . getLightDlgIndices
    decode = LightDlgIndices <$> decode

-- | Light delegation proxy signature, that holds a pair of epoch
-- indices.
type ProxySigLight a = ProxySignature LightDlgIndices a

-- | Same alias for the proxy secret key (see 'ProxySigLight').
type ProxySKLight = ProxySecretKey LightDlgIndices
