-- | Core delegation types.
module Pos.Core.Delegation
       (
       -- * Heavy/light delegation
         LightDlgIndices (..)
       , ProxySigLight
       , ProxySKLight
       , HeavyDlgIndex (..)
       , ProxySigHeavy
       , ProxySKHeavy

       -- * Payload/proof
       , DlgPayload (..)
       , DlgProof
       , mkDlgProof
       ) where

import           Universum

import           Data.Default (Default (def))
import qualified Data.Text.Buildable
import           Formatting (bprint, build, int, (%))
import           Serokell.Util (allDistinct, listJson, pairF)

import           Pos.Binary.Class (Bi)
import           Pos.Core.Slotting.Types (EpochIndex)
import           Pos.Crypto (HasCryptoConfiguration, Hash, ProxySecretKey (..), ProxySignature,
                             hash)
import           Pos.Util.Verification (PVerifiable (..), PVerifiableSub (..), pverFail)

----------------------------------------------------------------------------
-- Proxy signatures and signing keys
----------------------------------------------------------------------------

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

-- | Light delegation proxy signature, that holds a pair of epoch
-- indices.
type ProxySigLight a = ProxySignature LightDlgIndices a

-- | Same alias for the proxy secret key (see 'ProxySigLight').
type ProxySKLight = ProxySecretKey LightDlgIndices


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

-- | Simple proxy signature without ttl/epoch index constraints.
type ProxySigHeavy a = ProxySignature HeavyDlgIndex a

-- | Heavy delegation PSK.
type ProxySKHeavy = ProxySecretKey HeavyDlgIndex

----------------------------------------------------------------------------
-- Payload/proof
----------------------------------------------------------------------------

-- | 'DlgPayload' is put into 'MainBlock' and is a set of heavyweight
-- proxy signing keys. List of psk issuers should be unique also.
newtype DlgPayload = UncheckedDlgPayload
    { getDlgPayload :: [ProxySKHeavy]
    } deriving (Show, Eq, Generic, NFData)

instance Default DlgPayload where
    def = UncheckedDlgPayload mempty

instance Buildable DlgPayload where
    build (UncheckedDlgPayload psks) =
        bprint
            ("proxy signing keys ("%int%" items): "%listJson%"\n")
            (length psks) psks

instance (HasCryptoConfiguration, Bi HeavyDlgIndex) => PVerifiable DlgPayload where
    pverifySelf (UncheckedDlgPayload proxySKs) =
        unless (allDistinct $ map pskIssuerPk proxySKs) $
            pverFail "Some of block's PSKs have the same issuer, which is prohibited"
    pverifyFields (UncheckedDlgPayload certs) =
        map (\(c,(i::Integer)) -> PVerifiableSub ("dlgPayload#" <> show i) c)
            (certs `zip` [0..])

-- | Proof of delegation payload.
type DlgProof = Hash DlgPayload

-- | Creates 'DlgProof' out of delegation payload.
mkDlgProof :: Bi DlgPayload => DlgPayload -> DlgProof
mkDlgProof = hash
