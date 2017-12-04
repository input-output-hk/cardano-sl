-- | Core delegation types.

module Pos.Core.Delegation.Types
       (
         ProxySigLight
       , ProxySKLight
       , ProxySigHeavy
       , ProxySKHeavy

       , DlgPayload (..)
       ) where

import           Universum

import           Data.Default (Default (def))
import qualified Data.Text.Buildable
import           Formatting (bprint, int, (%))
import           Serokell.Util (listJson)

import           Pos.Core.Slotting.Types (EpochIndex)
import           Pos.Crypto (ProxySecretKey (..), ProxySignature)

----------------------------------------------------------------------------
-- Proxy signatures and signing keys
----------------------------------------------------------------------------

-- Notice: light delegation was removed as part of CSL-1856 and should
-- be reworked later. Though some parts of it are left to support
-- backward compatibility.

-- | Proxy signature, that holds a pair of epoch indices. Block is
-- valid if its epoch index is inside this range.
type ProxySigLight a = ProxySignature (EpochIndex, EpochIndex) a

-- | Same alias for the proxy secret key (see 'ProxySigLight').
type ProxySKLight = ProxySecretKey (EpochIndex, EpochIndex)


-- | Simple proxy signature without ttl/epoch index
-- constraints. 'EpochIndex' inside is needed for replay attack
-- prevention (it should match epoch of the block psk is announced
-- in).
type ProxySigHeavy a = ProxySignature EpochIndex a

-- | Heavy delegation psk.
type ProxySKHeavy = ProxySecretKey EpochIndex

----------------------------------------------------------------------------
-- Payload
----------------------------------------------------------------------------

-- Consider making this a set.
-- | 'DlgPayload' is put into 'MainBlock' and consists of a list of
-- heavyweight proxy signing keys. There must be no duplicates
-- (comparing by issuer) in this list. The order of PSKs doesn't
-- matter, as it's checked for cycles after bulk application. So it's
-- technically a set.
newtype DlgPayload = UnsafeDlgPayload
    { getDlgPayload :: [ProxySKHeavy]
    } deriving (Show, Eq, Generic, NFData)

instance Default DlgPayload where
    def = UnsafeDlgPayload []

instance Buildable DlgPayload where
    build (UnsafeDlgPayload psks) =
        bprint
            ("proxy signing keys ("%int%" items): "%listJson%"\n")
            (length psks) psks
