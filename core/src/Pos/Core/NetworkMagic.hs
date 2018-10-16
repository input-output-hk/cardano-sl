module Pos.Core.NetworkMagic
       ( NetworkMagic (..)
       , makeNetworkMagic
       ) where

import           Universum

import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import           Data.Serialize (getWord8, putWord8)
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))

import           Pos.Crypto.Configuration (ProtocolMagic (..), RequiresNetworkMagic (..),
                                           getProtocolMagic)
import           Pos.Util.Util (cerealError)


--------------------------------------------------------------------------------
-- NetworkMagic
--------------------------------------------------------------------------------

-- mhueschen: although it makes no sense for an identifier to have a sign, we're
-- opting to maintain consistency with `ProtocolMagicId`, rather than risk subtle
-- conversion bugs.
data NetworkMagic
    = NMNothing
    | NMJust !Int32
    deriving (Show, Eq, Ord, Generic)

instance NFData NetworkMagic

instance Buildable NetworkMagic where
    build NMNothing  = "NMNothing"
    build (NMJust n) = bprint ("NMJust ("%build%")") n

instance SafeCopy NetworkMagic where
    getCopy = contain $ getWord8 >>= \case
        0 -> pure NMNothing
        1 -> NMJust <$> safeGet
        t -> cerealError $ "getCopy@NetworkMagic: couldn't read tag: " <> show t
    putCopy NMNothing  = contain $ putWord8 0
    putCopy (NMJust x) = contain $ putWord8 1 >> safePut x

makeNetworkMagic :: ProtocolMagic -> NetworkMagic
makeNetworkMagic pm = case getRequiresNetworkMagic pm of
    NMMustBeNothing -> NMNothing
    NMMustBeJust    -> NMJust (getProtocolMagic pm)
