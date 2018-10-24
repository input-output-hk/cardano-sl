module Pos.Core.NetworkMagic
       ( NetworkMagic (..)
       , makeNetworkMagic
       ) where

import           Universum

import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import           Data.Serialize (getWord8, putWord8)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Crypto.Configuration (ProtocolMagic (..),
                     RequiresNetworkMagic (..), getProtocolMagic)
import           Pos.Util.Util (cerealError)


--------------------------------------------------------------------------------
-- NetworkMagic
--------------------------------------------------------------------------------

-- mhueschen: although it makes no sense for an identifier to have a sign, we're
-- opting to maintain consistency with `ProtocolMagicId`, rather than risk subtle
-- conversion bugs.
data NetworkMagic
    = NetworkMainOrStage
    | NetworkTestnet !Int32
    deriving (Show, Eq, Ord, Generic)

instance NFData NetworkMagic

instance Buildable NetworkMagic where
    build NetworkMainOrStage = "NetworkMainOrStage"
    build (NetworkTestnet n) = bprint ("NetworkTestnet ("%build%")") n

instance SafeCopy NetworkMagic where
    getCopy = contain $ getWord8 >>= \case
        0 -> pure NetworkMainOrStage
        1 -> NetworkTestnet <$> safeGet
        t -> cerealError $ "getCopy@NetworkMagic: couldn't read tag: " <> show t
    putCopy NetworkMainOrStage = contain $ putWord8 0
    putCopy (NetworkTestnet x) = contain $ putWord8 1 >> safePut x

makeNetworkMagic :: ProtocolMagic -> NetworkMagic
makeNetworkMagic pm = case getRequiresNetworkMagic pm of
    RequiresNoMagic -> NetworkMainOrStage
    RequiresMagic   -> NetworkTestnet (getProtocolMagic pm)
