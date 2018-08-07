module Pos.Core.Update.BlockVersion
       ( BlockVersion (..)
       , HasBlockVersion (..)
       ) where

import           Universum

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, shown)
import qualified Formatting.Buildable as Buildable
import qualified Prelude

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Util.Some (Some, liftLensSome)

-- | Communication protocol version.
data BlockVersion = BlockVersion
    { bvMajor :: !Word16
    , bvMinor :: !Word16
    , bvAlt   :: !Word8
    } deriving (Eq, Generic, Ord, Typeable)

instance Show BlockVersion where
    show BlockVersion {..} =
        intercalate "." [show bvMajor, show bvMinor, show bvAlt]

instance Buildable BlockVersion where
    build = bprint shown

instance Hashable BlockVersion

instance NFData BlockVersion

class HasBlockVersion a where
    blockVersionL :: Lens' a BlockVersion

instance HasBlockVersion (Some HasBlockVersion) where
    blockVersionL = liftLensSome blockVersionL

deriveJSON defaultOptions ''BlockVersion

deriveSimpleBi ''BlockVersion [
    Cons 'BlockVersion [
        Field [| bvMajor :: Word16 |],
        Field [| bvMinor :: Word16 |],
        Field [| bvAlt   :: Word8  |]
    ]]

deriveSafeCopySimple 0 'base ''BlockVersion
