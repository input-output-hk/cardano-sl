module Pos.Util.Binary
       (
       -- * SafeCopy
         getCopyBinary
       , putCopyBinary
       , Raw

       -- * Binary serialization
       , AsBinary (..)
       , AsBinaryClass (..)
       , fromBinaryM
       ) where
import           Data.SafeCopy    (Contained, SafeCopy (..), contain, safeGet, safePut)
import qualified Data.Serialize   as Cereal (Get, Put)
import           Universum

import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi

-- | A wrapper over 'ByteString' for adding type safety to
-- 'Pos.Crypto.Pki.encryptRaw' and friends.
newtype Raw = Raw ByteString
    deriving (Bi, Eq, Ord, Show, Typeable)

-- | A helper for "Data.SafeCopy" that creates 'putCopy' given a 'Binary'
-- instance.
putCopyBinary :: Bi a => a -> Contained Cereal.Put
putCopyBinary x = contain $ safePut (Bi.encode x)

-- | A helper for "Data.SafeCopy" that creates 'getCopy' given a 'Binary'
-- instance.
getCopyBinary :: Bi a => String -> Contained (Cereal.Get a)
getCopyBinary typeName = contain $ do
    bs <- safeGet
    case Bi.decodeFull bs of
        Left err -> fail ("getCopy@" ++ typeName ++ ": " ++ err)
        Right x  -> return x
        --
-- | See `Pos.Crypto.SerTypes` for details on this types

newtype AsBinary a = AsBinary
    { getAsBinary :: ByteString
    } deriving (Show, Eq, Ord, Hashable)

instance SafeCopy (AsBinary a) where
    getCopy = contain $ AsBinary <$> safeGet
    putCopy = contain . safePut . getAsBinary

class AsBinaryClass a where
  asBinary :: a -> AsBinary a
  fromBinary :: AsBinary a -> Either String a

fromBinaryM :: (AsBinaryClass a, MonadFail m) => AsBinary a -> m a
fromBinaryM = either fail return . fromBinary

