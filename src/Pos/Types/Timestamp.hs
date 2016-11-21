-- | Slotting functionality.

module Pos.Types.Timestamp
       ( Timestamp (..)
       , timestampF
       ) where

import           Control.Monad          (fail)
import           Control.TimeWarp.Timed (Microsecond)
import           Data.Binary            (Binary (get, put))
import           Data.MessagePack       (MessagePack (fromObject, toObject))
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as Buildable

import           Formatting             (Format, build)
import           Prelude                (Read (..), Show (..))
import           Universum              hiding (show)


-- | Timestamp is a number which represents some point in time. It is
-- used in MonadSlots and its meaning is up to implementation of this
-- type class. The only necessary knowledge is that difference between
-- timestamps is microsecond. Hence underlying type is Microsecond.
newtype Timestamp = Timestamp
    { getTimestamp :: Microsecond
    } deriving (Num, Eq, Ord, Enum, Real, Integral, Typeable)

instance Show Timestamp where
  show = show . getTimestamp

instance Read Timestamp where
  readsPrec i = fmap (first Timestamp) . readsPrec i

instance Buildable Timestamp where
    build = Buildable.build @Integer . fromIntegral

instance Binary Timestamp where
  get = fromInteger <$> get
  put = put . toInteger

-- ugly instance, but MessagePack doesn't export instance for Integer
-- and currently we need this instance only for testing
instance MessagePack Timestamp where
  fromObject = fromObject >=> fmap fromInteger . read'
    where
      read' :: (Monad m, Read a) => [Char] -> m a
      read' = either fail return . readEither
  toObject = toObject . show . toInteger

-- | Specialized formatter for 'Timestamp' data type.
timestampF :: Format r (Timestamp -> r)
timestampF = build
