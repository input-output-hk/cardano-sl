{-# LANGUAGE UndecidableInstances #-}

-- | Common things used in `Pos.Crypto.Arbitrary` and `Pos.Util.Arbitrary`

module Pos.Util.TimeWarp
       ( NetworkAddress
       , localhost

       , currentTime
       , mcs
       , ms
       , sec
       , minute
       , hour
       ) where

import           Control.Monad.Trans (MonadIO)
import           Data.ByteString     (ByteString)
import           Data.Time.Units     (Microsecond)
import           Data.Time.Units     (fromMicroseconds)
import           Data.Word           (Word16)
import           Mockable            (realTime)
import           Universum

-- | @"127.0.0.1"@.
localhost :: ByteString
localhost = "127.0.0.1"

-- | Full node address.
type NetworkAddress = (ByteString, Word16)

-- | Temporal solution
currentTime :: MonadIO m => m Microsecond
currentTime = realTime

-- | Converts a specified time to `Microsecond`.
mcs, ms, sec, minute, hour :: Int -> Microsecond
mcs    = fromMicroseconds . fromIntegral
ms     = fromMicroseconds . fromIntegral . (*) 1000
sec    = fromMicroseconds . fromIntegral . (*) 1000000
minute = fromMicroseconds . fromIntegral . (*) 60000000
hour   = fromMicroseconds . fromIntegral . (*) 3600000000
