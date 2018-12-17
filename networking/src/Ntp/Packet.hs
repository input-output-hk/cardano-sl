{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}

module Ntp.Packet
    ( NtpPacket (..)
    , ntpPacketSize
    , mkNtpPacket
    , NtpOffset (..)
    , clockOffsetPure
    , clockOffset
    , realMcsToNtp
    , ntpToRealMcs
    ) where


import           Control.Monad (replicateM_)
import           Data.Binary (Binary (..))
import           Data.Binary.Get (getInt8, getWord32be, getWord8, skip)
import           Data.Binary.Put (putInt8, putWord32be, putWord8)
import           Data.Int (Int8)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units (Microsecond, TimeUnit, fromMicroseconds,
                     toMicroseconds)
import           Data.Word (Word32, Word8)

data NtpPacket = NtpPacket
    { ntpParams       :: Word8        -- ^ some magic parameters
    , ntpPoll         :: Int8         -- ^ poll delay between requests
    , ntpOriginTime   :: Microsecond  -- ^ when server sent reply
    , ntpReceivedTime :: Microsecond  -- ^ when server got request
    , ntpTransmitTime :: Microsecond  -- ^ when client sent request
    } deriving (Show, Eq)

instance Binary NtpPacket where
    put NtpPacket{..} = do
        putWord8 ntpParams
        putWord8 0
        putInt8 ntpPoll
        putWord8 0

        replicateM_ 5 $ putWord32be 0

        let (osec, ofrac) = realMcsToNtp ntpOriginTime
        putWord32be osec
        putWord32be ofrac

        let (rsec, rfrac) = realMcsToNtp ntpReceivedTime
        putWord32be rsec
        putWord32be rfrac

        let (tsec, tfrac) = realMcsToNtp ntpTransmitTime
        putWord32be tsec
        putWord32be tfrac

    get = do
        ntpParams <- getWord8
        _         <- getWord8
        ntpPoll   <- getInt8
        _         <- getWord8

        -- skip 5 @'Word32'@ words
        skip 20

        ntpOriginTime   <- getTimestamp
        ntpReceivedTime <- getTimestamp
        ntpTransmitTime <- getTimestamp
        return NtpPacket{..}
      where
        getTimestamp = ntpToRealMcs <$> getWord32be <*> getWord32be

-- |
-- NTP timestamp start in 1.1.1900, i.e. 70 years before UNIX epoch.
-- references:
--  * https://tools.ietf.org/html/rfc5905#section-6
--  * https://tools.ietf.org/html/rfc5905#appendix-A.4
ntpTimestampDelta :: Integer
ntpTimestampDelta = 2208988800

-- |
-- We only need first 48 bytes of a packet:
-- reference: https://tools.ietf.org/html/rfc5905#section-7.3
ntpPacketSize :: Int
ntpPacketSize = 48

-- |
-- For pairs @(x, y) :: (Word32, Word32)@ with @y \`mod\` 4294 == 0@ it is
-- be right inverse of @'realMsgToNtp'@.   In general it is not injective (for that
-- we'd need to use @'Picosecond'@ instead of @'Microsecond'@).
ntpToRealMcs :: Word32 -> Word32 -> Microsecond
ntpToRealMcs sec frac =
    let -- microseconds
        secMicro :: Integer
        secMicro = (fromIntegral sec - ntpTimestampDelta) * 1000000
        -- We divide 1 second into 2 ^ 32 parts, giving 2.3283064365386963e-10
        -- as the quantum. A picosecond is 10e-12 of a second, so this is 232
        -- picoseconds or `1/4294` of a millisecond.
        -- ref: https://tools.ietf.org/html/rfc5905#section-6
        fracMicro :: Integer
        fracMicro = (fromIntegral frac) `div` 4294
    in fromMicroseconds $ secMicro + fracMicro

-- |
-- It is a partial function, since @Microsecond ~ Integer@; it is well defined
-- for:
-- @
--  x < 2085978496 = (maxBound @Word32 * 1000000) - ntpTimestampDelta + 1`
-- @
-- (in microseconds; this is roughly 66 years, so we're fine untill 2036).
realMcsToNtp :: Microsecond -> (Word32, Word32)
realMcsToNtp (toMicroseconds -> mcs) =
    let (sec, frac) = divMod mcs 1000000
    in  ( fromIntegral $ sec + ntpTimestampDelta
        , fromIntegral $ frac * 4294)

-- |
-- Smart constructor for @'NtpPacket'@.
mkNtpPacket :: IO NtpPacket
mkNtpPacket = do
    let ntpParams       = 0x1b
        ntpPoll         = 0
        ntpOriginTime   = 0
        ntpReceivedTime = 0
    ntpTransmitTime <- getCurrentTime
    return NtpPacket{..}

-- |
-- @'NtpOffset'@ is the difference between NTP time and local time.
newtype NtpOffset = NtpOffset { getNtpOffset :: Microsecond }
    deriving (Enum, Eq, Integral, Num, Ord, Real, Show, TimeUnit)

clockOffsetPure :: NtpPacket -> Microsecond -> NtpOffset
clockOffsetPure NtpPacket{..} localTime = NtpOffset
    $ (ntpReceivedTime - ntpOriginTime + ntpTransmitTime - localTime)
      `div` 2

-- |
-- Compute clock offset unless the NTP packet was requested more than the given
-- timeout.
clockOffset
    :: Microsecond
    -- ^ @'ntpResponseTimeout'@, ignore responses which come after it passed.
    -> NtpPacket
    -> IO (Maybe NtpOffset)
clockOffset respTimeout packet = do
    time <- getCurrentTime
    let isLate = time - ntpOriginTime packet >= respTimeout
    if isLate
        then return Nothing
        else return $ Just $ clockOffsetPure packet time

-- |
-- Helper function to get current time in @Microsecond@.
getCurrentTime :: IO Microsecond
getCurrentTime = fromMicroseconds . round . ( * 1000000) <$> getPOSIXTime
