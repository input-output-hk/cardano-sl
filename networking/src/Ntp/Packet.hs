{-# LANGUAGE ViewPatterns #-}

module Ntp.Packet
    ( NtpPacket (..)
    , ntpPacketSize
    , mkCliNtpPacket
    , evalClockOffset
    ) where


import           Control.Lens (each, (^..))
import           Control.Monad (replicateM_)
import           Control.Monad.Trans (MonadIO (..))
import           Data.Binary (Binary (..))
import           Data.Binary.Get (getInt8, getWord32be, getWord8)
import           Data.Binary.Put (putWord32be, putWord8)
import           Data.Time.Units (Microsecond, fromMicroseconds, toMicroseconds)
import           Data.Word (Word32, Word8)

import           Ntp.Util (getCurrentTime)

data NtpPacket = NtpPacket
    { ntpParams       :: Word8        -- some magic parameters
    , ntpPoll         :: Int          -- poll delay between requests
    , ntpOriginTime   :: Microsecond  -- when server sent reply
    , ntpReceivedTime :: Microsecond  -- when server got request
    , ntpTransmitTime :: Microsecond  -- when client sent request
    } deriving (Show)

-- | NTP timestamp is not the same as time past from start of UNIX epoch — they
-- differ by 70 years.
ntpTimestampDelta :: Integer
ntpTimestampDelta = 2208988800

ntpPacketSize :: Int
ntpPacketSize = 48

exp2'32 :: Integer
exp2'32 = 4294967296

ntpToRealMcs :: Word32 -> Word32 -> Microsecond
ntpToRealMcs integerSec fracSec = fromMicroseconds $
       (fromIntegral integerSec - ntpTimestampDelta) * 1000000
      + ((fromIntegral fracSec * 1000000) `div` exp2'32)

realMcsToNtp :: Microsecond -> (Word32, Word32)
realMcsToNtp (toMicroseconds -> mcs) =
    let integerSec = (mcs `div` 1000000) + ntpTimestampDelta
        fracSec    = ((mcs * exp2'32) `div` 1000000)
    in  (fromIntegral integerSec, fromIntegral fracSec)

instance Binary NtpPacket where
    put NtpPacket{..} = do
        putWord8 ntpParams

        -- since it's sent only by client, initialize only `transmitTime`
        replicateM_ 3 $ putWord8 0
        replicateM_ 9 $ putWord32be 0
        mapM_ putWord32be $ realMcsToNtp ntpTransmitTime ^.. each

    get = do
        ntpParams <- getWord8
        _         <- getWord8
        ntpPoll   <- fromIntegral <$> getInt8
        _         <- getWord8
        replicateM_ 5 getWord32be
        ntpOriginTime   <- getTimestamp
        ntpReceivedTime <- getTimestamp
        ntpTransmitTime <- getTimestamp
        return NtpPacket{..}
      where
        getTimestamp = ntpToRealMcs <$> getWord32be <*> getWord32be

mkCliNtpPacket :: MonadIO m => m NtpPacket
mkCliNtpPacket = do
    let ntpParams       = 0x1b
        ntpPoll         = 0
        ntpOriginTime   = 0
        ntpReceivedTime = 0
    ntpTransmitTime    <- getCurrentTime
    return NtpPacket{..}

evalClockOffset :: MonadIO m => NtpPacket -> m Microsecond
evalClockOffset NtpPacket{..} = do
    localTime <- getCurrentTime
    -- use formula of clock offset
    return $ (ntpReceivedTime - ntpOriginTime + ntpTransmitTime - localTime) `div` 2
