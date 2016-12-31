{-# LANGUAGE ViewPatterns #-}

module NTP.Packet
    ( NtpPacket (..)
    , mkCliNtpPacket
    , evalClockOffset
    ) where


import Control.Lens        (each, (^..))
import Control.Monad       (liftM4, replicateM_)
import Control.Monad.Trans (MonadIO (..))
import Data.Binary         (Binary (..))
import Data.Binary.Get     (getInt8, getWord32be, getWord8)
import Data.Binary.Put     (putWord32be, putWord8)
import Data.Time.Units     (Microsecond, fromMicroseconds, toMicroseconds)
import Data.Word           (Word32, Word8)

import NTP.Util (getCurrentTime)

import Debug.Trace

data NtpPacket = NtpPacket
    { ntpParams       :: Word8        -- some magic parameters
    , ntpPoll         :: Int          -- poll delay between requests
    , ntpOriginTime   :: Microsecond  -- when client sent request
    , ntpReceivedTime :: Microsecond  -- when server got request
    , ntpTransmitTime :: Microsecond  -- when server sent reply
    } deriving (Show)

-- | NTP timestamp is not the same as time past from start of UNIX epoch - differ in
-- 70 years
ntpTimestampDelta :: Integer
ntpTimestampDelta = 2208988800

power2of32 :: Integer
power2of32 = 4294967296

ntpToRealMcs :: Word32 -> Word32 -> Microsecond
ntpToRealMcs integerSec fracSec = fromMicroseconds $
       (fromIntegral integerSec - ntpTimestampDelta) * 1000000
      + ((fromIntegral fracSec * 1000000) `div` power2of32)

realMcsToNtp :: Microsecond -> (Word32, Word32)
realMcsToNtp (toMicroseconds -> mcs) =
    let integerSec = (mcs `div` 1000000) + ntpTimestampDelta
        -- not correct:
        fracSec    = ((mcs * power2of32) `div` 1000000)
    in  (fromIntegral integerSec, fromIntegral fracSec)


instance Binary NtpPacket where
    put NtpPacket{..} = do
        putWord8 ntpParams

        -- since it's sent only by client, initialize only `originTime`
        replicateM_ 3 $ putWord8 0
        replicateM_ 9 $ putWord32be 0
        mapM_ putWord32be $ realMcsToNtp ntpTransmitTime ^.. each

    get = do
        ntpParams <- getWord8
        _         <- getWord8
        ntpPoll   <- fromIntegral <$> getInt8
        _         <- getWord8
        replicateM_ 5 getWord32be
        _origTmS  <- getWord32be
        _origTmF  <- getWord32be
        _rxTmS    <- getWord32be
        _rxTmF    <- getWord32be
        _txTmS    <- getWord32be
        _txTmF    <- getWord32be
        let ntpOriginTime   = ntpToRealMcs _origTmS _origTmF
        let ntpReceivedTime = ntpToRealMcs _rxTmS _rxTmF
        let ntpTransmitTime = ntpToRealMcs _txTmS _txTmF
        return NtpPacket{..}

mkCliNtpPacket :: MonadIO m => m NtpPacket
mkCliNtpPacket = do
    let ntpParams       = 0x1b
        ntpPoll         = 0
        ntpOriginTime   = 0
        ntpReceivedTime = 0
    ntpTransmitTime    <- getCurrentTime
    return NtpPacket {..}

evalClockOffset :: MonadIO m => NtpPacket -> m Microsecond
evalClockOffset packet = do
    localTime <- getCurrentTime
    return $ liftM4 (\t1 t2 t3 t4 -> (t2 - t1 + t3 - t4) `div` 2)
                                -- ^ formula of clock offset
             ntpOriginTime ntpReceivedTime ntpTransmitTime (pure localTime)
           $ packet
