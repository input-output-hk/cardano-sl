module NTP.Packet
    ( NtpPacket (..)
    , mkCliNtpPacket
    ) where


import Control.Monad   (replicateM_)
import Data.Binary     (Binary (..))
import Data.Binary.Get (getInt8, getWord32be, getWord32host, getWord32le, getWord8)
import Data.Binary.Put (putWord32host, putWord8)
import Data.Time.Units (Microsecond, fromMicroseconds)
import Data.Word       (Word8)

data NtpPacket = NtpPacket
    { ntpParams :: Word8
    , ntpTime   :: Microsecond
    , ntpPoll   :: Int
    } deriving (Show)

-- | NTP timestamp is not the same as time past from start of UNIX epoch - differ in
-- 70 years
ntpTimestampDelta :: Integer
ntpTimestampDelta = 2208988800

instance Binary NtpPacket where
    put NtpPacket{..} = do
        putWord8 ntpParams

        -- since it's sent only by client, initialize remaining with zeros
        replicateM_ 3 $ putWord8 0
        replicateM_ 11 $ putWord32host 0

    get = do
        ntpParams    <- getWord8
        _            <- getWord8
        ntpPoll      <- fromIntegral <$> getInt8
        _            <- getWord8
        replicateM_ 9 $ getWord32host
        -- TODO: why getWord32host doesn't work here???
        _seconds     <- getWord32be
        _secondsFrac <- getWord32be
        let ntpTime = fromMicroseconds $
               (fromIntegral _seconds - ntpTimestampDelta) * 1000000
              + fromIntegral _secondsFrac `mod` 1000000
        return NtpPacket{..}

mkCliNtpPacket :: NtpPacket
mkCliNtpPacket =
    NtpPacket
    { ntpParams = 0x1b
    , ntpTime   = 0
    , ntpPoll   = 0
    }
