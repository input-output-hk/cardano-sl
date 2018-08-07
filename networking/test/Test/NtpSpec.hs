{-# LANGUAGE TypeApplications #-}

module Test.NtpSpec
    ( spec
    ) where

import           Data.Binary (decodeOrFail, encode)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Data.Word (Word32)

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Gen, counterexample, sized,
                     suchThat, (.&&.), (===))

import           Ntp.Packet (NtpOffset (..), NtpPacket (..), clockOffsetPure,
                     ntpToRealMcs, realMcsToNtp)

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

data NtpPacketWithOffset = NtpPacketWithOffset
    { npoNtpPacket       :: NtpPacket
    , npoOffset          :: NtpOffset
    , npoDestinationTime :: Microsecond
    }
    deriving (Show)

genMicro :: Gen Microsecond
genMicro = fromMicroseconds <$> arbitrary

newtype ArbitraryNtpPacket = ArbitraryNtpPacket NtpPacket
    deriving (Show, Eq)

instance Arbitrary ArbitraryNtpPacket where
    arbitrary = do
        ntpParams <- arbitrary
        ntpPoll <- arbitrary
        ntpOriginTime <- genMicro
        ntpReceivedTime <- suchThat genMicro (>= ntpOriginTime)
        ntpTransmitTime <- suchThat genMicro (>= ntpReceivedTime)
        return $ ArbitraryNtpPacket $ NtpPacket {..}

-- An arbitrary instance which generates a packet with a given offset, with
-- ideal symmetric trip time.
instance Arbitrary NtpPacketWithOffset where
    arbitrary = sized $ \offset -> do
        let drift :: Microsecond
            drift = fromMicroseconds $ fromIntegral offset
        ntpParams <- arbitrary
        ntpPoll <- arbitrary
        ntpOriginTime <- genMicro
        tripTime <- genMicro
        let ntpReceivedTime = ntpOriginTime + tripTime + drift
        ntpTransmitTime <- suchThat genMicro (>= ntpReceivedTime)
        let npoDestinationTime = ntpTransmitTime + tripTime - drift
        return $ NtpPacketWithOffset
            { npoNtpPacket = NtpPacket {..}
            , npoOffset = NtpOffset drift
            , npoDestinationTime = npoDestinationTime
            }

newtype NtpTime = NtpTime (Word32, Word32)
    deriving Show

-- Generate arbitrary pairs of @'Word32'@.  The ntp fractions correspond to 232
-- picoseconds (so 1msc correspond to @1000000 `div` 232 = 4310@).  Since we
-- operate with micorseconds we generated them only up this resolution.
instance Arbitrary NtpTime where
    arbitrary = do
        sec <- arbitrary
        frac <- suchThat arbitrary (\x -> x > 0 && x < (maxBound @Word32 `div` 1000000) * 232)
        return $ NtpTime (sec, frac * (1000000 `div` 232) - 1)

newtype NtpMicrosecond = NtpMicrosecond Microsecond
    deriving Show

-- Generate NtpMicrosecond which must be smaller than
-- @'maxBound' \@Word32 - 2200898800@ (we substract 70 years in seconds).
instance Arbitrary NtpMicrosecond where
    arbitrary = do
        x <- suchThat arbitrary (\y -> y < (fromIntegral $ maxBound @Word32) - 2208988800)
        return $ NtpMicrosecond $ fromMicroseconds $ x

spec :: Spec
spec = describe "NtpClient" $ do
    describe "clockOffsetPure" $ do
        prop "should return clock offset" $ \(NtpPacketWithOffset {..}) ->
            let offset = clockOffsetPure npoNtpPacket npoDestinationTime
            in npoOffset === offset
    describe "realMcsToNtp" $ do
        prop "should be right inverse of ntpToRealMcs" $ \(NtpMicrosecond x) ->
            x === uncurry ntpToRealMcs (realMcsToNtp x)
        prop "should be left inverse of ntpToRealMcs"  $ \(NtpTime x@(sec, frac)) ->
            let (sec', frac') = realMcsToNtp (uncurry ntpToRealMcs x)
            -- Each npt fraction unit correspond to 232 picoseconds, there are
            -- 4310 of them in a millisecond.
            in sec === sec' .&&. frac `div` 4310 === frac' `div` 4310
    describe "NptPacket" $ do
        prop "should serialize and deserialize"  $ \(ArbitraryNtpPacket ntpPacket) ->
            let bs = encode ntpPacket
            in do
                case decodeOrFail bs of
                    Left (_, _, err)     ->
                        counterexample (show ntpPacket ++ " " ++ show err) False
                    Right (_, _, packet) ->
                        ntpPacket === packet
