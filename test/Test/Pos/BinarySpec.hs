-- | This module tests Binary instances.

module Test.Pos.BinarySpec
       ( spec
       ) where

import           Data.Bits             (setBit)
import qualified Data.ByteString       as BS
import           Numeric               (showHex)

import           Test.Hspec            (Spec, anyErrorCall, describe, it, shouldBe,
                                        shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), choose, generate, suchThat)
import           Universum

import qualified Pos.Binary            as B

import           Test.Pos.Util         (binaryEncodeDecode, binaryTest,
                                        shouldThrowException)

spec :: Spec
spec = describe "Bi" $ do
    describe "Numbers" $ do
        unsignedVarIntSpec
        signedVarIntSpec
        fixedSizeIntSpec
        tinyVarIntSpec

unsignedVarIntSpec :: Spec
unsignedVarIntSpec = describe "UnsignedVarInt" $ do
    binaryTest @(B.UnsignedVarInt Int)
    binaryTest @(B.UnsignedVarInt Int64)
    binaryTest @(B.UnsignedVarInt Word)
    binaryTest @(B.UnsignedVarInt Word16)
    binaryTest @(B.UnsignedVarInt Word32)
    binaryTest @(B.UnsignedVarInt Word64)

signedVarIntSpec :: Spec
signedVarIntSpec = describe "SignedVarInt" $ do
    binaryTest @(B.SignedVarInt Int)
    binaryTest @(B.SignedVarInt Int64)

fixedSizeIntSpec :: Spec
fixedSizeIntSpec = describe "FixedSizeInt" $ do
    binaryTest @(B.FixedSizeInt Int)
    binaryTest @(B.FixedSizeInt Int64)

tinyVarIntSpec :: Spec
tinyVarIntSpec = describe "TinyVarInt" $ do
    let hex n = "0x" ++ showHex n ""
        test_roundtrip n =
            prop ("roundtrip " ++ hex n) $ binaryEncodeDecode (B.TinyVarInt n)
        plural b = show b ++ " byte" ++ if b > 1 then "s" else ""
        test_length n b =
            it ("serializing " ++ hex n ++ " takes " ++ plural b) $
                B.biSize (B.TinyVarInt n) `shouldBe` b
        maxnum = 2^(14::Int)-1

    describe "1 byte" $ do
        test_length 0x00 1 >> test_roundtrip 0x00
        test_length 0x01 1 >> test_roundtrip 0x01
        test_length 0x30 1 >> test_roundtrip 0x30
        test_length 0x75 1 >> test_roundtrip 0x75
        test_length 0x79 1 >> test_roundtrip 0x79

    describe "2 bytes" $ do
        test_length 0x80   2 >> test_roundtrip 0x80
        test_length 0x81   2 >> test_roundtrip 0x81
        test_length 0x82   2 >> test_roundtrip 0x82
        test_length 0x100  2 >> test_roundtrip 0x100
        test_length 0x1000 2 >> test_roundtrip 0x1000
        it "serializing 2^14-1 takes 2 bytes" $
            B.biSize (B.TinyVarInt maxnum) `shouldBe` 2
        prop "roundtrip 2^14-1" $
            binaryEncodeDecode (B.TinyVarInt maxnum)
        prop "fails to serialize more than 2^14-1" $ do
            n <- generate $ B.TinyVarInt <$> choose (maxnum + 1, maxBound)
            shouldThrowException B.encode anyErrorCall n

    describe "more than 2 bytes" $ do
        prop "can't completely deserialize 3+ arbitrary bytes" $ do
            bs <- generate $ arbitrary `suchThat` ((> 2) . length)
            B.decodeFull @B.TinyVarInt bs `shouldSatisfy` isLeft
        prop "can't deserialize a *varint* with length 3+" $ do
            len <- generate $ choose (3, 8)
            bytes <- generate $ map (`setBit` 7) <$>
                                  replicateM (len - 1) arbitrary
            let bs = BS.pack (bytes ++ [0x01])
            shouldThrowException (B.decodeOrFail @B.TinyVarInt) anyErrorCall bs
        prop "never generates more than 2 bytes" $ do
            n <- generate $ choose (0, maxnum)
            B.biSize (B.TinyVarInt n) `shouldSatisfy` (<= 2)

-- CSL-1122: restore this test
--    describe "normal varint followed by unrelated bytes" $ do
--        prop "1 byte" $ do
--            n <- generate $ choose (0x00, 0x79)
--            bs <- generate arbitrary
--            B.decode (B.encode (B.TinyVarInt n) <> bs)
--                `shouldBe` B.TinyVarInt n
--        prop "2 bytes" $ do
--            n <- generate $ choose (0x80, maxnum)
--            bs <- generate arbitrary
--            B.decode (B.encode (B.TinyVarInt n) <> bs)
--                `shouldBe` B.TinyVarInt n

    describe "inefficiently encoded data" $ do
        prop "fails to deserialize ambiguous numbers (e.g. 0x80 0x00)" $ do
            word8 <- generate $ flip setBit 7 <$> arbitrary
            let bs = BS.pack [word8, 0x00]
            shouldThrowException (B.decodeOrFail @B.TinyVarInt) anyErrorCall bs
