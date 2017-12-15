-- | This module tests Binary instances.

module Test.Pos.BinarySpec
       ( spec
       ) where

import           Data.Bits (setBit)
import qualified Data.ByteString as BS
import           Data.Fixed (Nano)
import           Data.Time.Units (Microsecond, Millisecond)
import           Numeric (showHex)
import           Serokell.Data.Memory.Units (Byte)

import           Test.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldSatisfy, xdescribe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), choose, generate, suchThat)
import           Universum

import qualified Pos.Binary as B

import           Test.Pos.Helpers (binaryEncodeDecode, binaryTest, shouldThrowException)

spec :: Spec
spec = describe "Bi" $ modifyMaxSuccess (const 10000) $ do
    describe "Numbers" $ do
        unsignedVarIntSpec
        signedVarIntSpec
        fixedSizeIntSpec
        tinyVarIntSpec
    describe "Primitive instances" $ do
        binaryTest @()
        binaryTest @Bool
        binaryTest @Char
        binaryTest @Integer
        binaryTest @Word
        binaryTest @Word8
        binaryTest @Word16
        binaryTest @Word32
        binaryTest @Word64
        binaryTest @Int
        binaryTest @Float
        binaryTest @Int32
        binaryTest @Int64
        binaryTest @Nano
        binaryTest @Millisecond
        binaryTest @Microsecond
        binaryTest @Byte
        binaryTest @(Map Int Int)
        binaryTest @(HashMap Int Int)
        binaryTest @(Set Int)
        binaryTest @(HashSet Int)

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
    binaryTest @(B.FixedSizeInt Word)

tinyVarIntSpec :: Spec
tinyVarIntSpec = describe "TinyVarInt" $ do
    binaryTest @(B.TinyVarInt)

    let hex n = "0x" ++ showHex n ""
        test_roundtrip n =
            prop ("roundtrip " ++ hex n) $ binaryEncodeDecode (B.TinyVarInt n)
        plural b = show b ++ " byte" ++ if b > 1 then "s" else ""
        test_length n b =
            it ("serializing " ++ hex n ++ " takes " ++ plural b) $
                B.biSize (B.TinyVarInt n) `shouldBe` b
        maxnum = 2^(14::Int)-1

    describe "1 or 2 bytes" $ do
        test_length 0x00 1 >> test_roundtrip 0x00
        test_length 0x01 1 >> test_roundtrip 0x01
        test_length 0x30 2 >> test_roundtrip 0x30
        test_length 0x75 2 >> test_roundtrip 0x75
        test_length 0x79 2 >> test_roundtrip 0x79

    describe "2 or 3 bytes" $ do
        test_length 0x80   2 >> test_roundtrip 0x80
        test_length 0x81   2 >> test_roundtrip 0x81
        test_length 0x82   2 >> test_roundtrip 0x82
        test_length 0x100  3 >> test_roundtrip 0x100
        test_length 0x1000 3 >> test_roundtrip 0x1000
        it "serializing 2^14-1 takes 3 bytes" $
            B.biSize (B.TinyVarInt maxnum) `shouldBe` 3
        prop "roundtrip 2^14-1" $
            binaryEncodeDecode (B.TinyVarInt maxnum)
        xdescribe "Needs to be clarified after CBOR migration" $ do
          prop "fails to serialize more than 2^14-1" $ do
            n <- generate $ B.TinyVarInt <$> choose (maxnum + 1, maxBound)
            shouldThrowException B.encode anyErrorCall n

    xdescribe "Needs to be clarified after CBOR migration" $ do
      describe "more than 2 bytes" $ do
          prop "can't completely deserialize 3+ arbitrary bytes" $ do
              bs <- generate $ arbitrary `suchThat` ((> 2) . length)
              B.decodeFull @B.TinyVarInt bs `shouldSatisfy` isLeft
          prop "can't deserialize a *varint* with length 3+" $ do
              len <- generate $ choose (3, 8)
              bytes <- generate $ map (`setBit` 7) <$>
                                    replicateM (len - 1) arbitrary
              let bs = BS.pack (bytes ++ [0x01])
              shouldThrowException (B.deserialize' @B.TinyVarInt) anyErrorCall bs
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

    xdescribe "Needs to be clarified after CBOR migration" $ do
      describe "inefficiently encoded data" $ do
          prop "fails to deserialize ambiguous numbers (e.g. 0x80 0x00)" $ do
              word8 <- generate $ flip setBit 7 <$> arbitrary
              let bs = BS.pack [word8, 0x00]
              shouldThrowException (B.deserialize' @B.TinyVarInt) anyErrorCall bs
