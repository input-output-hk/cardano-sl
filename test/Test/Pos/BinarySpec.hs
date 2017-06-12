-- | This module tests Binary instances.

module Test.Pos.BinarySpec
       ( spec
       ) where

import           Data.Bits                  (setBit)
import qualified Data.ByteString.Lazy       as BS
import           Numeric                    (showHex)
import           Serokell.Data.Memory.Units (Byte)

import           Test.Hspec                 (Expectation, Spec, anyErrorCall, describe,
                                             it, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (..), choose, generate, suchThat)
import           Universum

import qualified Pos.Binary                 as B

import           Test.Pos.Util              (binaryEncodeDecode, binaryTest,
                                             shouldThrowException)

spec :: Spec
spec = describe "Bi" $ do
    describe "Numbers" $ do
        describe "UnsignedVarInt" $ do
            binaryTest @(B.UnsignedVarInt Int)
            binaryTest @(B.UnsignedVarInt Int64)
            binaryTest @(B.UnsignedVarInt Word)
            binaryTest @(B.UnsignedVarInt Word16)
            binaryTest @(B.UnsignedVarInt Word32)
            binaryTest @(B.UnsignedVarInt Word64)
        describe "SignedVarInt" $ do
            binaryTest @(B.SignedVarInt Int)
            binaryTest @(B.SignedVarInt Int64)
        describe "FixedSizeInt" $ do
            binaryTest @(B.FixedSizeInt Int)
            binaryTest @(B.FixedSizeInt Int64)
        describe "TinyVarInt" $ do
            let hex n = "0x" ++ showHex n ""
                test_roundtrip n =
                    prop ("roundtrip " ++ hex n) $ binaryEncodeDecode (B.TinyVarInt n)
                plural b = show b ++ " byte" ++ if b == 1 then "s" else ""
                test_length n b =
                    it ("serializing " ++ hex n ++ " takes " ++ plural b) $
                        binarySizeShouldBe n b
            describe "1 byte" $ do
                test_length 0 1
                test_roundtrip 0
                test_length 1 1
                test_roundtrip 1
                test_length 0x75 1
                test_roundtrip 0x75
                test_length 0x79 1
                test_roundtrip 0x79
            describe "2 bytes" $ do
                test_length 0x80 2
                test_roundtrip 0x80
                test_length 0x81 2
                test_roundtrip 0x81
                test_length 0x82 2
                test_roundtrip 0x82
                test_length 0x100 2
                test_roundtrip 0x100
                test_length 0x1000 2
                test_roundtrip 0x1000
                let maxnum = 2^(14::Int)-1
                it "serializing (2^14)-1 takes 2 bytes" $ binarySizeShouldBe maxnum 2
                prop "roundtrip 2^14-1" $ binaryEncodeDecode (B.TinyVarInt maxnum)
                prop "fails to serialize more than 2^14 - 1" $ do
                    n <-  generate $ B.TinyVarInt <$> choose (maxnum + 1, maxBound)
                    shouldThrowException B.encode anyErrorCall n
            describe "deserialize more than 2 bytes" $ do
                prop "fails to deserialize more than 2 bytes" $ do
                    bs <- generate $ arbitrary `suchThat` ((> 2) . length)
                    B.decodeFull @B.TinyVarInt bs `shouldSatisfy` isLeft
            describe "deserialize inefficiently encoded data" $ do
                prop "fails to deserialize more than 2^14 - 1" $ do
                    let wordGen = generate $ flip setBit 7 <$> arbitrary
                    w1 <- wordGen
                    w2 <- wordGen
                    let bs = BS.pack [w1, w2]
                    shouldThrowException (B.decode @B.TinyVarInt) anyErrorCall bs
                prop "fails to deserialize an ambiguous zero (i.e. @80 00@)" $ do
                    word8 <- generate $ flip setBit 7 <$> arbitrary
                    let bs = BS.pack [word8, 0x00]
                    shouldThrowException (B.decode @B.TinyVarInt) anyErrorCall bs

binarySizeShouldBe :: Word16 -> Byte -> Expectation
binarySizeShouldBe num expectedSize =
    B.biSize (B.TinyVarInt num) `shouldBe` expectedSize
