-- | This module tests Binary instances.

module Test.Pos.BinarySpec
       ( spec
       ) where

import           Data.Bits                  (testBit)
import qualified Data.ByteString.Lazy       as BS
import           Data.ByteString.Lazy.Char8 (singleton)

import           Test.Hspec                 (Spec, anyErrorCall, describe, it, shouldBe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (..), Gen, choose, generate,
                                             suchThat)
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
            describe "1 byte" $ do
                it "serializing 0 takes 1 byte" $ B.biSize (B.TinyVarInt 0) `shouldBe` 1
                prop "0" $ binaryEncodeDecode (B.TinyVarInt 0)
                it "serializing 1 takes 1 byte" $ B.biSize (B.TinyVarInt 1) `shouldBe` 1
                prop "1" $ binaryEncodeDecode (B.TinyVarInt 1)
                it "serializing 0x75 takes 1 byte" $
                    B.biSize (B.TinyVarInt 0x75) `shouldBe` 1
                prop "0x75" $ binaryEncodeDecode (B.TinyVarInt 0x75)
                it "serializing 0x79 takes 1 byte" $
                    B.biSize (B.TinyVarInt 0x79) `shouldBe` 1
                prop "0x79" $ binaryEncodeDecode (B.TinyVarInt 0x79)
            describe "2 bytes" $ do
                it "serializing 0x80 takes 2 bytes" $
                    B.biSize (B.TinyVarInt 0x80) `shouldBe` 2
                prop "0x80" $ binaryEncodeDecode (B.TinyVarInt 0x80)
                it "serializing 0x81 takes 2 bytes" $
                    B.biSize (B.TinyVarInt 0x81) `shouldBe` 2
                prop "0x81" $ binaryEncodeDecode (B.TinyVarInt 0x81)
                it "serializing 0x82 takes 2 bytes" $
                    B.biSize (B.TinyVarInt 0x82) `shouldBe` 2
                prop "0x82" $ binaryEncodeDecode (B.TinyVarInt 0x82)
                it "serializing 0x100 takes 2 bytes" $
                    B.biSize (B.TinyVarInt 0x100) `shouldBe` 2
                prop "0x100" $ binaryEncodeDecode (B.TinyVarInt 0x100)
                it "serializing 0x1000 takes 2 bytes" $
                    B.biSize (B.TinyVarInt 0x1000) `shouldBe` 2
                prop "0x1000" $ binaryEncodeDecode (B.TinyVarInt 0x1000)
                let maxnum = 2^(14::Int)-1
                it "serializing (2^14)-1 takes 2 bytes" $
                    B.biSize (B.TinyVarInt maxnum) `shouldBe` 2
                prop "2^14-1" $ binaryEncodeDecode (B.TinyVarInt maxnum)
                prop "fails to serialize more than 2^14 - 1" $ do
                    n <-  generate $ B.TinyVarInt <$> choose (maxnum + 1, maxBound)
                    shouldThrowException B.encode anyErrorCall n
            describe "deserialize more than 2 bytes" $ do
                prop "fails to deserialize more than 2 bytes" $ do
                    let wordGen =
                            generate $
                                (arbitrary :: Gen Word8) `suchThat` (flip testBit 7)
                    w1 <- wordGen
                    w2 <- wordGen
                    let bs =
                            uncurry BS.append . bimap BS.singleton BS.singleton $ (w1, w2)
                    shouldThrowException (B.decode @B.TinyVarInt) anyErrorCall bs
            describe "deserialize ambiguous zero" $ do
                prop "fails to deserialize an ambiguous zero (i.e. @80 00@)" $ do
                    word8 <- generate $
                        (arbitrary :: Gen Word8) `suchThat` (flip testBit 7)
                    let zero = '\NUL'
                        bs = (BS.singleton word8) `BS.append` (singleton zero)
                    shouldThrowException (B.decode @B.TinyVarInt) anyErrorCall bs
