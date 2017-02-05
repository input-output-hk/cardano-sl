-- | This module tests Binary instances.

module Test.Pos.BinarySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Binary            as B

import           Test.Pos.Util         (binaryEncodeDecode, binaryTest)

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
            -- TODO: check that these actually take 1 byte
            describe "1 byte" $ do
                prop "0" $ binaryEncodeDecode (B.TinyVarInt 0)
                prop "1" $ binaryEncodeDecode (B.TinyVarInt 1)
                prop "0x75" $ binaryEncodeDecode (B.TinyVarInt 0x75)
                prop "0x79" $ binaryEncodeDecode (B.TinyVarInt 0x79)
            -- TODO: check that these actually take 2 bytes
            describe "2 bytes" $ do
                prop "0x80" $ binaryEncodeDecode (B.TinyVarInt 0x80)
                prop "0x81" $ binaryEncodeDecode (B.TinyVarInt 0x81)
                prop "0x82" $ binaryEncodeDecode (B.TinyVarInt 0x82)
                prop "0x100" $ binaryEncodeDecode (B.TinyVarInt 0x100)
                prop "0x1000" $ binaryEncodeDecode (B.TinyVarInt 0x1000)
                let maxnum = 2^(14::Int)-1
                prop "2^14-1" $ binaryEncodeDecode (B.TinyVarInt maxnum)
            -- TODO: check that trying to serialize more than 2^14-1 fails
            -- TODO: check that deserializing more than 2 bytes fails
            -- TODO: check that deserializing an ambiguous 0 (i.e. @80 00@)
            --       fails as well
