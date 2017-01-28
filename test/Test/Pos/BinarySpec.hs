-- | This module tests Binary instances.

module Test.Pos.BinarySpec
       ( spec
       ) where

import           Test.Hspec    (Spec, describe)
import           Universum

import qualified Pos.Binary    as B
import           Pos.Types.Types (Coin)

import           Test.Pos.Util (binaryTest)

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
        describe "Coin" $ do
            binaryTest @Coin
