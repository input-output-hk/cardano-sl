-- | This module tests Binary instances.

module Test.Pos.BinarySpec
       ( spec
       ) where

import           Universum

import           Data.Fixed (Nano)
import           Data.Time.Units (Microsecond, Millisecond)
import           Serokell.Data.Memory.Units (Byte)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

import           Test.Pos.Helpers (binaryTest)

spec :: Spec
spec = describe "Bi" $ modifyMaxSuccess (const 10000) $ do
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
