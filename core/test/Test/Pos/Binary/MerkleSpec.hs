module Test.Pos.Binary.MerkleSpec where

import           Universum
import           Test.Hspec (Spec, describe, it)
import           Test.QuickCheck (arbitrary)

import           Pos.Merkle
import           Pos.Binary.Merkle () -- Bi instance of `MerkleTree`
import           Pos.Arbitrary.Core () -- Arbitrary instance of `MerkleTree`
import           Test.Pos.Binary.Class.Core


spec :: Spec
spec = describe "Bi" $ do
    it "encodedSize Merkle" $ encodedSizeProp @(MerkleTree Int) arbitrary
    it "encodedListSize Merkle" $ encodedListSizeProp @(MerkleTree Int) arbitrary
