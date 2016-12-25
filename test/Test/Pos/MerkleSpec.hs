-- | "Pos.Merkle" specification

module Test.Pos.MerkleSpec
       ( spec
       ) where

import           Data.SafeCopy         (SafeCopy)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (===))
import           Universum

import           Pos.Binary            (Bi, decode, encode)
import           Pos.Merkle            (mkMerkleTree, mtSize)
import           Test.Pos.Util         (binaryEncodeDecode, safeCopyEncodeDecode)

spec :: Spec
spec = describe "Merkle" $ do
    prop
        "toList . mkMerkleTree === id"
        (generateAndFoldProp @Int)
    prop
        "size . mkMerkleTree === length"
        (sizeProp @Int)
    prop
        "bi instance, encode === decode⁻¹"
        (biProp @Int)

generateAndFoldProp :: (Eq a, Show a, Bi a) => [a] -> Property
generateAndFoldProp xs = toList (mkMerkleTree xs) === xs

sizeProp :: (Bi a) => [a] -> Property
sizeProp xs = mtSize (mkMerkleTree xs) === fromIntegral (length xs)

biProp :: (Eq a, Show a, Bi a) => [a] -> Property
biProp xs = let m = mkMerkleTree xs in binaryEncodeDecode m

safeProp :: (Eq a, Show a, Bi a, SafeCopy a) => [a] -> Property
safeProp xs = let m = mkMerkleTree xs in safeCopyEncodeDecode m
