-- | "Pos.Merkle" specification

module Test.Pos.MerkleSpec
       ( spec
       ) where

import           Universum

import           Data.SafeCopy (SafeCopy)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Property, (===))

import           Pos.Binary (Bi)
import           Pos.Merkle (mkMerkleTree)
import           Pos.SafeCopy ()
import           Test.Pos.Helpers (binaryEncodeDecode, safeCopyEncodeDecode)

spec :: Spec
spec = describe "Merkle" $ do
    prop
        "toList . mkMerkleTree === id"
        (generateAndFoldProp @Int32)
    prop
        "size . mkMerkleTree === length"
        (sizeProp @Int32)
    prop
        "bi instance, encode === decode^-1"
        (biProp @Int32)
    prop
        "safeCopy instance, get === put^-1"
        (safeProp @Int32)

generateAndFoldProp :: (Eq a, Show a, Bi a) => [a] -> Property
generateAndFoldProp xs = toList (mkMerkleTree xs) === xs

sizeProp :: (Bi a) => [a] -> Property
sizeProp xs = length (mkMerkleTree xs) === fromIntegral (length xs)

biProp :: (Eq a, Show a, Bi a) => [a] -> Property
biProp xs = let m = mkMerkleTree xs in binaryEncodeDecode m

safeProp :: (Eq a, Typeable a, Show a, Bi a, SafeCopy a) => [a] -> Property
safeProp xs = let m = mkMerkleTree xs in safeCopyEncodeDecode m
