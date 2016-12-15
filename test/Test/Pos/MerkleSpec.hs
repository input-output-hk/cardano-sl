-- | "Pos.Merkle" specification

module Test.Pos.MerkleSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (===))
import           Universum

import           Pos.Binary            (Bi, decode, encode)
import           Pos.Merkle            (mkMerkleTree, mtSize)

spec :: Spec
spec = describe "Merkle" $ do
    prop
        "toList . mkMerkleTree === id"
        (generateAndFoldProp @Int)
    prop
        "size . mkMerkleTree === length"
        (sizeProp @Int)
    prop
        "bi instance, encode === decode^-1"
        (biProp @Int)

generateAndFoldProp :: (Eq a, Show a, Bi a) => [a] -> Property
generateAndFoldProp xs = toList (mkMerkleTree xs) === xs

sizeProp :: (Bi a) => [a] -> Property
sizeProp xs = mtSize (mkMerkleTree xs) === fromIntegral (length xs)

biProp :: (Eq a, Show a, Bi a) => [a] -> Property
biProp xs = let m = mkMerkleTree xs in decode (encode m) === m
