{-# LANGUAGE TypeApplications #-}

-- | "Pos.Merkle" specification

module Test.Pos.MerkleSpec
       ( spec
       ) where

import           Data.Binary           (Binary)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (===))
import           Universum

import           Pos.Merkle            (mkMerkleTree, mtSize)

spec :: Spec
spec = describe "Merkle" $ do
    prop
        "toList . mkMerkleTree === id"
        (generateAndFoldProp @Int)
    prop
        "size . mkMerkleTree === length"
        (sizeProp @Int)

generateAndFoldProp :: (Eq a, Show a, Binary a) => [a] -> Property
generateAndFoldProp xs = maybe [] toList (mkMerkleTree xs) === xs

sizeProp :: (Binary a) => [a] -> Property
sizeProp xs = maybe 0 mtSize (mkMerkleTree xs) === fromIntegral (length xs)
