{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Util.BitsSpec where

import           Universum hiding (one)

import           Data.List (last)
import           Test.Hspec (Spec, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), (===))
import           Test.QuickCheck.Gen (suchThat)

import           Pos.Util.Bits (Word11, fromBits, one, toBits, zero)


spec :: Spec
spec = do
    prop "(Word8) toBits . fromBits = pure" $
        \(ws :: [Word8]) -> (fromBits . toBits) ws === pure ws

    prop "(Word11) toBits . fromBits = pure" $
        \(ws :: [Word11]) -> (fromBits . toBits) ws === pure ws

    prop "(Word8) x % 2 == 0 => last (toBits [x]) == 0" $
        \(w :: Word8) -> last (toBits [w]) == if w `mod` 2 == 0 then zero else one

    prop "(Word8) length . toBits == (8 *) . length" $
        \(ws :: [Word8]) -> (length . toBits) ws == ((8 *) . length) ws

    prop "(Word11) length . toBits == (11 *) . length" $
        \(ws :: [Word11]) -> (length . toBits) ws == ((11 *) . length) ws

    it "toBits (42 :: Word8) = [0,0,1,0,1,0,1,0]" $
        toBits ([42 :: Word8]) `shouldBe` [zero, zero, one, zero, one, zero, one, zero]

    it "toBits (14 :: Word11) = [0,0,0,0,0,0,0,1,1,1,0]" $
        toBits ([14 :: Word11]) `shouldBe` [zero, zero, zero, zero, zero, zero, zero, one, one, one, zero]


instance Arbitrary Word11 where
    arbitrary =
        fromIntegral <$> suchThat (arbitrary @Int) predicate
      where
        predicate x =
            let
                l = fromIntegral (minBound @Word11)
                h = fromIntegral (maxBound @Word11)
            in
                x >= l && x <= h
