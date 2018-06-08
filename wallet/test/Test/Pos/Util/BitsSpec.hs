{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Util.BitsSpec where

import           Universum

import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), (===))
import           Test.QuickCheck.Gen (suchThat)

import           Pos.Util.Bits (Word11, fromBits, toBits)


spec :: Spec
spec = do
    prop "(Word8) toBits . fromBits = pure" $
        \(wrds :: [Word8]) -> (fromBits . toBits) wrds === pure wrds

    prop "(Word11) toBits . fromBits = pure" $
        \(wrds :: [Word11]) -> (fromBits . toBits) wrds === pure wrds


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
