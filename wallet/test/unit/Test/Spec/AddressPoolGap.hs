module Test.Spec.AddressPoolGap (spec) where

import           Universum

import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap,
                     mkAddressPoolGap)

import           Test.Hspec (Spec, describe, it)
import           Test.Pos.Core.Arbitrary ()
import           Test.QuickCheck (Property, label, property)

spec :: Spec
spec = describe "AddressPoolGap" $ do
    it "mkAddressPoolGap" $ property prop_mkAddressPoolGap


-- | It proves that we:
-- 1. can create 'AddressPoolGap' from any number within the min-max range,
-- 2. cannot create 'AddressPoolGap' from any other number.
prop_mkAddressPoolGap
    :: Int
    -> Property
prop_mkAddressPoolGap gap =
    case mkAddressPoolGap (fromIntegral gap) of
        Left _  ->
            label "gap outsides boundaries" $ not (isWithinBound gap)
        Right _ ->
            label "gap within boundaries" $ isWithinBound gap
  where
    isWithinBound g =
        g >= fromIntegral (minBound @AddressPoolGap) &&
        g <= fromIntegral (maxBound @AddressPoolGap)
