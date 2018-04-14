module Test.Pos.Binary.Core.AddressSpec
    ( spec
    ) where

import           Universum
import           Test.Hspec (Spec, describe, it)
import           Test.QuickCheck (arbitrary)

import           Pos.Arbitrary.Txp () -- Arbitrary instances
import           Pos.Core
import           Pos.Data.Attributes
import           Pos.Binary.Core () -- Bi instances
import           Test.Pos.Binary.Class.Core


spec :: Spec
spec =
    describe "Bi" $ do
        it "encodedSize AddrType" $ encodedSizeProp @AddrType arbitrary
        it "encodedListSize AddrType" $ encodedListSizeProp @AddrType arbitrary

        it "encodedSize AddrSpendingData" $ encodedSizeProp @AddrSpendingData arbitrary
        it "encodedListSize AddrSpendingData" $ encodedListSizeProp @AddrSpendingData arbitrary

        it "encodedSize AddrStakeDistribution" $ encodedSizeProp @AddrStakeDistribution arbitrary
        it "encodedListSize AddrStakeDistribution" $ encodedListSizeProp @AddrStakeDistribution arbitrary

        it "encodedSize (Attributes AddrAttributes)" $ encodedSizeProp @(Attributes AddrAttributes) arbitrary
        it "encodedListSize (Attributes AddrAttributes)" $ encodedListSizeProp @(Attributes AddrAttributes) arbitrary

        it "encodedSize Address" $ encodedSizePropGE @Address arbitrary
        it "encodedListSize Address" $ encodedListSizePropGE @Address arbitrary
