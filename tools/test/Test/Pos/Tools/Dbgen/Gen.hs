module Test.Pos.Tools.Dbgen.Gen
       ( genAccountSpec
       , genAddressRange
       , genDistributionAmount
       , genFakeTxsHistory
       , genFakeUtxoCoinDistribution
       , genGenSpec
       , genWalletSpec
       ) where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Tools.Dbgen.Lib (AccountSpec (..), AddressRange (..),
                     DistributionAmount (..), FakeTxsHistory (..),
                     FakeUtxoCoinDistribution (..), GenSpec (..),
                     WalletSpec (..))

genGenSpec :: Gen GenSpec
genGenSpec = GenSpec <$> genLimitedInteger <*> genWalletSpec

genWalletSpec :: Gen WalletSpec
genWalletSpec =
    WalletSpec
        <$> genLimitedInteger
        <*> genAccountSpec
        <*> genFakeUtxoCoinDistribution
        <*> genFakeTxsHistory

genAccountSpec :: Gen AccountSpec
genAccountSpec = AccountSpec <$> genLimitedInteger

genAddressRange :: Gen AddressRange
genAddressRange = AddressRange <$> genInteger

genDistributionAmount :: Gen DistributionAmount
genDistributionAmount = DistributionAmount <$> genInteger

genFakeUtxoCoinDistribution :: Gen FakeUtxoCoinDistribution
genFakeUtxoCoinDistribution = Gen.choice [ pure NoDistribution
                                         , RangeDistribution
                                               <$> genAddressRange
                                               <*> genDistributionAmount
                                         ]

genFakeTxsHistory :: Gen FakeTxsHistory
genFakeTxsHistory = Gen.choice [ pure NoHistory
                               , SimpleTxsHistory <$> genInteger <*> genInt
                               ]


genLimitedInteger :: Gen Integer
genLimitedInteger = Gen.integral (Range.constant 0 10) :: Gen Integer

genInteger :: Gen Integer
genInteger = Gen.integral (Range.constant 0 2000000) :: Gen Integer

genInt :: Gen Int
genInt = Gen.int Range.constantBounded
