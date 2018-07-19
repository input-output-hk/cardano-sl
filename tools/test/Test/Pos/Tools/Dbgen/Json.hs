{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Pos.Tools.Dbgen.Json
       ( tests
       ) where

import           Universum

import           Hedgehog (Property, checkParallel, checkSequential)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Tools.Dbgen.Lib

import           Test.Pos.Tools.Dbgen.Gen (genAccountSpec, genAddressRange,
                     genDistributionAmount, genFakeTxsHistory,
                     genFakeUtxoCoinDistribution, genGenSpec, genWalletSpec)
import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestJSON)
import           Test.Pos.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonBuildable, roundTripsAesonShow)

--------------------------------------------------------------------------------
-- GenSpec
--------------------------------------------------------------------------------

golden_GenSpec :: Property
golden_GenSpec =
    goldenTestJSON exampleGenSpec "test/golden/GenSpec"

roundTripGenSpec :: Property
roundTripGenSpec =
    eachOf 1000 genGenSpec roundTripsAesonShow

--------------------------------------------------------------------------------
-- WalletSpec
--------------------------------------------------------------------------------

golden_WalletSpec :: Property
golden_WalletSpec =
    goldenTestJSON exampleWalletSpec "test/golden/WalletSpec"

roundTripWalletSpec :: Property
roundTripWalletSpec =
    eachOf 1000 genWalletSpec roundTripsAesonShow

--------------------------------------------------------------------------------
-- AccountSpec
--------------------------------------------------------------------------------

golden_AccountSpec :: Property
golden_AccountSpec =
    goldenTestJSON exampleAccountSpec "test/golden/AccountSpec"

roundTripAccountSpec :: Property
roundTripAccountSpec =
    eachOf 1000 genAccountSpec roundTripsAesonShow

--------------------------------------------------------------------------------
-- AddressRange
--------------------------------------------------------------------------------

golden_AddressRange :: Property
golden_AddressRange =
    goldenTestJSON exampleAddressRange "test/golden/AddressRange"

roundTripAddressRange :: Property
roundTripAddressRange =
    eachOf 1000 genAddressRange roundTripsAesonShow

--------------------------------------------------------------------------------
-- DistributionAmount
--------------------------------------------------------------------------------

golden_DistributionAmount :: Property
golden_DistributionAmount =
    goldenTestJSON exampleDistributionAmount "test/golden/DistributionAmount"

roundTripDistributionAmount :: Property
roundTripDistributionAmount =
    eachOf 1000 genDistributionAmount roundTripsAesonShow

--------------------------------------------------------------------------------
-- FakeUtxoCoinDistribution
--------------------------------------------------------------------------------

golden_FakeUtxoCoinDistribution :: Property
golden_FakeUtxoCoinDistribution =
    goldenTestJSON exampleFakeUtxoCoinDistribution "test/golden/FakeUtxoCoinDistribution"

roundTripFakeUtxoCoinDistribution :: Property
roundTripFakeUtxoCoinDistribution =
    eachOf 1000 genFakeUtxoCoinDistribution roundTripsAesonShow

--------------------------------------------------------------------------------
-- FakeTxsHistory
--------------------------------------------------------------------------------

golden_FakeTxsHistory :: Property
golden_FakeTxsHistory =
    goldenTestJSON exampleFakeTxsHistory "test/golden/FakeTxsHistory"

roundTripFakeTxsHistory :: Property
roundTripFakeTxsHistory =
    eachOf 1000 genFakeTxsHistory roundTripsAesonShow

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleGenSpec :: GenSpec
exampleGenSpec = GenSpec
    { wallets    = 1337
    , walletSpec = exampleWalletSpec
    }

exampleWalletSpec :: WalletSpec
exampleWalletSpec = WalletSpec
    { accounts          = 1337
    , accountSpec       = exampleAccountSpec
    , fakeUtxoCoinDistr = exampleFakeUtxoCoinDistribution
    , fakeTxsHistory    = exampleFakeTxsHistory
    }

exampleAccountSpec :: AccountSpec
exampleAccountSpec = AccountSpec { addresses = 1337 }

exampleAddressRange :: AddressRange
exampleAddressRange = AddressRange 1337

exampleDistributionAmount :: DistributionAmount
exampleDistributionAmount = DistributionAmount 1337

exampleFakeUtxoCoinDistribution :: FakeUtxoCoinDistribution
exampleFakeUtxoCoinDistribution =
    RangeDistribution exampleAddressRange exampleDistributionAmount

exampleFakeTxsHistory :: FakeTxsHistory
exampleFakeTxsHistory = SimpleTxsHistory 1337 8337

--------------------------------------------------------------------------------
-- Main test function
--------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> checkSequential $$discoverGolden
             <*> checkParallel $$discoverRoundTrip
