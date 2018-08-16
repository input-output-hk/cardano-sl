{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Pos.Chain.Txp.Json
       ( tests
       ) where
import           Universum

import qualified Data.Set as S
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Chain.Txp (RequiresNetworkMagic (..),
                     TxpConfiguration (..))

import           Test.Pos.Chain.Txp.Gen (genTxpConfiguration)
import           Test.Pos.Core.ExampleHelpers (exampleAddress, exampleAddress1,
                     exampleAddress2, exampleAddress3, exampleAddress4)
import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestJSON,
                     goldenTestJSONDec)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

-------------------------------------------------------------------------------
-- TxpConfiguration
-------------------------------------------------------------------------------

golden_TxpConfiguration0 :: Property
golden_TxpConfiguration0 =
    goldenTestJSON exampleTxpConfiguration0
        "test/golden/json/TxpConfiguration0_YesNetworkMagic"

golden_TxpConfiguration1 :: Property
golden_TxpConfiguration1 =
    goldenTestJSON exampleTxpConfiguration1
        "test/golden/json/TxpConfiguration1_YesNetworkMagic"

golden_TxpConfiguration2 :: Property
golden_TxpConfiguration2 =
    goldenTestJSON exampleTxpConfiguration2
        "test/golden/json/TxpConfiguration2_YesNetworkMagic"

golden_TxpConfiguration3 :: Property
golden_TxpConfiguration3 =
    goldenTestJSON exampleTxpConfiguration3
        "test/golden/json/TxpConfiguration3_YesNetworkMagic"

golden_TxpConfiguration4 :: Property
golden_TxpConfiguration4 =
    goldenTestJSON exampleTxpConfiguration4
        "test/golden/json/TxpConfiguration4_YesNetworkMagic"

golden_TxpConfiguration5 :: Property
golden_TxpConfiguration5 =
    goldenTestJSON exampleTxpConfiguration5
        "test/golden/json/TxpConfiguration5_YesNetworkMagic"


-- Test only decoding (for ensuring backwards compatibility with
-- old TxpConfiguration format).
golden_TxpConfiguration0Dec :: Property
golden_TxpConfiguration0Dec =
    goldenTestJSONDec exampleTxpConfiguration0
        "test/golden/json/TxpConfiguration0_NoNetworkMagic"

golden_TxpConfiguration1Dec :: Property
golden_TxpConfiguration1Dec =
    goldenTestJSONDec exampleTxpConfiguration1
        "test/golden/json/TxpConfiguration1_NoNetworkMagic"

golden_TxpConfiguration2Dec :: Property
golden_TxpConfiguration2Dec =
    goldenTestJSONDec exampleTxpConfiguration2
        "test/golden/json/TxpConfiguration2_NoNetworkMagic"


roundTripTxpConfiguration :: Property
roundTripTxpConfiguration =
    eachOf 200 genTxpConfiguration roundTripsAesonShow

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

exampleTxpConfiguration0 :: TxpConfiguration
exampleTxpConfiguration0 = TxpConfiguration 99 talsa NMMustBeJust
  where
    talsa = S.fromList [exampleAddress]

exampleTxpConfiguration1 :: TxpConfiguration
exampleTxpConfiguration1 = TxpConfiguration 9 talsa NMMustBeJust
  where
    talsa = S.fromList [exampleAddress1, exampleAddress2, exampleAddress3]

exampleTxpConfiguration2 :: TxpConfiguration
exampleTxpConfiguration2 = TxpConfiguration 700 talsa NMMustBeJust
  where
    talsa = S.fromList [exampleAddress4, exampleAddress]

exampleTxpConfiguration3 :: TxpConfiguration
exampleTxpConfiguration3 = TxpConfiguration 208 talsa NMMustBeNothing
  where
    talsa = S.fromList [ exampleAddress, exampleAddress3, exampleAddress4
                       , exampleAddress1, exampleAddress2]

exampleTxpConfiguration4 :: TxpConfiguration
exampleTxpConfiguration4 = TxpConfiguration 0 talsa NMMustBeJust
  where
    talsa = S.fromList [exampleAddress4, exampleAddress]

exampleTxpConfiguration5 :: TxpConfiguration
exampleTxpConfiguration5 = TxpConfiguration 33 talsa NMMustBeNothing
  where
    talsa = S.fromList [exampleAddress4, exampleAddress2, exampleAddress]

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
