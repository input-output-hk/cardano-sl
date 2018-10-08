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

import           Pos.Chain.Txp (TxpConfiguration (..))

import           Test.Pos.Chain.Txp.Gen (genTxpConfiguration)
import           Test.Pos.Core.ExampleHelpers (exampleAddress, exampleAddress1,
                     exampleAddress2, exampleAddress3, exampleAddress4)
import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestJSONPretty)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

-------------------------------------------------------------------------------
-- TxpConfiguration
-------------------------------------------------------------------------------

golden_TxpConfiguration0 :: Property
golden_TxpConfiguration0 =
    goldenTestJSONPretty exampleTxpConfiguration0
        "test/golden/json/TxpConfiguration0"

golden_TxpConfiguration1 :: Property
golden_TxpConfiguration1 =
    goldenTestJSONPretty exampleTxpConfiguration1
        "test/golden/json/TxpConfiguration1"

golden_TxpConfiguration2 :: Property
golden_TxpConfiguration2 =
    goldenTestJSONPretty exampleTxpConfiguration2
        "test/golden/json/TxpConfiguration2"

roundTripTxpConfiguration :: Property
roundTripTxpConfiguration =
    eachOf 200 genTxpConfiguration roundTripsAesonShow

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

exampleTxpConfiguration0 :: TxpConfiguration
exampleTxpConfiguration0 = TxpConfiguration 99 talsa
  where
    talsa = S.fromList [exampleAddress]

exampleTxpConfiguration1 :: TxpConfiguration
exampleTxpConfiguration1 = TxpConfiguration 9 talsa
  where
    talsa = S.fromList [exampleAddress1, exampleAddress2, exampleAddress3]

exampleTxpConfiguration2 :: TxpConfiguration
exampleTxpConfiguration2 = TxpConfiguration 700 talsa
  where
    talsa = S.fromList [exampleAddress4, exampleAddress]

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
