{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Pos.Ssc.Json
       ( tests
       ) where
import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Test.Pos.Ssc.Gen (genAttackTarget)
import           Test.Pos.Util.Golden (eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

-------------------------------------------------------------------------------
-- AttackTarget
-------------------------------------------------------------------------------

roundTripAttackTarget :: Property
roundTripAttackTarget =
    eachOf 1000 genAttackTarget roundTripsAesonShow

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
