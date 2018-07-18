{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Infra.Bi
       ( tests
       ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Test.Pos.Binary.Helpers.GoldenRoundTrip (roundTripsBiBuildable)
import           Test.Pos.Infra.Gen (genHandlerSpec)
import           Test.Pos.Util.Golden (eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)

--------------------------------------------------------------------------------
-- HandlerSpec
--------------------------------------------------------------------------------

roundTripHandlerSpecBi :: Property
roundTripHandlerSpecBi = eachOf 1000 genHandlerSpec roundTripsBiBuildable

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [ H.checkParallel $$discoverRoundTrip
    ]
