{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Pos.Infra.Bi
       ( tests
       ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Pos.Infra.Communication.Types.Protocol (HandlerSpec (..))
import qualified Prelude
import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable)
import           Test.Pos.Infra.Gen (genHandlerSpec)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)

--------------------------------------------------------------------------------
-- HandlerSpec
--------------------------------------------------------------------------------

golden_HandlerSpec_ConvHandler :: Property
golden_HandlerSpec_ConvHandler =
    goldenTestBi exampleConvHandler "test/golden/HandlerSpec_ConvHandler"
  where
    exampleConvHandler = ConvHandler 15553

golden_HandlerSpec_UnknownHandler :: Property
golden_HandlerSpec_UnknownHandler =
    goldenTestBi exampleUnknownHandler "test/golden/HandlerSpec_UnknownHandler"
  where
    exampleUnknownHandler = UnknownHandler 104 Prelude.$ encodeUtf8 @String @ByteString "\248U=\232\167\t"

roundTripHandlerSpecBi :: Property
roundTripHandlerSpecBi = eachOf 1000 genHandlerSpec roundTripsBiBuildable

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [ H.checkSequential $$discoverGolden
    , H.checkParallel $$discoverRoundTrip
    ]
