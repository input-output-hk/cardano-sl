module Test.Pos.Core.Bi
    ( tests
    ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Pos.Binary.Helpers.GoldenRoundTrip (discoverGolden, discoverRoundTrip, eachOf,
                                                          -- goldenTestBi, roundTripsAesonBuildable,
                                                          -- roundTripsAesonShow,
                                                          roundTripsBiBuildable, roundTripsBiShow)
import           Test.Pos.Core.Gen


--------------------------------------------------------------------------------
-- Pos.Core.Block
--------------------------------------------------------------------------------

roundTripBlockBodyAttributesBi :: Property
roundTripBlockBodyAttributesBi = eachOf 1000 genBlockBodyAttributes roundTripsBiBuildable

roundTripBlockHeaderBi :: Property
roundTripBlockHeaderBi = eachOf 1000 genBlockHeader roundTripsBiBuildable

roundTripBlockHeaderAttributesBi :: Property
roundTripBlockHeaderAttributesBi = eachOf 1000 genBlockHeaderAttributes roundTripsBiBuildable

roundTripBlockSignatureBi :: Property
roundTripBlockSignatureBi = eachOf 1000 genBlockSignature roundTripsBiBuildable

roundTripGenesisBlockHeaderBi :: Property
roundTripGenesisBlockHeaderBi = eachOf 1000 genGenesisBlockHeader roundTripsBiBuildable

roundTripGenesisBodyBi :: Property
roundTripGenesisBodyBi = eachOf 1000 genGenesisBody roundTripsBiShow

roundTripGenesisConsensusDataBi :: Property
roundTripGenesisConsensusDataBi = eachOf 1000 genGenesisConsensusData roundTripsBiShow

-- GenesisHash is just a newtype around a Hash, and lacks Bi instances. The newtype is
-- unwrapped when constructing a block, so it doesn't appear anywhere and we don't need
-- to test it.

roundTripGenesisProofBi :: Property
roundTripGenesisProofBi = eachOf 1000 genGenesisProof roundTripsBiBuildable

roundTripMainBlockHeaderBi :: Property
roundTripMainBlockHeaderBi = eachOf 1000 genMainBlockHeader roundTripsBiBuildable

roundTripMainBodyBi :: Property
roundTripMainBodyBi = eachOf 1000 genMainBody roundTripsBiShow

roundTripMainConsensusDataBi :: Property
roundTripMainConsensusDataBi = eachOf 1000 genMainConsensusData roundTripsBiShow

roundTripMainExtraBodyDataBi :: Property
roundTripMainExtraBodyDataBi = eachOf 1000 genMainExtraBodyData roundTripsBiBuildable

roundTripMainExtraHeaderDataBi :: Property
roundTripMainExtraHeaderDataBi = eachOf 1000 genMainExtraHeaderData roundTripsBiBuildable

roundTripMainProofBi :: Property
roundTripMainProofBi = eachOf 1000 genMainProof roundTripsBiBuildable

roundTripMainToSignBi :: Property
roundTripMainToSignBi = eachOf 1000 genMainToSign roundTripsBiShow

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkSequential $$discoverRoundTrip
