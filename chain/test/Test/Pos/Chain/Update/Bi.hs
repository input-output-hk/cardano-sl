{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Chain.Update.Bi
       ( tests
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Binary.Class (Raw (..))
import           Pos.Chain.Update (ApplicationName (..), SoftforkRule (..))
import           Pos.Core (CoinPortion (..))
import           Pos.Crypto (Hash, abstractHash)

import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Pos.Chain.Update.Example (exampleBlockVersion,
                     exampleBlockVersionData0, exampleBlockVersionModifier,
                     exampleSoftwareVersion, exampleSystemTag,
                     exampleUpAttributes, exampleUpId, exampleUpdateData,
                     exampleUpdatePayload, exampleUpdateProof,
                     exampleUpdateProposal, exampleUpdateProposalToSign,
                     exampleUpdateVote, exampleVoteId)
import           Test.Pos.Chain.Update.Gen (genApplicationName, genBlockVersion,
                     genBlockVersionData, genBlockVersionModifier,
                     genSoftforkRule, genSoftwareVersion, genSystemTag,
                     genUpAttributes, genUpId, genUpdateData, genUpdatePayload,
                     genUpdateProof, genUpdateProposal,
                     genUpdateProposalToSign, genUpdateProposals,
                     genUpdateVote, genUpsData, genVoteId)
import           Test.Pos.Core.ExampleHelpers (feedPM)
import           Test.Pos.Core.Gen (genHashRaw)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)

--------------------------------------------------------------------------------
-- ApplicationName
--------------------------------------------------------------------------------

golden_ApplicationName :: Property
golden_ApplicationName = goldenTestBi aN "test/golden/bi/update/ApplicationName"
    where aN = ApplicationName "Golden"

roundTripApplicationName :: Property
roundTripApplicationName = eachOf 50 genApplicationName roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersion
--------------------------------------------------------------------------------

golden_BlockVersion :: Property
golden_BlockVersion = goldenTestBi exampleBlockVersion "test/golden/bi/update/BlockVersion"

roundTripBlockVersion :: Property
roundTripBlockVersion = eachOf 50 genBlockVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

golden_BlockVersionData :: Property
golden_BlockVersionData = goldenTestBi bVerDat "test/golden/bi/update/BlockVersionData"
    where bVerDat = exampleBlockVersionData0

roundTripBlockVersionData :: Property
roundTripBlockVersionData = eachOf 50 genBlockVersionData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionModifier
--------------------------------------------------------------------------------

golden_BlockVersionModifier :: Property
golden_BlockVersionModifier = goldenTestBi bVerMod "test/golden/bi/update/BlockVersionModifier"
    where bVerMod = exampleBlockVersionModifier

roundTripBlockVersionModifier :: Property
roundTripBlockVersionModifier = eachOf 50 genBlockVersionModifier roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HashRaw
--------------------------------------------------------------------------------

golden_BlockHashRaw :: Property
golden_BlockHashRaw = goldenTestBi hRaw "test/golden/bi/update/HashRaw"
    where hRaw = (abstractHash $ Raw ("9" ) :: Hash Raw)

roundTripHashRaw :: Property
roundTripHashRaw = eachOf 50 genHashRaw roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

golden_SoftforkRule :: Property
golden_SoftforkRule = goldenTestBi sfR "test/golden/bi/update/SoftforkRule"
    where sfR = SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99)

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 10 genSoftforkRule roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

golden_SoftwareVersion :: Property
golden_SoftwareVersion = goldenTestBi exampleSoftwareVersion "test/golden/bi/update/SoftwareVersion"

roundTripSoftwareVersion :: Property
roundTripSoftwareVersion = eachOf 10 genSoftwareVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SystemTag
--------------------------------------------------------------------------------

golden_SystemTag :: Property
golden_SystemTag = goldenTestBi exampleSystemTag "test/golden/bi/update/SystemTag"

roundTripSystemTag :: Property
roundTripSystemTag = eachOf 10 genSystemTag roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpAttributes
--------------------------------------------------------------------------------

golden_UpAttributes :: Property
golden_UpAttributes = goldenTestBi exampleUpAttributes "test/golden/bi/update/UpAttributes"

roundTripUpAttributes :: Property
roundTripUpAttributes = eachOf 20 genUpAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateData
--------------------------------------------------------------------------------

golden_UpdateData :: Property
golden_UpdateData = goldenTestBi exampleUpdateData "test/golden/bi/update/UpdateData"

roundTripUpdateData :: Property
roundTripUpdateData = eachOf 20 genUpdateData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdatePayload
--------------------------------------------------------------------------------

golden_UpdatePayload :: Property
golden_UpdatePayload = goldenTestBi exampleUpdatePayload "test/golden/bi/update/UpdatePayload"

roundTripUpdatePayload :: Property
roundTripUpdatePayload = eachOf 20 (feedPM genUpdatePayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProof
--------------------------------------------------------------------------------

golden_UpdateProof :: Property
golden_UpdateProof = goldenTestBi exampleUpdateProof "test/golden/bi/update/UpdateProof"

roundTripUpdateProof :: Property
roundTripUpdateProof = eachOf 20 (feedPM genUpdateProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposal
--------------------------------------------------------------------------------

golden_UpdateProposal :: Property
golden_UpdateProposal = goldenTestBi exampleUpdateProposal "test/golden/bi/update/UpdateProposal"

roundTripUpdateProposal :: Property
roundTripUpdateProposal = eachOf 20 (feedPM genUpdateProposal) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposals
--------------------------------------------------------------------------------

golden_UpdateProposals :: Property
golden_UpdateProposals = goldenTestBi ups "test/golden/bi/update/UpdateProposals"
  where
    -- Need to revisit this.
    ups = HM.fromList [(exampleUpId, exampleUpdateProposal)]

roundTripUpdateProposals :: Property
roundTripUpdateProposals = eachOf 20 (feedPM genUpdateProposals) roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateProposalToSign
--------------------------------------------------------------------------------

golden_UpdateProposalToSign :: Property
golden_UpdateProposalToSign =
  goldenTestBi exampleUpdateProposalToSign "test/golden/bi/update/UpdateProposalToSign"

roundTripUpdateProposalToSign :: Property
roundTripUpdateProposalToSign = eachOf 20 genUpdateProposalToSign roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateVote
--------------------------------------------------------------------------------

golden_UpdateVote :: Property
golden_UpdateVote = goldenTestBi exampleUpdateVote "test/golden/bi/update/UpdateVote"

roundTripUpdateVote :: Property
roundTripUpdateVote = eachOf 20 (feedPM genUpdateVote) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpId
--------------------------------------------------------------------------------

golden_UpId :: Property
golden_UpId = goldenTestBi exampleUpId "test/golden/bi/update/UpId"

roundTripUpId :: Property
roundTripUpId = eachOf 20 (feedPM genUpId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpsData NB: UpsData is not a type it is a record accessor of `UpdateProposalToSign`
--------------------------------------------------------------------------------

roundTripUpsData :: Property
roundTripUpsData = eachOf 20 genUpsData roundTripsBiShow

--------------------------------------------------------------------------------
-- VoteId
--------------------------------------------------------------------------------

golden_VoteId :: Property
golden_VoteId = goldenTestBi exampleVoteId "test/golden/bi/update/VoteId"

roundTripVoteId :: Property
roundTripVoteId = eachOf 20 (feedPM genVoteId) roundTripsBiBuildable

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [H.checkSequential $$discoverGolden, H.checkParallel $$discoverRoundTrip]
