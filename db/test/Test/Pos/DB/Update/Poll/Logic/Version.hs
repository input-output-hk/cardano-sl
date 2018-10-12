{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.DB.Update.Poll.Logic.Version
       ( tests
       ) where

import           Universum

import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import           Hedgehog (Property, checkParallel, discover, property,
                     withTests, (===))

import           Pos.Chain.Update (ApplicationName (..), BlockVersion (..),
                     PollVerFailure (..), SoftwareVersion (..), UpId,
                     UpdateProposal, mkUpdateProposalWSign)
import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..), decodeAbstractHash, hash)
import           Pos.DB.Update (PollState (..), PurePoll (..),
                     evalPurePollWithLogger)
import           Pos.DB.Update.Poll.Logic.Version
                     (verifyBlockAndSoftwareVersions)

import           Test.Pos.Chain.Update.Example (exampleBlockVersionData1,
                     exampleSystemTag, exampleUpAttributes, exampleUpdateData)
import           Test.Pos.Core.ExampleHelpers (exampleSafeSigner)


--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_valid0 :: Property
prop_valid0 =
    mkProperty (mkBV 0 0 0, Just $ mkSV 1) (mkBV 0 0 0, mkSV 2)
               (Right ())

prop_valid1 :: Property
prop_valid1 =
    mkProperty (mkBV 0 0 1, Nothing) (mkBV 0 0 1, mkSV 0)
               (Right ())

prop_valid3 :: Property
prop_valid3 =
    mkProperty (mkBV 7 3 4, Just $ mkSV 9) (mkBV 7 4 0, mkSV 10)
               (Right ())

prop_invalidBvBump0 :: Property
prop_invalidBvBump0 =
    mkProperty (mkBV 0 3 1, Just $ mkSV 9) (mkBV 0 3 0, mkSV 9)
               (Left (PollBadBlockVersion
                   (decodeOrError
                       "3f76b57b805608b04eeaa59299637934355b63d4cd6fc10c86b0ddd47a5631c3")
                   (mkBV 0 3 0)
                   (mkBV 0 3 1)))

prop_invalidBvBump1 :: Property
prop_invalidBvBump1 =
    mkProperty (mkBV 0 3 0, Just $ mkSV 9) (mkBV 0 2 0, mkSV 9)
               (Left (PollBadBlockVersion
                   (decodeOrError
                       "2537deac28d6e7ea3647f1c7103ef0afe6f69e6ab4434f752a8327a8d4d8ee52")
                   (mkBV 0 2 0)
                   (mkBV 0 3 0)))

prop_invalidBvBump2 :: Property
prop_invalidBvBump2 =
    mkProperty (mkBV 2 7 5, Just $ mkSV 4) (mkBV 2 7 3, mkSV 4)
               (Left (PollBadBlockVersion
                   (decodeOrError
                       "bf3fb28df9a6151c834074e75103ec589e5f2ca6c1a5de511ad012249f87c08b")
                   (mkBV 2 7 3)
                   (mkBV 2 7 5)))

prop_invalidBvBump3 :: Property
prop_invalidBvBump3 =
    mkProperty (mkBV 2 7 5, Just $ mkSV 2) (mkBV 2 8 2, mkSV 2)
               (Left (PollBadBlockVersion
                   (decodeOrError
                       "e00dd03a121d2c718b51e3e7c92807de10aacfe8581bfc3619928c3f14af2240")
                   (mkBV 2 8 2)
                   (mkBV 2 7 5)))

-- Test alt bump
prop_invalidBvBump4 :: Property
prop_invalidBvBump4 =
    mkProperty (mkBV 7 3 0, Just $ mkSV 12) (mkBV 7 3 1, mkSV 13)
               (Left (PollBadBlockVersion
                   (decodeOrError
                       "59177181f10e2de7d32ce2c4d5f63fd22a0dcbba71e369b74b0a0e480e5c3325")
                   (mkBV 7 3 1)
                   (mkBV 7 3 0)))

prop_noProtocolOnly0 :: Property
prop_noProtocolOnly0 =
    mkProperty (mkBV 0 3 3, Just $ mkSV 0) (mkBV 0 4 0, mkSV 0)
               (Left (PollBadBlockVersion
                   (decodeOrError
                       "5da2ab2d393b72fa29c5a055740e53dd5e5ca37acad2767add693c2609343fa4")
                   (mkBV 0 0 1)
                   (mkBV 0 0 0)))

prop_noProtocolOnly1 :: Property
prop_noProtocolOnly1 =
    mkProperty (mkBV 0 0 1, Just $ mkSV 1) (mkBV 0 1 0, mkSV 1)
               (Left
                  (PollWrongSoftwareVersion
                     (Just 1)
                     ApplicationName { getApplicationName = "cardano-sl" }
                     1
                     (decodeOrError
                         "d8730c72142a53d835b13e8313c9b7902f782d1f51cf448fbe0ed14f42578de5")))

prop_badSvBump :: Property
prop_badSvBump =
    mkProperty (mkBV 0 0 1, Just $ mkSV 3) (mkBV 0 0 1, mkSV 7)
               (Left
                  (PollWrongSoftwareVersion
                     (Just 3)
                     ApplicationName { getApplicationName = "cardano-sl" }
                     7
                     (decodeOrError
                        "8116d4b2be066b0cfeeea8a5850c3f0bf89640f63437951031e636e358df1b6d")))

prop_badInitialSV :: Property
prop_badInitialSV =
    mkProperty (mkBV 0 0 1, Nothing) (mkBV 0 0 1, mkSV 1)
               (Left
                  (PollWrongSoftwareVersion
                     Nothing
                     ApplicationName { getApplicationName = "cardano-sl" }
                     1
                     (decodeOrError
                        "e64e5215b1e84dddabd04bfa5d2b9dd5abe29f1af6308ac065cc79df540e702b")))


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Scaffolding code which constructs a PollState and executes the monadic
-- `verifyBlockAndSoftwareVersions` check with the appropriate block &
-- software versions in context.
mkProperty :: (BlockVersion, Maybe SoftwareVersion)
           -> (BlockVersion, SoftwareVersion)
           -> Either PollVerFailure ()
           -> Property
mkProperty (adoptedBV, mAdoptedSV) (proposedBV, proposedSV) expected =
    withTests 1 (property (result === expected))
  where
    confirmedANs = case mAdoptedSV of
        Nothing -> HM.empty
        Just adoptedSV -> HM.singleton (svAppName adoptedSV) (svNumber adoptedSV)
    initPS = PollState mempty (adoptedBV, exampleBlockVersionData1)
                       mempty confirmedANs mempty mempty
                       mempty mempty mempty mempty
    --
    initUP   = mkUpdateProposal proposedBV proposedSV
    initUpId = hash initUP
    --
    check :: ExceptT PollVerFailure PurePoll ()
    check = verifyBlockAndSoftwareVersions initUpId initUP
    --
    result :: Either PollVerFailure ()
    result = evalPurePollWithLogger initPS (runExceptT check)

mkUpdateProposal :: BlockVersion -> SoftwareVersion -> UpdateProposal
mkUpdateProposal bv sv =
    mkUpdateProposalWSign pm bv bvm sv hm ua ss
  where
    pm  = ProtocolMagic (ProtocolMagicId 0) RequiresNoMagic
    bvm = def
    hm  = HM.fromList [(exampleSystemTag, exampleUpdateData)]
    ua  = exampleUpAttributes
    ss  = exampleSafeSigner 0

-- Helper functions for test concision
mkBV :: Word16 -> Word16 -> Word8 -> BlockVersion
mkBV = BlockVersion

mkSV :: Word32 -> SoftwareVersion
mkSV = SoftwareVersion (ApplicationName "cardano-sl")

decodeOrError :: Text -> UpId
decodeOrError = fromRight (error "bad decode") . decodeAbstractHash

-- We need to derive these instances to compare failures
deriving instance Eq PollVerFailure
deriving instance Show PollVerFailure


--------------------------------------------------------------------------------
-- Test export
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$(discover)
