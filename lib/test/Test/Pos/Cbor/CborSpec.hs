{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TemplateHaskell           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test.Pos.Cbor.CborSpec specification

module Test.Pos.Cbor.CborSpec
       ( spec
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Data.Tagged (Tagged)
import           System.FileLock (FileLock)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..))

import           Pos.Binary.Communication ()
import           Pos.Chain.Delegation (DlgPayload, DlgUndo)
import qualified Pos.Chain.Ssc as Ssc
import qualified Pos.Chain.Txp as T
import qualified Pos.Chain.Update as U
import qualified Pos.Communication as C
import           Pos.Communication.Limits (mlOpening, mlUpdateVote,
                     mlVssCertificate)
import           Pos.Core (StakeholderId)
import           Pos.Core.Delegation (ProxySKHeavy)
import           Pos.Core.Ssc (VssCertificate)
import qualified Pos.Core.Ssc as Ssc
import           Pos.Core.Txp (TxMsgContents (..))
import           Pos.Crypto.Signing (EncryptedSecretKey)
import           Pos.Infra.Communication.Limits.Instances (mlDataMsg, mlInvMsg,
                     mlMempoolMsg, mlReqMsg)
import qualified Pos.Infra.Communication.Relay as R
import           Pos.Infra.Communication.Types.Relay (DataMsg (..))
import qualified Pos.Infra.DHT.Model as DHT
import           Pos.Infra.Slotting.Types (SlottingData)
import           Pos.Util.UserPublic (UserPublic, WalletUserPublic)
import           Pos.Util.UserSecret (UserSecret, WalletUserSecret)

import           Test.Pos.Binary.Helpers (U, binaryTest, extensionProperty,
                     msgLenLimitedTest)
import           Test.Pos.Cbor.Arbitrary.UserPublic ()
import           Test.Pos.Cbor.Arbitrary.UserSecret ()
import           Test.Pos.Chain.Delegation.Arbitrary ()
import           Test.Pos.Chain.Ssc.Arbitrary ()
import           Test.Pos.Chain.Update.Arbitrary ()
import           Test.Pos.Configuration (withDefConfiguration)
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Crypto.Arbitrary ()
import           Test.Pos.DB.Update.Arbitrary ()
import           Test.Pos.Infra.Arbitrary ()
import           Test.Pos.Infra.Arbitrary.Communication ()
import           Test.Pos.Infra.Arbitrary.Slotting ()
import           Test.Pos.Infra.Arbitrary.Ssc ()
import           Test.Pos.Infra.Arbitrary.Update ()
import           Test.Pos.Util.QuickCheck (SmallGenerator)

type VoteId' = Tagged U.UpdateVote U.VoteId
type UpId' = Tagged (U.UpdateProposal, [U.UpdateVote])U.UpId

----------------------------------------

spec :: Spec
spec = withDefConfiguration $ \_ -> do
    describe "Cbor.Bi instances" $ do
        modifyMaxSuccess (const 1000) $ do
            describe "Lib/core instances" $ do
                brokenDisabled $ binaryTest @UserPublic
                brokenDisabled $ binaryTest @UserSecret
                modifyMaxSuccess (min 50) $ do
                    binaryTest @WalletUserPublic
                    binaryTest @WalletUserSecret
                    binaryTest @EncryptedSecretKey


        describe "Types" $ do
          describe "Message length limit" $ do
              msgLenLimitedTest @VssCertificate mlVssCertificate
        describe "Communication" $ do
            describe "Bi instances" $ do
                binaryTest @C.HandlerSpec
                binaryTest @C.VerInfo
                binaryTest @C.MessageCode
            describe "Bi extension" $ do
                prop "HandlerSpec" (extensionProperty @C.HandlerSpec)
        describe "DHT.Model" $ do
            describe "Bi instances" $ do
                binaryTest @DHT.DHTKey
                binaryTest @DHT.DHTData
        describe "Delegation types" $ do
            describe "Bi instances" $ do
                binaryTest @DlgPayload
                binaryTest @DlgUndo
            describe "Network" $ do
                binaryTest @(DataMsg ProxySKHeavy)
        describe "Slotting types" $ do
            binaryTest @SlottingData
        describe "Ssc" $ do
            describe "Bi instances" $ do
                binaryTest @Ssc.Commitment
                binaryTest @Ssc.CommitmentsMap
                binaryTest @Ssc.Opening
                modifyMaxSuccess (min 10) $ do
                    binaryTest @Ssc.SscPayload
                    binaryTest @Ssc.TossModifier
                    binaryTest @Ssc.VssCertData
                    binaryTest @Ssc.SscGlobalState
                binaryTest @Ssc.SscProof
                binaryTest @(R.InvMsg (Tagged Ssc.MCCommitment StakeholderId))
                binaryTest @(R.ReqMsg (Tagged Ssc.MCCommitment StakeholderId))
                binaryTest @(R.MempoolMsg Ssc.MCCommitment)
                binaryTest @(R.DataMsg Ssc.MCCommitment)
                binaryTest @(R.DataMsg Ssc.MCOpening)
                binaryTest @(R.DataMsg Ssc.MCShares)
                binaryTest @(R.DataMsg Ssc.MCVssCertificate)
                binaryTest @Ssc.SscTag
                binaryTest @Ssc.SscSecretStorage
            describe "Message length limit" $ do
                msgLenLimitedTest @Ssc.Opening mlOpening
                msgLenLimitedTest @(R.InvMsg (Tagged Ssc.MCCommitment StakeholderId)) mlInvMsg
                msgLenLimitedTest @(R.ReqMsg (Tagged Ssc.MCCommitment StakeholderId)) mlReqMsg
                msgLenLimitedTest @(R.MempoolMsg Ssc.MCCommitment) mlMempoolMsg
                -- msgLenLimitedTest' @(C.MaxSize (R.DataMsg Ssc.MCCommitment))
                --     (C.MaxSize . R.DataMsg <$> C.mcCommitmentMsgLenLimit)
                --     "MCCommitment"
                --     (has Ssc._MCCommitment . R.dmContents . C.getOfMaxSize)
                -- msgLenLimitedTest' @(R.DataMsg Ssc.MCOpening)
                --     (R.DataMsg <$> C.mcOpeningLenLimit)
                --     "MCOpening"
                --     (has Ssc._MCOpening . R.dmContents)
                -- msgLenLimitedTest' @(C.MaxSize (R.DataMsg Ssc.MCShares))
                --     (C.MaxSize . R.DataMsg <$> C.mcSharesMsgLenLimit)
                --     "MCShares"
                --     (has Ssc._MCShares . R.dmContents . C.getOfMaxSize)
                -- msgLenLimitedTest' @(R.DataMsg Ssc.MCVssCertificate)
                --     (R.DataMsg <$> C.mcVssCertificateLenLimit)
                --     "MCVssCertificate"
                --     (has Ssc._MCVssCertificate . R.dmContents)
        describe "Txp (transaction processing) system" $ do
            describe "Bi instances" $ do
                describe "Core" $ do
                    binaryTest @T.TxIn
                    binaryTest @T.TxOut
                    binaryTest @T.TxOutAux
                    binaryTest @T.Tx
                    binaryTest @T.TxInWitness
                    binaryTest @T.TxSigData
                    binaryTest @T.TxAux
                    binaryTest @T.TxProof
                    binaryTest @(SmallGenerator T.TxPayload)
                    binaryTest @T.TxpUndo
                describe "Network" $ do
                    binaryTest @(R.InvMsg (Tagged TxMsgContents T.TxId))
                    binaryTest @(R.ReqMsg (Tagged TxMsgContents T.TxId))
                    binaryTest @(R.MempoolMsg TxMsgContents)
                    binaryTest @(R.DataMsg TxMsgContents)
            describe "Bi extension" $ do
                prop "TxInWitness" (extensionProperty @T.TxInWitness)
            describe "Message length limit" $ do
                msgLenLimitedTest @(R.InvMsg (Tagged TxMsgContents T.TxId)) mlInvMsg
                msgLenLimitedTest @(R.ReqMsg (Tagged TxMsgContents T.TxId)) mlReqMsg
                msgLenLimitedTest @(R.MempoolMsg TxMsgContents) mlMempoolMsg
                -- No check for (DataMsg TxMsgContents) since overal message size
                -- is forcely limited
        describe "Update system" $ do
            describe "Bi instances" $ do
                describe "Core" $ do
                    binaryTest @U.BlockVersionModifier
                    binaryTest @U.SystemTag
                    binaryTest @U.UpdateVote
                    binaryTest @U.UpdateData
                    binaryTest @U.UpdateProposal
                    binaryTest @U.UpdateProposalToSign
                    binaryTest @U.UpdatePayload
                    binaryTest @U.VoteState
                    binaryTest @U.UpId
                describe "Poll" $ do
                    binaryTest @(U.PrevValue ())
                    binaryTest @(U.PrevValue U)
                    binaryTest @U.USUndo
                    binaryTest @U.UpsExtra
                    binaryTest @U.DpsExtra
                    binaryTest @U.UndecidedProposalState
                    binaryTest @U.DecidedProposalState
                    binaryTest @U.ProposalState
                    binaryTest @U.ConfirmedProposalState
                    binaryTest @U.BlockVersionState
                describe "Network" $ do
                    binaryTest @(R.InvMsg VoteId')
                    binaryTest @(R.ReqMsg VoteId')
                    binaryTest @(R.MempoolMsg U.UpdateVote)
                    binaryTest @(R.DataMsg U.UpdateVote)
                    binaryTest @(R.InvMsg UpId')
                    binaryTest @(R.ReqMsg UpId')
                    binaryTest @(R.MempoolMsg (U.UpdateProposal, [U.UpdateVote]))
                    binaryTest @(R.DataMsg (U.UpdateProposal, [U.UpdateVote]))
                describe "Message length limit" $ do
                    msgLenLimitedTest @(R.InvMsg VoteId') mlInvMsg
                    msgLenLimitedTest @(R.ReqMsg VoteId') mlReqMsg
                    msgLenLimitedTest @(R.MempoolMsg U.UpdateVote) mlMempoolMsg
                    msgLenLimitedTest @(R.InvMsg UpId') mlInvMsg
                    msgLenLimitedTest @(R.ReqMsg UpId') mlReqMsg
                    msgLenLimitedTest @(R.MempoolMsg (U.UpdateProposal, [U.UpdateVote])) mlMempoolMsg
                    -- TODO [CSL-859]
                    -- msgLenLimitedTest @(C.MaxSize (R.DataMsg (U.UpdateProposal, [U.UpdateVote])))
                    msgLenLimitedTest @(DataMsg U.UpdateVote) (mlDataMsg mlUpdateVote)
                    -- msgLenLimitedTest @U.UpdateProposal

instance {-# OVERLAPPING #-} Arbitrary (Maybe FileLock) where
    arbitrary = pure Nothing

-- | This instance is unsafe, as it allows a timing attack. But it's OK for
-- tests.
instance Eq CC.XPrv where
    (==) = (==) `on` CC.unXPrv

-- | Mark a test case as broken. The intended use is for tests that are
-- themselves valid, but the code they're testing turned out to be buggy.
brokenDisabled :: Monad m => m a -> m ()
brokenDisabled _ = return ()
