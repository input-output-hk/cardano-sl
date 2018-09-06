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
import           Crypto.Hash (Blake2b_224, Blake2b_256)
import           Data.Tagged (Tagged)
import           System.FileLock (FileLock)
import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), arbitrary, generate)

import           Pos.Arbitrary.Infra ()
import           Pos.Arbitrary.Slotting ()
import           Pos.Arbitrary.Ssc ()
import           Pos.Arbitrary.Update ()
import           Pos.Binary.Class
import           Pos.Binary.Communication ()
import           Pos.Binary.Core ()
import           Pos.Binary.Ssc ()
import qualified Pos.Block.Network as BT
import qualified Pos.Block.Types as BT
import qualified Pos.Communication as C
import           Pos.Communication.Limits (mlOpening, mlUpdateVote, mlVssCertificate)
import           Pos.Core (ProxySKHeavy, StakeholderId, VssCertificate)
import qualified Pos.Core.Block as BT
import qualified Pos.Core.Ssc as Ssc
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import qualified Pos.Crypto as Crypto
import           Pos.Crypto.Signing (EncryptedSecretKey)
import           Pos.Delegation (DlgPayload, DlgUndo)
import           Pos.Infra.Binary ()
import           Pos.Infra.Communication.Limits.Instances (mlDataMsg, mlInvMsg, mlMempoolMsg,
                                                           mlReqMsg)
import qualified Pos.Infra.Communication.Relay as R
import           Pos.Infra.Communication.Types.Relay (DataMsg (..))
import qualified Pos.Infra.DHT.Model as DHT
import           Pos.Infra.Slotting.Types (SlottingData)
import qualified Pos.Ssc as Ssc
import qualified Pos.Txp as T
import qualified Pos.Update as U
import           Pos.Util.UserSecret (UserSecret, WalletUserSecret)

import           Test.Pos.Binary.Helpers (U, binaryTest, extensionProperty, msgLenLimitedTest)
import           Test.Pos.Block.Arbitrary ()
import           Test.Pos.Block.Arbitrary.Message ()
import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Crypto.Arbitrary ()
import           Test.Pos.Delegation.Arbitrary ()
import           Test.Pos.Txp.Arbitrary.Network ()
import           Test.Pos.Util.QuickCheck (SmallGenerator)


type VoteId' = Tagged U.UpdateVote U.VoteId
type UpId' = Tagged (U.UpdateProposal, [U.UpdateVote])U.UpId

----------------------------------------

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $ do
    describe "Cbor.Bi instances" $ do
        modifyMaxSuccess (const 1000) $ do
            describe "Lib/core instances" $ do
                brokenDisabled $ binaryTest @UserSecret
                modifyMaxSuccess (min 50) $ do
                    binaryTest @WalletUserSecret
                    binaryTest @EncryptedSecretKey


        describe "Types" $ do
          describe "Message length limit" $ do
              msgLenLimitedTest @VssCertificate mlVssCertificate
        describe "Block types" $ do
            describe "Bi instances" $ do
                describe "Undo" $ do
                    binaryTest @BT.SlogUndo
                    modifyMaxSuccess (min 50) $ do
                        binaryTest @BT.Undo
                describe "Block network types" $ modifyMaxSuccess (min 10) $ do
                    binaryTest @BT.MsgGetHeaders
                    binaryTest @BT.MsgGetBlocks
                    binaryTest @BT.MsgHeaders
                    binaryTest @BT.MsgBlock
                    binaryTest @BT.MsgStream
                    binaryTest @BT.MsgStreamBlock
                describe "Blockchains and blockheaders" $ do
                    modifyMaxSuccess (min 10) $ describe "GenericBlockHeader" $ do
                        describe "GenesisBlockHeader" $ do
                            binaryTest @BT.GenesisBlockHeader
                        describe "MainBlockHeader" $ do
                            binaryTest @BT.MainBlockHeader
                    describe "GenesisBlockchain" $ do
                        describe "BodyProof" $ do
                            binaryTest @BT.GenesisExtraHeaderData
                            binaryTest @BT.GenesisExtraBodyData
                            binaryTest @(BT.BodyProof BT.GenesisBlockchain)
                        describe "ConsensusData" $ do
                            binaryTest @(BT.ConsensusData BT.GenesisBlockchain)
                        describe "Body" $ do
                            binaryTest @(BT.Body BT.GenesisBlockchain)
                    describe "MainBlockchain" $ do
                        describe "BodyProof" $ do
                            binaryTest @(BT.BodyProof BT.MainBlockchain)
                        describe "BlockSignature" $ do
                            binaryTest @BT.BlockSignature
                        describe "ConsensusData" $ do
                            binaryTest @(BT.ConsensusData BT.MainBlockchain)
                        modifyMaxSuccess (min 10) $ describe "Body" $ do
                            binaryTest @(BT.Body BT.MainBlockchain)
                        describe "MainToSign" $ do
                            binaryTest @BT.MainToSign
                        describe "Extra data" $ do
                            binaryTest @BT.MainExtraHeaderData
                            binaryTest @BT.MainExtraBodyData
        describe "Communication" $ do
            describe "Bi instances" $ do
                binaryTest @C.HandlerSpec
                binaryTest @C.VerInfo
                binaryTest @C.MessageCode
            describe "Bi extension" $ do
                prop "HandlerSpec" (extensionProperty @C.HandlerSpec)
        describe "Crypto" $ do
            describe "Hashing" $ do
                binaryTest @(Crypto.Hash Word64)
            describe "Signing" $ do
                describe "Bi instances" $ do
                    binaryTest @Crypto.SecretKey
                    binaryTest @Crypto.PublicKey
                    binaryTest @(Crypto.Signature ())
                    binaryTest @(Crypto.Signature U)
                    binaryTest @(Crypto.ProxyCert Int32)
                    binaryTest @(Crypto.ProxySecretKey Int32)
                    binaryTest @(Crypto.ProxySecretKey U)
                    binaryTest @(Crypto.ProxySignature Int32 Int32)
                    binaryTest @(Crypto.ProxySignature U U)
                    binaryTest @(Crypto.Signed Bool)
                    binaryTest @(Crypto.Signed U)
                    binaryTest @Crypto.RedeemSecretKey
                    binaryTest @Crypto.RedeemPublicKey
                    binaryTest @(Crypto.RedeemSignature Bool)
                    binaryTest @(Crypto.RedeemSignature U)
                    binaryTest @Crypto.Threshold
                    binaryTest @Crypto.VssPublicKey
                    binaryTest @Crypto.PassPhrase
                    binaryTest @Crypto.VssKeyPair
                    binaryTest @Crypto.Secret
                    binaryTest @Crypto.DecShare
                    binaryTest @Crypto.EncShare
                    binaryTest @Crypto.SecretProof
                    binaryTest @Crypto.HDAddressPayload
                    binaryTest @(Crypto.AbstractHash Blake2b_224 U)
                    binaryTest @(Crypto.AbstractHash Blake2b_256 U)
                    binaryTest @(AsBinary Crypto.VssPublicKey)
                    binaryTest @(AsBinary Crypto.Secret)
                    binaryTest @(AsBinary Crypto.DecShare)
                    binaryTest @(AsBinary Crypto.EncShare)
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
                    binaryTest @(R.InvMsg (Tagged T.TxMsgContents T.TxId))
                    binaryTest @(R.ReqMsg (Tagged T.TxMsgContents T.TxId))
                    binaryTest @(R.MempoolMsg T.TxMsgContents)
                    binaryTest @(R.DataMsg T.TxMsgContents)
            describe "Bi extension" $ do
                prop "TxInWitness" (extensionProperty @T.TxInWitness)
            describe "Message length limit" $ do
                msgLenLimitedTest @(R.InvMsg (Tagged T.TxMsgContents T.TxId)) mlInvMsg
                msgLenLimitedTest @(R.ReqMsg (Tagged T.TxMsgContents T.TxId)) mlReqMsg
                msgLenLimitedTest @(R.MempoolMsg T.TxMsgContents) mlMempoolMsg
                -- No check for (DataMsg T.TxMsgContents) since overal message size
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
