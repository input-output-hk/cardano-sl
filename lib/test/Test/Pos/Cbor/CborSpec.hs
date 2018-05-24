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
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Block ()
import           Pos.Arbitrary.Block.Message ()
import           Pos.Arbitrary.Core ()
import           Pos.Arbitrary.Delegation ()
import           Pos.Arbitrary.Infra ()
import           Pos.Arbitrary.Slotting ()
import           Pos.Arbitrary.Ssc ()
import           Pos.Arbitrary.Update ()
import           Pos.Binary.Class
import           Pos.Binary.Communication ()
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Binary.Infra ()
import           Pos.Binary.Ssc ()
import qualified Pos.Block.Network as BT
import qualified Pos.Block.Types as BT
import qualified Pos.Communication as C
import           Pos.Communication.Limits (mlOpening, mlUpdateVote, mlVssCertificate)
import           Pos.Communication.Limits.Instances (mlMempoolMsg, mlInvMsg, mlReqMsg, mlDataMsg)
import qualified Pos.Communication.Relay as R
import           Pos.Communication.Types.Relay (DataMsg (..))
import qualified Pos.Core as T
import qualified Pos.Core.Block as BT
import           Pos.Core.Common (ScriptVersion)
import qualified Pos.Core.Ssc as Ssc
import qualified Pos.Crypto as Crypto
import           Pos.Crypto.Signing (EncryptedSecretKey)
import           Pos.Data.Attributes (Attributes (..), decodeAttributes, encodeAttributes)
import           Pos.Delegation (DlgPayload, DlgUndo)
import qualified Pos.DHT.Model as DHT
import           Pos.Merkle (MerkleTree)
import           Pos.Slotting.Types (SlottingData)
import qualified Pos.Ssc as Ssc
import qualified Pos.Txp as T
import qualified Pos.Update as U
import           Pos.Core.Chrono (NE, NewestFirst, OldestFirst)
import           Pos.Util.UserSecret (UserSecret, WalletUserSecret)

import           Test.Pos.Configuration (withDefConfiguration)
import           Test.Pos.Crypto.Arbitrary ()
import           Test.Pos.Binary.Helpers (U, binaryTest, extensionProperty, msgLenLimitedTest)
import           Test.Pos.Txp.Arbitrary.Network ()
import           Test.Pos.Util.QuickCheck (SmallGenerator)


type VoteId' = Tagged U.UpdateVote U.VoteId
type UpId' = Tagged (U.UpdateProposal, [U.UpdateVote])U.UpId


data MyScript = MyScript
    { version :: ScriptVersion -- ^ Version
    , script  :: ByteString    -- ^ Serialized script
    } deriving (Eq, Show, Generic, Typeable)

instance Arbitrary MyScript where
    arbitrary = MyScript <$> arbitrary <*> arbitrary

deriveSimpleBi ''MyScript [
    Cons 'MyScript [
        Field [| version :: ScriptVersion |],
        Field [| script  :: ByteString   |]
    ]]

----------------------------------------

data X1 = X1 { x1A :: Int }
    deriving (Eq, Ord, Show, Generic)

data X2 = X2 { x2A :: Int, x2B :: String }
    deriving (Eq, Ord, Show, Generic)

instance Arbitrary X1 where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary X2 where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Bi (Attributes X1) where
    encode = encodeAttributes [(0, serialize . x1A)]
    decode = decodeAttributes (X1 0) $ \n v acc -> case n of
        0 -> pure $ Just $ acc { x1A = unsafeDeserialize v }
        _ -> pure $ Nothing

instance Bi (Attributes X2) where
    encode = encodeAttributes [(0, serialize . x2A), (1, serialize . x2B)]
    decode = decodeAttributes (X2 0 []) $ \n v acc -> case n of
        0 -> return $ Just $ acc { x2A = unsafeDeserialize v }
        1 -> return $ Just $ acc { x2B = unsafeDeserialize v }
        _ -> return $ Nothing

----------------------------------------

soundSerializationAttributesOfAsProperty
    :: forall a b aa ab. (aa ~ Attributes a, ab ~ Attributes b,
                          Bi aa, Bi ab, Eq aa, Arbitrary a, Show aa)
    => Property
soundSerializationAttributesOfAsProperty = forAll arbitraryAttrs $ \input ->
    let serialized      = serialize input
        (middle  :: ab) = unsafeDeserialize serialized
        (encoded :: aa) = unsafeDeserialize $ serialize middle
    in encoded === input
  where
    arbitraryAttrs :: Gen aa
    arbitraryAttrs = Attributes <$> arbitrary <*> arbitrary


spec :: Spec
spec = withDefConfiguration $ do
    describe "Cbor.Bi instances" $ do
        modifyMaxSuccess (const 1000) $ do
            describe "Test instances" $ do
                binaryTest @MyScript
                prop "X2" (soundSerializationAttributesOfAsProperty @X2 @X1)
            describe "Lib/core instances" $ do
                binaryTest @(Attributes X1)
                binaryTest @(Attributes X2)
                brokenDisabled $ binaryTest @UserSecret
                modifyMaxSuccess (min 50) $ do
                    binaryTest @WalletUserSecret
                    binaryTest @EncryptedSecretKey


        describe "Types" $ do
          -- 100 is not enough to catch some bugs (e.g. there was a bug with
          -- addresses that only manifested when address's CRC started with 0x00)
          describe "Bi instances" $ do
              describe "Core.Address" $ do
                  binaryTest @T.Address
                  binaryTest @T.Address'
                  binaryTest @T.AddrType
                  binaryTest @T.AddrStakeDistribution
                  binaryTest @T.AddrSpendingData
              describe "Core.Types" $ do
                  binaryTest @T.Timestamp
                  binaryTest @T.TimeDiff
                  binaryTest @T.EpochIndex
                  binaryTest @T.Coin
                  binaryTest @T.CoinPortion
                  binaryTest @T.LocalSlotIndex
                  binaryTest @T.SlotId
                  binaryTest @T.EpochOrSlot
                  binaryTest @T.SharedSeed
                  binaryTest @T.ChainDifficulty
                  binaryTest @T.SoftforkRule
                  binaryTest @T.BlockVersionData
                  binaryTest @(Attributes ())
                  binaryTest @(Attributes T.AddrAttributes)
              describe "Core.Fee" $ do
                  binaryTest @T.Coeff
                  binaryTest @T.TxSizeLinear
                  binaryTest @T.TxFeePolicy
              describe "Core.Script" $ do
                  binaryTest @T.Script
              describe "Core.Vss" $ do
                  binaryTest @T.VssCertificate
              describe "Core.Version" $ do
                  binaryTest @T.ApplicationName
                  binaryTest @T.SoftwareVersion
                  binaryTest @T.BlockVersion
              describe "Util" $ do
                  binaryTest @(NewestFirst NE U)
                  binaryTest @(OldestFirst NE U)
          describe "Message length limit" $ do
              msgLenLimitedTest @T.VssCertificate mlVssCertificate
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
        describe "Merkle" $ do
            binaryTest @(MerkleTree Int32)
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
                binaryTest @(DataMsg T.ProxySKHeavy)
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
                binaryTest @(R.InvMsg (Tagged Ssc.MCCommitment T.StakeholderId))
                binaryTest @(R.ReqMsg (Tagged Ssc.MCCommitment T.StakeholderId))
                binaryTest @(R.MempoolMsg Ssc.MCCommitment)
                binaryTest @(R.DataMsg Ssc.MCCommitment)
                binaryTest @(R.DataMsg Ssc.MCOpening)
                binaryTest @(R.DataMsg Ssc.MCShares)
                binaryTest @(R.DataMsg Ssc.MCVssCertificate)
                binaryTest @Ssc.SscTag
                binaryTest @Ssc.SscSecretStorage
            describe "Message length limit" $ do
                msgLenLimitedTest @Ssc.Opening mlOpening
                msgLenLimitedTest @(R.InvMsg (Tagged Ssc.MCCommitment T.StakeholderId)) mlInvMsg
                msgLenLimitedTest @(R.ReqMsg (Tagged Ssc.MCCommitment T.StakeholderId)) mlReqMsg
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
