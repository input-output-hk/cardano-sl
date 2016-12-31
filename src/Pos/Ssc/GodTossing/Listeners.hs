{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Instance of SscListenersClass

module Pos.Ssc.GodTossing.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscGodTossing
       ) where

import           Data.HashMap.Strict                    (lookup)
import           Data.Tagged                            (Tagged (..))
import           Formatting                             (build, sformat, (%))
import           Serokell.Util.Verify                   (VerificationRes (..))
import           System.Wlog                            (logDebug)
import           Universum

import           Pos.Binary.Class                       (Bi)
import           Pos.Binary.Crypto                      ()
import           Pos.Communication.Types                (ResponseMode)
import           Pos.Context                            (WithNodeContext (getNodeContext))
import           Pos.DHT.Model                          (ListenerDHT (..))
import           Pos.Security                           (shouldIgnorePkAddress)
import           Pos.Slotting                           (getCurrentSlot)
import           Pos.Ssc.Class.Listeners                (SscListenersClass (..))
import           Pos.Ssc.Extra.MonadLD                  (sscGetLocalPayload)
import           Pos.Ssc.GodTossing.LocalData.LocalData (sscIsDataUseful,
                                                         sscProcessMessage)
import           Pos.Ssc.GodTossing.Types.Base          (Commitment, Opening,
                                                         VssCertificate)
import           Pos.Ssc.GodTossing.Types.Instance      ()
import           Pos.Ssc.GodTossing.Types.Message       (GtMsgContents (..),
                                                         GtMsgTag (..),
                                                         isGoodSlotIdForTag,
                                                         msgContentsTag)
import           Pos.Ssc.GodTossing.Types.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types         (GtPayload (..), GtProof,
                                                         _gpCertificates)
import           Pos.Types.Address                      (StakeholderId)
import           Pos.Util.Relay                         (DataMsg, InvMsg, Relay (..),
                                                         ReqMsg, handleDataL, handleInvL,
                                                         handleReqL)
import           Pos.WorkMode                           (WorkMode)

instance ( Bi VssCertificate
         , Bi Opening
         , Bi Commitment
         , Bi GtPayload
         , Bi GtProof
         , Bi (InvMsg GtMsgTag)
         , Bi (ReqMsg GtMsgTag)
         , Bi (DataMsg GtMsgContents)
         ) =>
         SscListenersClass SscGodTossing where
    sscListeners =
        Tagged
            [ ListenerDHT handleInvGt
            , ListenerDHT handleReqGt
            , ListenerDHT handleDataGt
            ]

handleInvGt :: (Bi (ReqMsg GtMsgTag), ResponseMode SscGodTossing m) => InvMsg GtMsgTag -> m ()
handleInvGt = handleInvL

handleReqGt :: (Bi (DataMsg GtMsgContents), ResponseMode SscGodTossing m) => ReqMsg GtMsgTag -> m ()
handleReqGt = handleReqL

handleDataGt :: (Bi (InvMsg GtMsgTag), ResponseMode SscGodTossing m) => DataMsg GtMsgContents -> m ()
handleDataGt = handleDataL

instance ( WorkMode SscGodTossing m
         ) => Relay m GtMsgTag GtMsgContents where
    contentsToTag = pure . msgContentsTag

    verifyInvTag tag =
      ifM (isGoodSlotIdForTag tag <$> getCurrentSlot)
          (pure VerSuccess)
          (pure $ VerFailure ["slot is not appropriate"])

    verifyReqTag _ = pure VerSuccess

    verifyDataContents dat =
      ifM (isGoodSlotIdForTag (msgContentsTag dat) <$> getCurrentSlot)
          (pure VerSuccess)
          (pure $ VerFailure ["slot is not appropriate"])

    handleInv = sscIsDataUseful
    handleReq tag addr = toContents tag addr <$>
                            (getCurrentSlot >>= sscGetLocalPayload)

    handleData dat addr = do
        -- TODO: Add here malicious emulation for network addresses
        -- when TW will support getting peer address properly
        ifM (not <$> flip shouldIgnorePkAddress addr <$> getNodeContext)
            (sscProcessMessage dat addr) $ True <$
            (logDebug $ sformat
                ("Malicious emulation: data "%build%" for address "%build%" ignored")
                dat addr)

toContents :: GtMsgTag -> StakeholderId -> GtPayload -> Maybe GtMsgContents
toContents CommitmentMsg addr (CommitmentsPayload comm _) =
    MCCommitment <$> lookup addr comm
toContents OpeningMsg addr (OpeningsPayload opens _) =
    MCOpening <$> lookup addr opens
toContents SharesMsg addr (SharesPayload shares _) =
    MCShares <$> lookup addr shares
toContents VssCertificateMsg addr payload =
    MCVssCertificate <$> lookup addr (_gpCertificates payload)
toContents _ _ _ = Nothing
