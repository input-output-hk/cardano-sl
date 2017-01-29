{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Instance of SscListenersClass

module Pos.Ssc.GodTossing.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscGodTossing
       ) where

import           Data.HashMap.Strict                    (lookup)
import           Data.Proxy                             (Proxy (..))
import           Data.Tagged                            (Tagged (..))
import           Formatting                             (build, sformat, (%))
import           Serokell.Util.Verify                   (VerificationRes (..))
import           System.Wlog                            (logDebug)
import           Universum

import           Pos.Binary.Communication               ()
import           Pos.Binary.Crypto                      ()
import           Pos.Binary.Relay                       ()
import           Pos.Binary.Ssc                         ()
import           Pos.Communication.Message              ()
import           Pos.Communication.Protocol             (Listener, listenerConv,
                                                         listenerOneMsg)
import           Pos.Communication.Relay                (DataMsg, InvMsg, Relay (..),
                                                         RelayProxy (..), ReqMsg,
                                                         relayListeners,
                                                         relayStubListeners)
import           Pos.Communication.Util                 (stubListenerOneMsg)
import           Pos.Context                            (WithNodeContext (getNodeContext))
import qualified Pos.DB.Lrc                             as LrcDB
import           Pos.Security                           (shouldIgnorePkAddress)
import           Pos.Slotting                           (getCurrentSlot)
import           Pos.Ssc.Class.Listeners                (SscListenersClass (..))
import           Pos.Ssc.Extra.MonadLD                  (sscGetLocalPayload)
import           Pos.Ssc.GodTossing.LocalData.LocalData (sscIsDataUseful,
                                                         sscProcessMessage)
import           Pos.Ssc.GodTossing.Types.Instance      ()
import           Pos.Ssc.GodTossing.Types.Message       (GtMsgContents (..),
                                                         GtMsgTag (..),
                                                         isGoodSlotIdForTag,
                                                         msgContentsTag)
import           Pos.Ssc.GodTossing.Types.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types         (GtPayload (..), _gpCertificates)
import           Pos.Types                              (SlotId (..), StakeholderId)
import           Pos.WorkMode                           (WorkMode)

instance SscListenersClass SscGodTossing where
    sscListeners =
        Tagged $ relayListeners
                    (RelayProxy :: RelayProxy StakeholderId GtMsgTag GtMsgContents)
    sscStubListeners =
        Tagged $ relayStubListeners
                    (RelayProxy :: RelayProxy StakeholderId GtMsgTag GtMsgContents)

instance WorkMode SscGodTossing m
    => Relay m GtMsgTag StakeholderId GtMsgContents where

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

    handleReq tag addr = do
      payload <- getCurrentSlot >>= sscGetLocalPayload
      return $ case payload of Nothing -> Nothing
                               Just p  -> toContents tag addr p

    handleData dat addr = do
        -- [CSL-685] Add here malicious emulation for network addresses
        -- when TW will support getting peer address properly
        ifM (not <$> flip shouldIgnorePkAddress addr <$> getNodeContext)
            (sscProcessMessageRichmen dat addr) $ True <$
            (logDebug $ sformat
                ("Malicious emulation: data "%build%" for address "%build%" ignored")
                dat addr)

sscProcessMessageRichmen :: WorkMode SscGodTossing m
                          => GtMsgContents -> StakeholderId -> m Bool
sscProcessMessageRichmen dat addr = do
    epoch <- siEpoch <$> getCurrentSlot
    richmenMaybe <- LrcDB.getRichmenSsc epoch
    maybe (pure False) (\r -> sscProcessMessage r dat addr) richmenMaybe

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
