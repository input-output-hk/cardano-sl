{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Instance of SscListenersClass

module Pos.Ssc.GodTossing.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscGodTossing
       ) where

import           Data.HashMap.Strict              (lookup)
import           Data.Tagged                      (Tagged (..))
import           Formatting                       (build, sformat, (%))
import           Serokell.Util.Verify             (VerificationRes (..))
import           System.Wlog                      (logDebug)
import           Universum

import           Pos.Binary.Communication         ()
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Relay                 ()
import           Pos.Binary.Ssc                   ()
import           Pos.Communication.Message        ()
import           Pos.Communication.Relay          (Relay (..), RelayProxy (..),
                                                   relayListeners, relayStubListeners)
import           Pos.Context                      (WithNodeContext (getNodeContext))
import qualified Pos.DB.Lrc                       as LrcDB
import           Pos.Security                     (shouldIgnorePkAddress)
import           Pos.Slotting                     (getCurrentSlot)
import           Pos.Ssc.Class.Listeners          (SscListenersClass (..))
import           Pos.Ssc.Extra                    (sscGetLocalPayload)
import           Pos.Ssc.GodTossing.Core          (GtPayload (..), getCertId,
                                                   getCommitmentsMap, vcSigningKey,
                                                   _gpCertificates)
import           Pos.Ssc.GodTossing.LocalData     (sscIsDataUseful)
import           Pos.Ssc.GodTossing.Toss.Types    (GtMsgTag (..), isGoodSlotIdForTag)
import           Pos.Ssc.GodTossing.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..), msgContentsTag)
import           Pos.Types                        (SlotId (..), StakeholderId,
                                                   addressHash)
import           Pos.WorkMode                     (WorkMode)

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
    contentsToKey x = pure $ case x of
        MCShares k _          -> k
        MCOpening k _         -> k
        MCCommitment (pk,_,_) -> addressHash pk
        MCVssCertificate vc   -> getCertId vc

    verifyInvTag _ = pure VerSuccess

    verifyReqTag _ = pure VerSuccess

    verifyDataContents _ = pure VerSuccess

    handleInv = sscIsDataUseful

    handleReq tag addr =
        toContents tag addr <$> (getCurrentSlot >>= sscGetLocalPayload)

    handleData dat = do
        addr <- contentsToKey dat
        -- [CSL-685] TODO: Add here malicious emulation for network addresses
        -- when TW will support getting peer address properly
        notImplemented
        -- ifM (not <$> flip shouldIgnorePkAddress addr <$> getNodeContext)
        --     (sscProcessMessageRichmen dat) $ True <$
        --     (logDebug $ sformat
        --         ("Malicious emulation: data "%build%" for address "%build%" ignored")
        --         dat addr)

-- sscProcessMessageRichmen :: WorkMode SscGodTossing m
--                           => GtMsgContents -> m Bool
-- sscProcessMessageRichmen dat = do
--     epoch <- siEpoch <$> getCurrentSlot
--     richmenMaybe <- LrcDB.getRichmenSsc epoch
--     maybe (pure False) (handleRichmen . (epoch,)) richmenMaybe
--   where
--     handleRichmen r = do
--         res <- sscProcessMessage r dat
--         case res of
--             Right _ -> pure True
--             Left er -> False <$ logDebug (sformat ("Data is rejected, reason: "%build) er)

toContents :: GtMsgTag -> StakeholderId -> GtPayload -> Maybe GtMsgContents
toContents CommitmentMsg addr (CommitmentsPayload comm _) =
    MCCommitment <$> lookup addr (getCommitmentsMap comm)
toContents OpeningMsg addr (OpeningsPayload opens _) =
    MCOpening addr <$> lookup addr opens
toContents SharesMsg addr (SharesPayload shares _) =
    MCShares addr <$> lookup addr shares
toContents VssCertificateMsg addr payload =
    MCVssCertificate <$> lookup addr (_gpCertificates payload)
toContents _ _ _ = Nothing
