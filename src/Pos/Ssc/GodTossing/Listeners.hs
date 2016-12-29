{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Instance of SscListenersClass

module Pos.Ssc.GodTossing.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscGodTossing
       ) where

import           Control.Monad.Loops                    (andM)
import           Data.HashMap.Strict                    (lookup)
import           Data.List.NonEmpty                     (NonEmpty)
import           Data.Tagged                            (Tagged (..))
import           Formatting                             (build, sformat, stext, (%))
import           System.Wlog                            (logDebug, logInfo)
import           Universum

import           Pos.Binary.Class                       (Bi)
import           Pos.Binary.Crypto                      ()
import           Pos.Communication.Methods              (sendToNeighborsSafe)
import           Pos.Communication.Types                (ResponseMode)
import           Pos.Context                            (WithNodeContext (getNodeContext),
                                                         ncPropagation)
import           Pos.Crypto                             (shortHashF)
import           Pos.DHT.Model                          (ListenerDHT (..), replyToNode)
import           Pos.Security                           (shouldIgnorePkAddress)
import           Pos.Slotting                           (getCurrentSlot)
import           Pos.Ssc.Class.Listeners                (SscListenersClass (..))
import           Pos.Ssc.Extra.MonadLD                  (sscGetLocalPayload)
import           Pos.Ssc.GodTossing.LocalData.LocalData (sscIsDataUseful,
                                                         sscProcessMessage)
import           Pos.Ssc.GodTossing.Types.Base          (Commitment, Opening,
                                                         VssCertificate)
import           Pos.Ssc.GodTossing.Types.Instance      ()
import           Pos.Ssc.GodTossing.Types.Message       (DataMsg (..), InvMsg (..),
                                                         MsgTag (..), ReqMsg (..),
                                                         dataMsgNodeId, dataMsgTag,
                                                         isGoodSlotIdForTag)
import           Pos.Ssc.GodTossing.Types.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types         (GtPayload (..), GtProof,
                                                         _gpCertificates)
import           Pos.Types.Address                      (NodeId)
import           Pos.WorkMode                           (WorkMode)

instance (Bi VssCertificate
         ,Bi Opening
         ,Bi Commitment
         ,Bi GtPayload
         ,Bi GtProof
         ,Bi InvMsg
         ,Bi DataMsg
         ,Bi ReqMsg) =>
         SscListenersClass SscGodTossing where
    sscListeners =
        Tagged
            [ ListenerDHT handleInv
            , ListenerDHT handleReq
            , ListenerDHT handleData
            ]

handleInv :: (ResponseMode SscGodTossing m, Bi ReqMsg) => InvMsg -> m ()
handleInv (InvMsg tag keys) =
    ifM (isGoodSlotIdForTag tag <$> getCurrentSlot)
        (handleInvDo tag keys)
        (logDebug $
         sformat ("Ignoring "%build%", because slot is not appropriate") tag)

handleInvDo :: (Bi ReqMsg) => ResponseMode SscGodTossing m => MsgTag -> NonEmpty NodeId -> m ()
handleInvDo tag keys = mapM_ handleSingle keys
  where
    handleSingle addr =
        ifM (sscIsDataUseful tag addr)
            (replyToNode $ ReqMsg tag addr)
            (logDebug $
             sformat ("Ignoring "%build% " ("%build%"), because it's useless")
                 tag addr)

handleReq :: (Bi DataMsg) => ResponseMode SscGodTossing m => ReqMsg -> m ()
handleReq (ReqMsg tag addr) = do
    localPayload <- sscGetLocalPayload =<< getCurrentSlot
    whenJust (toDataMsg tag addr localPayload) (replyToNode @_ @_ @DataMsg)

toDataMsg :: MsgTag -> NodeId -> GtPayload -> Maybe DataMsg
toDataMsg CommitmentMsg addr (CommitmentsPayload comm _) =
    DMCommitment addr <$> lookup addr comm
toDataMsg OpeningMsg addr (OpeningsPayload opens _) =
    DMOpening addr <$> lookup addr opens
toDataMsg SharesMsg addr (SharesPayload shares _) =
    DMShares addr <$> lookup addr shares
toDataMsg VssCertificateMsg addr payload =
    DMVssCertificate addr <$> lookup addr (_gpCertificates payload)
toDataMsg _ _ _ = Nothing

handleData
    :: (Bi VssCertificate
       ,Bi Opening
       ,Bi Commitment
       ,Bi InvMsg
       ,WorkMode SscGodTossing m)
    => DataMsg -> m ()
handleData msg = do
    let tag = dataMsgTag msg
        nid = dataMsgNodeId msg
    -- TODO: Add here malicious emulation for network addresses
    -- when TW will support getting peer address properly
    whenM (andM [ isGoodSlotIdForTag tag <$> getCurrentSlot
                , not <$> flip shouldIgnorePkAddress nid <$> getNodeContext ]) $ do
        added <- sscProcessMessage msg
        loggerAction tag added nid
        needPropagate <- ncPropagation <$> getNodeContext
        when (added && needPropagate) $
            sendToNeighborsSafe $ InvMsg tag $ pure nid

loggerAction :: WorkMode SscGodTossing m
             => MsgTag -> Bool -> NodeId -> m ()
loggerAction msgTag added pkHash = logAction msg
  where
      msgAction | added = "added to local storage"
                | otherwise = "ignored"
      msg = sformat (build%" from "%shortHashF%" has been "%stext)
          msgTag pkHash msgAction
      logAction | added = logInfo
                | otherwise = logDebug
