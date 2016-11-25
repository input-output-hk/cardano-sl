{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Instance of SscListenersClass

module Pos.Ssc.GodTossing.Listener.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscGodTossing
       ) where

import           Data.HashMap.Strict                    (lookup)
import           Data.List.NonEmpty                     (NonEmpty)
import           Data.Tagged                            (Tagged (..))
import           Formatting                             (build, sformat, stext, (%))
import           System.Wlog                            (logDebug, logInfo)
import           Universum

import           Pos.Communication.Methods              (sendToNeighborsSafe)
import           Pos.Communication.Types                (ResponseMode)
import           Pos.Crypto                             (PublicKey)
import           Pos.DHT                                (ListenerDHT (..), replyToNode)
import           Pos.Slotting                           (getCurrentSlot)
import           Pos.Ssc.Class.Listeners                (SscListenersClass (..))
import           Pos.Ssc.Class.LocalData                (sscGetLocalPayload)
import           Pos.Ssc.GodTossing.LocalData.LocalData (sscIsDataUseful,
                                                         sscProcessMessage)
import           Pos.Ssc.GodTossing.Types.Instance      ()
import           Pos.Ssc.GodTossing.Types.Message       (DataMsg (..), InvMsg (..),
                                                         MsgTag (..), ReqMsg (..),
                                                         dataMsgPublicKey, dataMsgTag,
                                                         isGoodSlotIdForTag)
import           Pos.Ssc.GodTossing.Types.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types         (GtPayload (..), _gpCertificates)
import           Pos.WorkMode                           (WorkMode)

instance SscListenersClass SscGodTossing where
    sscListeners =
        Tagged
            [ ListenerDHT handleInv
            , ListenerDHT handleReq
            , ListenerDHT handleData
            ]

handleInv :: ResponseMode SscGodTossing m => InvMsg -> m ()
handleInv (InvMsg tag keys) =
    ifM (isGoodSlotIdForTag tag <$> getCurrentSlot)
        (handleInvDo tag keys)
        (logDebug $
         sformat ("Ignoring "%build%", because slot is not appropriate") tag)

handleInvDo :: ResponseMode SscGodTossing m => MsgTag -> NonEmpty PublicKey -> m ()
handleInvDo tag keys = mapM_ handleSingle keys
  where
    handleSingle pk =
        ifM (sscIsDataUseful tag pk)
            (replyToNode $ ReqMsg tag pk)
            (logDebug $
             sformat ("Ignoring "%build% " ("%build%"), because it's useless")
                 tag pk)

handleReq :: ResponseMode SscGodTossing m => ReqMsg -> m ()
handleReq (ReqMsg tag key) = do
    localPayload <- sscGetLocalPayload =<< getCurrentSlot
    whenJust (toDataMsg tag key localPayload) (replyToNode @_ @DataMsg)

toDataMsg :: MsgTag -> PublicKey -> GtPayload -> Maybe DataMsg
toDataMsg CommitmentMsg key (CommitmentsPayload comm _) =
    DMCommitment key <$> lookup key comm
toDataMsg OpeningMsg key (OpeningsPayload opens _) =
    DMOpening key <$> lookup key opens
toDataMsg SharesMsg key (SharesPayload shares _) =
    DMShares key <$> lookup key shares
toDataMsg VssCertificateMsg key payload =
    DMVssCertificate key <$> lookup key (_gpCertificates payload)
toDataMsg _ _ _ = Nothing

handleData :: WorkMode SscGodTossing m => DataMsg -> m ()
handleData msg =
    whenM (isGoodSlotIdForTag (dataMsgTag msg) <$> getCurrentSlot) $
    do added <- sscProcessMessage msg
       let tag = dataMsgTag msg
           pk = dataMsgPublicKey msg
       loggerAction tag added pk
       when added $ sendToNeighborsSafe $ InvMsg tag $ pure pk

loggerAction :: WorkMode SscGodTossing m
             => MsgTag -> Bool -> PublicKey -> m ()
loggerAction msgTag added pk = logAction msg
  where
      msgAction | added = "added to local storage"
                | otherwise = "ignored"
      msg = sformat (build%" from "%build%" have/has been "%stext)
          msgTag pk msgAction
      logAction | added = logInfo
                | otherwise = logDebug
