{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Instance of SscListenersClass

module Pos.Ssc.DynamicState.Instance.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscDynamicState
       ) where

import           Control.TimeWarp.Logging           (logDebug, logError, logInfo)
import           Control.TimeWarp.Rpc               (BinaryP, MonadDialog)
import           Data.List                          ((\\))
import           Data.List.NonEmpty                 (NonEmpty, nonEmpty)
import           Data.Tagged                        (Tagged (..))
import           Formatting                         (build, sformat, stext, (%))
import           Universum

import           Pos.Communication.Util             (modifyListenerLogger)
import           Pos.Crypto                         (PublicKey)
import           Pos.DHT                            (ListenerDHT (..))
import           Pos.Ssc.Class.Listeners            (SscListenersClass (..))
import           Pos.Ssc.DynamicState.Instance.Type (SscDynamicState)
import           Pos.Ssc.DynamicState.Server        (announceCommitments,
                                                     announceOpenings,
                                                     announceVssCertificates)
import           Pos.Ssc.DynamicState.Types         (DSMessage (..))
import qualified Pos.State                          as St
import           Pos.WorkMode                       (WorkMode)

instance SscListenersClass SscDynamicState where
    sscListeners = Tagged mpcListeners

mpcListeners :: (MonadDialog BinaryP m, WorkMode SscDynamicState m) => [ListenerDHT m]
mpcListeners = map (modifyListenerLogger "mpc") [ListenerDHT handleSsc]

handleSsc :: WorkMode SscDynamicState m => DSMessage -> m ()
handleSsc m@(DSCommitments comms) = do
    processed <- St.processSscMessage m
    let call = handleSscDo "Commitment" announceCommitments comms
    case processed of
        Nothing                     -> call []
        Just (DSCommitments rcomms) -> call $ toList rcomms
        Just x                      -> distConstrsError m x

handleSsc m@(DSOpenings ops)        = do
    processed <- St.processSscMessage m
    let call = handleSscDo "Opening" announceOpenings ops
    case processed of
        Nothing                -> call []
        Just (DSOpenings rops) -> call $ toList rops
        Just x                 -> distConstrsError m x

handleSsc m@(DSSharesMulti s)         = do
    processed <- St.processSscMessage m
    let call p = do
          loggerAction "Shares" True . map fst $ p
          loggerAction "Shares" False . map fst $ (toList s) \\ p
    case processed of
        Nothing                 -> call []
        Just (DSSharesMulti rs) -> call $ toList rs
        Just x                  -> distConstrsError m x

handleSsc m@(DSVssCertificates certs) = do
    processed <- St.processSscMessage m
    let call = handleSscDo "VssCertificate" announceVssCertificates certs
    case processed of
        Nothing                         -> call []
        Just (DSVssCertificates rcerts) -> call $ toList rcerts
        Just x                          -> distConstrsError m x

handleSscDo
    :: (WorkMode SscDynamicState m, Eq a)
    => Text
    -> (NonEmpty (PublicKey, a) -> m ()) --Announce
    -> NonEmpty (PublicKey, a) --NE from DSMessage
    -> [(PublicKey, a)]        --List is returned from sscProcessMessage
    -> m ()
handleSscDo logMsg announce msgs processed = do
    loggerAction logMsg True . map fst $ processed
    loggerAction logMsg False . map fst $ (toList msgs) \\ processed
    whenJust (nonEmpty  processed) announce

loggerAction :: WorkMode SscDynamicState m
             => Text -> Bool -> [PublicKey] -> m ()
loggerAction dsType added pkeys =
    forM_ pkeys $ \pk -> do
      let msgAction = if added then "added to local storage" else "ignored"
      let msg = sformat (build%" from "%build%" has been "%stext) dsType pk msgAction
      let logAction = if added then logInfo else logDebug
      logAction msg

distConstrsError :: WorkMode SscDynamicState m => DSMessage -> DSMessage -> m ()
distConstrsError ex reci = do
    logError $
            sformat ("Internal error: "%stext%" constructor\
                      \was passed to processSscMessage, but "%stext%" is returned")
            (constrName ex) (constrName reci)
    where
        --TODO holyshit probably, DSMessage must derive Data
        constrName (DSCommitments _)     = "DSCommitments"
        constrName (DSOpenings _)        = "DSOpenings"
        constrName (DSSharesMulti _)     = "DSSharesMulti"
        constrName (DSVssCertificates _) = "DSVssCertificates"
