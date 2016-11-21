{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Instance of SscListenersClass

module Pos.Ssc.GodTossing.Listener.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscGodTossing
       ) where

import           Data.List                         ((\\))
import           Data.Tagged                       (Tagged (..))
import           Formatting                        (build, sformat, stext, (%))
import           Serokell.Util.Text                (listJson)
import           System.Wlog                       (logDebug, logError, logInfo)
import           Universum

import           Pos.Communication.Methods         (announceSsc)
import           Pos.Crypto                        (PublicKey)
import           Pos.DHT                           (ListenerDHT (..))
import           Pos.Ssc.Class.Listeners           (SscListenersClass (..))
import           Pos.Ssc.GodTossing.Types.Instance ()
import           Pos.Ssc.GodTossing.Types.Message  (DataMsg, InvMsg, ReqMsg)
import           Pos.Ssc.GodTossing.Types.Type     (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types    (GtMessage (..))
import qualified Pos.State                         as St
import           Pos.WorkMode                      (WorkMode)

instance SscListenersClass SscGodTossing where
    sscListeners =
        Tagged
            [ ListenerDHT handleSsc
            , ListenerDHT handleInv
            , ListenerDHT handleReq
            , ListenerDHT handleData
            ]

handleInv :: WorkMode SscGodTossing m => InvMsg -> m ()
handleInv = notImplemented

handleReq :: WorkMode SscGodTossing m => ReqMsg -> m ()
handleReq = notImplemented

handleData :: WorkMode SscGodTossing m => DataMsg -> m ()
handleData = notImplemented

----------------------------------------------------------------------------
-- Old
----------------------------------------------------------------------------

handleSsc :: WorkMode SscGodTossing m => GtMessage -> m ()
handleSsc m = do
    processed <- St.processSscMessage m
    let msgName = dsMessageName m
    let (added, ignored) = getAddedAndIgnored m processed
    loggerAction msgName True added
    loggerAction msgName False ignored
    let call p
            | checkSameConstrs m p = announceSsc p
            | otherwise = distConstrsError m p
    whenJust processed call

dsMessageName :: GtMessage -> Text
dsMessageName (DSCommitments _)     = "commitments"
dsMessageName (DSOpenings _)        = "openings"
dsMessageName (DSSharesMulti _)     = "shares"
dsMessageName (DSVssCertificates _) = "VSS certificates"

checkSameConstrs :: GtMessage -> GtMessage -> Bool
checkSameConstrs (DSCommitments _) (DSCommitments _)         = True
checkSameConstrs (DSOpenings _) (DSOpenings _)               = True
checkSameConstrs (DSSharesMulti _) (DSSharesMulti _)         = True
checkSameConstrs (DSVssCertificates _) (DSVssCertificates _) = True
checkSameConstrs _ _                                         = False

getMessageKeys :: GtMessage -> [PublicKey]
getMessageKeys (DSCommitments l)     = map fst . toList $ l
getMessageKeys (DSOpenings l)        = map fst . toList $ l
getMessageKeys (DSSharesMulti l)     = map fst . toList $ l
getMessageKeys (DSVssCertificates l) = map fst . toList $ l

getAddedAndIgnored :: GtMessage -> Maybe GtMessage -> ([PublicKey], [PublicKey])
getAddedAndIgnored before after = (added, ignored)
  where
    beforeKeys = getMessageKeys before
    afterKeys = maybe [] getMessageKeys after
    added = afterKeys
    ignored = beforeKeys \\ added

loggerAction :: WorkMode SscGodTossing m
             => Text -> Bool -> [PublicKey] -> m ()
loggerAction _ _ [] = pass
loggerAction dsType added pkeys = logAction msg
  where
      msgAction | added = "added to local storage"
                | otherwise = "ignored"
      msg = sformat (build%" from "%listJson%" have been "%stext)
          dsType pkeys msgAction
      logAction | added = logInfo
                | otherwise = logDebug

distConstrsError :: WorkMode SscGodTossing m => GtMessage -> GtMessage -> m ()
distConstrsError ex reci = do
    logError $
            sformat ("Internal error: "%stext%" constructor\
                      \was passed to processSscMessage, but "%stext%" is returned")
            (constrName ex) (constrName reci)
    where
        -- Better aprooach is to derive Data.Data instance
        constrName (DSCommitments _)     = "DSCommitments"
        constrName (DSOpenings _)        = "DSOpenings"
        constrName (DSSharesMulti _)     = "DSSharesMulti"
        constrName (DSVssCertificates _) = "DSVssCertificates"
