{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Instance of SscListenersClass

module Pos.Ssc.DynamicState.Instance.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscDynamicState
       ) where

import           Control.TimeWarp.Logging            (logDebug, logError, logInfo)
import           Data.List                           ((\\))
import           Data.Tagged                         (Tagged (..))
import           Formatting                          (build, sformat, stext, (%))
import           Serokell.Util.Text                  (listJson)
import           Universum

import           Pos.Communication.Methods           (announceSsc)
import           Pos.Crypto                          (PublicKey)
import           Pos.DHT                             (ListenerDHT (..))
import           Pos.Ssc.Class.Listeners             (SscListenersClass (..))
import           Pos.Ssc.DynamicState.Instance.Type  (SscDynamicState)
import           Pos.Ssc.DynamicState.Instance.Types ()
import           Pos.Ssc.DynamicState.Types          (DSMessage (..))
import qualified Pos.State                           as St
import           Pos.WorkMode                        (WorkMode)

instance SscListenersClass SscDynamicState where
    sscListeners = Tagged [ListenerDHT handleSsc]

handleSsc :: WorkMode SscDynamicState m => DSMessage -> m ()
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

dsMessageName :: DSMessage -> Text
dsMessageName (DSCommitments _)     = "commitments"
dsMessageName (DSOpenings _)        = "openings"
dsMessageName (DSSharesMulti _)     = "shares"
dsMessageName (DSVssCertificates _) = "VSS certificates"

checkSameConstrs :: DSMessage -> DSMessage -> Bool
checkSameConstrs (DSCommitments _) (DSCommitments _)         = True
checkSameConstrs (DSOpenings _) (DSOpenings _)               = True
checkSameConstrs (DSSharesMulti _) (DSSharesMulti _)         = True
checkSameConstrs (DSVssCertificates _) (DSVssCertificates _) = True
checkSameConstrs _ _                                         = False

getMessageKeys :: DSMessage -> [PublicKey]
getMessageKeys (DSCommitments l)     = map fst . toList $ l
getMessageKeys (DSOpenings l)        = map fst . toList $ l
getMessageKeys (DSSharesMulti l)     = map fst . toList $ l
getMessageKeys (DSVssCertificates l) = map fst . toList $ l

getAddedAndIgnored :: DSMessage -> Maybe DSMessage -> ([PublicKey], [PublicKey])
getAddedAndIgnored before after = (added, ignored)
  where
    beforeKeys = getMessageKeys before
    afterKeys = maybe [] getMessageKeys after
    added = afterKeys
    ignored = beforeKeys \\ added

loggerAction :: WorkMode SscDynamicState m
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

distConstrsError :: WorkMode SscDynamicState m => DSMessage -> DSMessage -> m ()
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
