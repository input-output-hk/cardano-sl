{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Server which handles MPC-related things.

module Pos.Ssc.DynamicState.Communication
       ( -- * Instances
         -- ** instance SscListenersClass SscDynamicState
       ) where

import           Control.TimeWarp.Logging      (logDebug, logInfo)
import           Control.TimeWarp.Rpc          (BinaryP, MonadDialog)
import           Data.Tagged                   (Tagged (..))
import           Formatting                    (build, sformat, stext, (%))
import           Universum

import           Pos.Communication.Util        (modifyListenerLogger)
import           Pos.DHT                       (ListenerDHT (..))
import           Pos.Ssc.Class.Listeners       (SscListenersClass (..))
import           Pos.Ssc.DynamicState.Instance (SscDynamicState)
import           Pos.Ssc.DynamicState.Types    (DSMessage (..))
import           Pos.Ssc.DynamicState.Base     (Opening, SignedCommitment, VssCertificate)
import qualified Pos.State                     as St
import           Pos.WorkMode                  (WorkMode)
import           Data.List.NonEmpty            (NonEmpty, nonEmpty)
import           Pos.Crypto                    (PublicKey, Share)
import           Pos.Ssc.DynamicState.Server   (announceCommitments, announceOpenings,
                                                announceVssCertificates)

instance SscListenersClass SscDynamicState where
    sscListeners = Tagged mpcListeners

mpcListeners :: (MonadDialog BinaryP m, WorkMode SscDynamicState m) => [ListenerDHT m]
mpcListeners = map (modifyListenerLogger "mpc") [ListenerDHT handleSsc]

handleSsc :: WorkMode SscDynamicState m => DSMessage -> m ()
handleSsc (DSCommitments comms)     = handleSscDo comms handleCommitment announceCommitments
handleSsc (DSOpenings ops)          = handleSscDo ops handleOpening announceOpenings
handleSsc (DSSharesMulti s)         = mapM_ (uncurry handleShares) s
handleSsc (DSVssCertificates certs) = handleSscDo certs handleCert announceVssCertificates

handleSscDo
    :: WorkMode SscDynamicState m
    => NonEmpty (PublicKey, a)
    -> (PublicKey -> a -> m Bool)
    -> (NonEmpty (PublicKey, a) -> m ())
    -> m ()
handleSscDo items f announce = do
    added <- toList <$> mapM (uncurry f) items
    let addedItems = map snd . filter fst . zip added . toList $ items
    whenJust (nonEmpty addedItems) announce

-- TODO: refactor, lol! :)
handleCommitment :: WorkMode SscDynamicState m => PublicKey -> SignedCommitment -> m Bool
handleCommitment pk c = do
    -- TODO: actually check the commitment
    added <- St.processSscMessage $ DSCommitments $ pure (pk, c)
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Commitment from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    added <$ logAction msg

-- TODO: I don't like that these are in "Server.Mpc" but use 'processOpening'
-- instead of 'mpcProcessOpening' – the idea is that 'mpcProcessOpening' does
-- the MPC part and 'processOpening' may potentially do more than that, so
-- it's counterintuitive that 'handleOpening' is in "Server.Mpc". I'd like to
-- just move all handlers into "Pos.Communication.Server". — @neongreen
handleOpening :: WorkMode SscDynamicState m => PublicKey -> Opening -> m Bool
handleOpening pk o = do
    added <- St.processSscMessage $ DSOpenings $ pure (pk, o)
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Opening from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    added <$ logAction msg

handleShares :: WorkMode SscDynamicState m => PublicKey -> HashMap PublicKey Share -> m ()
handleShares pk s = do
    added <- St.processSscMessage $ DSSharesMulti $ pure (pk, s)
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Shares from "%build%" have been "%stext) pk msgAction
    -- let logAction = if added then logInfo else logDebug
    -- TODO: investigate!
    let logAction = logDebug
    logAction msg

handleCert :: WorkMode SscDynamicState m => PublicKey -> VssCertificate -> m Bool
handleCert pk c = do
    added <- St.processSscMessage $ DSVssCertificates $ pure (pk, c)
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("VssCertificate from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    added <$ logAction msg
