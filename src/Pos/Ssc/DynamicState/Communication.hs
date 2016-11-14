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
import qualified Pos.State                     as St
import           Pos.WorkMode                  (WorkMode)

instance SscListenersClass SscDynamicState where
    sscListeners = Tagged mpcListeners

mpcListeners :: (MonadDialog BinaryP m, WorkMode SscDynamicState m) => [ListenerDHT m]
mpcListeners = map (modifyListenerLogger "mpc") [ListenerDHT handleSsc]

-- TODO: refactor, lol! :)
handleSsc :: WorkMode SscDynamicState m => DSMessage -> m ()
handleSsc commitment@(DSCommitment pk _) =  do
    -- TODO: actually check the commitment
    added <- St.processSscMessage commitment
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Commitment from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    logAction msg
-- TODO: I don't like that these are in "Server.Mpc" but use 'processOpening'
-- instead of 'mpcProcessOpening' – the idea is that 'mpcProcessOpening' does
-- the MPC part and 'processOpening' may potentially do more than that, so
-- it's counterintuitive that 'handleOpening' is in "Server.Mpc". I'd like to
-- just move all handlers into "Pos.Communication.Server". — @neongreen
handleSsc opening@(DSOpening pk _) = do
    added <- St.processSscMessage opening
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Opening from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    logAction msg
handleSsc shares@(DSShares pk _) = do
    added <- St.processSscMessage shares
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Shares from "%build%" have been "%stext) pk msgAction
    -- let logAction = if added then logInfo else logDebug
    -- TODO: investigate!
    let logAction = logDebug
    logAction msg
handleSsc vssCertificate@(DSVssCertificate pk _) = do
    added <- St.processSscMessage vssCertificate
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("VssCertificate from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    logAction msg
