{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Server which handles MPC-related things.

module Pos.Communication.Server.Mpc
       ( -- * Instances
         -- ** instance SscListenersClass SscDynamicState
       ) where

import           Control.TimeWarp.Logging    (logDebug, logInfo)
import           Control.TimeWarp.Rpc        (BinaryP, MonadDialog)
import           Data.Tagged                 (Tagged (..))
import           Formatting                  (build, sformat, stext, (%))
import           Universum

import qualified Pos.Communication.Types.Mpc as Mpc
import           Pos.Communication.Util      (modifyListenerLogger)
import           Pos.DHT                     (ListenerDHT (..))
import           Pos.Ssc.Class.Listeners     (SscListenersClass (..))
import           Pos.Ssc.DynamicState.Types  (DSMessage (..), SscDynamicState)
import qualified Pos.State                   as St
import           Pos.WorkMode                (WorkMode)

instance SscListenersClass SscDynamicState where
    sscListeners = Tagged mpcListeners

mpcListeners :: (MonadDialog BinaryP m, WorkMode m) => [ListenerDHT m]
mpcListeners = map (modifyListenerLogger "mpc") [ListenerDHT handleSsc]

-- TODO: refactor, lol! :)
handleSsc :: WorkMode m => Mpc.SendSsc -> m ()
handleSsc (Mpc.SendCommitment pk c) =  do
    -- TODO: actually check the commitment
    added <- St.processSscMessage $ DSCommitment pk c
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Commitment from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    logAction msg
-- TODO: I don't like that these are in "Server.Mpc" but use 'processOpening'
-- instead of 'mpcProcessOpening' – the idea is that 'mpcProcessOpening' does
-- the MPC part and 'processOpening' may potentially do more than that, so
-- it's counterintuitive that 'handleOpening' is in "Server.Mpc". I'd like to
-- just move all handlers into "Pos.Communication.Server". — @neongreen
handleSsc (Mpc.SendOpening pk o) = do
    added <- St.processSscMessage $ DSOpening pk o
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Opening from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    logAction msg
handleSsc (Mpc.SendShares pk s) = do
    added <- St.processSscMessage $ DSShares pk s
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Shares from "%build%" have been "%stext) pk msgAction
    -- let logAction = if added then logInfo else logDebug
    -- TODO: investigate!
    let logAction = logDebug
    logAction msg
handleSsc (Mpc.SendVssCertificate pk c) = do
    added <- St.processSscMessage $ DSVssCertificate pk c
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("VssCertificate from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    logAction msg
