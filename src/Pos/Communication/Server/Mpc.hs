{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Server which handles MPC-related things.

module Pos.Communication.Server.Mpc
       ( module Mpc
       , mpcListeners
       ) where

import           Control.TimeWarp.Logging    (logDebug, logInfo)
import           Control.TimeWarp.Rpc        (BinaryP, MonadDialog)
import           Formatting                  (build, sformat, stext, (%))
import           Universum

import           Pos.Communication.Types.Mpc as Mpc
import           Pos.Communication.Util      (modifyListenerLogger)
import           Pos.DHT                     (ListenerDHT (..))
import qualified Pos.State                   as St
import           Pos.WorkMode                (WorkMode)

mpcListeners :: (MonadDialog BinaryP m, WorkMode m) => [ListenerDHT m]
mpcListeners =
    map (modifyListenerLogger "mpc")
    [ ListenerDHT handleCommitment
    , ListenerDHT handleOpening
    , ListenerDHT handleShares
    , ListenerDHT handleVssCertificate ]

handleCommitment :: WorkMode m => SendCommitment -> m ()
handleCommitment (SendCommitment pk c) = do
    -- TODO: actually check the commitment
    added <- St.processCommitment pk c
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Commitment from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    logAction msg

-- TODO: I don't like that these are in "Server.Mpc" but use 'processOpening'
-- instead of 'mpcProcessOpening' – the idea is that 'mpcProcessOpening' does
-- the MPC part and 'processOpening' may potentially do more than that, so
-- it's counterintuitive that 'handleOpening' is in "Server.Mpc". I'd like to
-- just move all handlers into "Pos.Communication.Server". — @neongreen
handleOpening :: WorkMode m => SendOpening -> m ()
handleOpening (SendOpening pk o) = do
    added <- St.processOpening pk o
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Opening from "%build%" has been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    logAction msg

handleShares :: WorkMode m => SendShares -> m ()
handleShares (SendShares pk s) = do
    added <- St.processShares pk s
    let msgAction = if added then "added to local storage" else "ignored"
    let msg = sformat ("Shares from "%build%" have been "%stext) pk msgAction
    let logAction = if added then logInfo else logDebug
    logAction msg

handleVssCertificate :: WorkMode m => SendVssCertificate -> m ()
handleVssCertificate (SendVssCertificate pk c) = do
    St.processVssCertificate pk c
