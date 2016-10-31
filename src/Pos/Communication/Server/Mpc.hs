{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Server which handles MPC-related things.

module Pos.Communication.Server.Mpc
       ( module Mpc
       , mpcListeners
       ) where

import           Control.TimeWarp.Logging    (logDebug)
import           Control.TimeWarp.Rpc        (BinaryP, MonadDialog)
import           Formatting                  (build, sformat, (%))
import           Universum

import           Pos.Communication.Types.Mpc as Mpc
import           Pos.Communication.Util      (modifyListenerLogger)
import           Pos.DHT                     (ListenerDHT (..))
import qualified Pos.State                   as St
import           Pos.WorkMode                (WorkMode)

mpcListeners :: (MonadDialog BinaryP m, WorkMode m) => [ListenerDHT m]
mpcListeners =
    map (modifyListenerLogger "tx")
    [ ListenerDHT handleCommitment
    , ListenerDHT handleOpening
    , ListenerDHT handleShares
    , ListenerDHT handleVssCertificate ]

handleCommitment :: WorkMode m => SendCommitment -> m ()
handleCommitment (SendCommitment pk c) = do
    -- TODO: actually check the commitment
    added <- St.processCommitment pk c
    when added $
        logDebug $
        sformat ("Commitment from "%build%" has been added to local storage") pk

-- TODO: I don't like that these are in "Server.Mpc" but use 'processOpening'
-- instead of 'mpcProcessOpening' – the idea is that 'mpcProcessOpening' does
-- the MPC part and 'processOpening' may potentially do more than that, so
-- it's counterintuitive that 'handleOpening' is in "Server.Mpc". I'd like to
-- just move all handlers into "Pos.Communication.Server". — @neongreen
handleOpening :: WorkMode m => SendOpening -> m ()
handleOpening (SendOpening pk o) = do
    added <- St.processOpening pk o
    when added $
        logDebug $
        sformat ("Opening from "%build%" has been added to local storage") pk

handleShares :: WorkMode m => SendShares -> m ()
handleShares (SendShares pk s) = do
    added <- St.processShares pk s
    when added $
        logDebug $
        sformat ("Share from "%build%" has been added to local storage") pk

handleVssCertificate :: WorkMode m => SendVssCertificate -> m ()
handleVssCertificate (SendVssCertificate pk c) = do
    St.processVssCertificate pk c
