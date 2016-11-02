{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server which handles transactions.

module Pos.Communication.Server.Tx
       ( txListeners
       ) where

import           Control.TimeWarp.Logging (logDebug, logError, logInfo)
import           Control.TimeWarp.Rpc     (BinaryP, MonadDialog)
import           Formatting               (build, sformat, stext, (%))
import           Universum

import           Pos.Communication.Types  (ResponseMode, SendTx (..), SendTxs (..))
import           Pos.Communication.Util   (modifyListenerLogger)
import           Pos.DHT                  (ListenerDHT (..))
import           Pos.State                (ProcessTxRes (..), processTx)
import           Pos.Statistics           (statlogReceivedTx)
import           Pos.Types                (txF)
import           Pos.WorkMode             (WorkMode)

-- | Listeners for requests related to blocks processing.
txListeners :: (MonadDialog BinaryP m, WorkMode m) => [ListenerDHT m]
txListeners =
    map (modifyListenerLogger "tx")
    [ ListenerDHT handleTx
    , ListenerDHT handleTxs
    ]

handleTx
    :: ResponseMode m
    => SendTx -> m ()
handleTx (SendTx tx) = do
    statlogReceivedTx tx
    res <- processTx tx
    case res of
        PTRadded ->
            logInfo $
            sformat ("Transaction has been added to storage: " %build) tx
        PTRinvalid msg ->
            logError $ sformat ("Transaction "%txF%" failed to verify: "%stext) tx msg
        PTRknown ->
            logDebug $ sformat ("Transaction is already known: " %build) tx

handleTxs
    :: ResponseMode m
    => SendTxs -> m ()
handleTxs (SendTxs txs) = mapM_ (handleTx . SendTx) txs
