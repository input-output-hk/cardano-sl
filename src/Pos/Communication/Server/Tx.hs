{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server which handles transactions.

module Pos.Communication.Server.Tx
       ( txListeners
       ) where

import           Control.TimeWarp.Logging (logError, logInfo)
import           Control.TimeWarp.Rpc     (BinaryP, MonadDialog)
import           Formatting               (build, sformat, (%))
import           Universum

import           Pos.Communication.Types  (ResponseMode, SendTx (..), SendTxs (..))
import           Pos.Communication.Util   (modifyListenerLogger)
import           Pos.DHT                  (ListenerDHT (..))
import           Pos.State                (processTx)
import           Pos.Statistics           (statlogReceivedTx)
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
    success <- processTx tx
    if success
        then logInfo $ sformat ("Transaction has been added to storage: "%build) tx
        else logError $ sformat ("Transaction FAILED to verify! "%build) tx

handleTxs
    :: ResponseMode m
    => SendTxs -> m ()
handleTxs (SendTxs txs) = mapM_ (handleTx . SendTx) txs
