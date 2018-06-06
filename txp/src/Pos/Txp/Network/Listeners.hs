{-# LANGUAGE CPP        #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Server which handles transactions.
--
-- TODO rename this module. It doesn't define any listeners and doesn't deal
-- with a network.

module Pos.Txp.Network.Listeners
       ( handleTxDo
       , TxpMode
       ) where

import           Data.Tagged (Tagged (..))
import           Formatting (build, sformat, (%))
import           Node.Message.Class (Message)
import           Universum

import           Pos.Binary.Txp ()
import           Pos.Core.Txp (TxAux (..), TxId)
import           Pos.Crypto (hash)
import qualified Pos.Infra.Communication.Relay as Relay
import           Pos.Infra.Util.JsonLog.Events (JLTxR (..))
import           Pos.Txp.MemState (MempoolExt, MonadTxpLocal, MonadTxpMem, txpProcessTx)
import           Pos.Txp.Network.Types (TxMsgContents (..))
import           Pos.Util.Trace (Trace, traceWith)
import           Pos.Util.Trace.Unstructured (LogItem, logInfo)

-- Real tx processing
-- CHECK: @handleTxDo
-- #txProcessTransaction
handleTxDo
    :: TxpMode ctx m
    => Trace m LogItem
    -> Trace m JLTxR    -- ^ How to log transactions
    -> TxAux            -- ^ Incoming transaction to be processed
    -> m Bool
handleTxDo logTrace jsonLogTrace txAux = do
    let txId = hash (taTx txAux)
    res <- txpProcessTx (txId, txAux)
    let json me = traceWith jsonLogTrace $ JLTxR
            { jlrTxId     = sformat build txId
            , jlrError    = me
            }
    case res of
        Right _ -> do
            logInfo logTrace $
                sformat ("Transaction has been added to storage: "%build) txId
            json Nothing
            pure True
        Left er -> do
            logInfo logTrace $
                sformat ("Transaction hasn't been added to storage: "%build%" , reason: "%build) txId er
            json $ Just $ sformat build er
            pure False

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type TxpMode ctx m =
    ( MonadIO m
    , MonadTxpLocal m
    , MonadTxpMem (MempoolExt m) ctx m
    , Each '[Message]
        '[ Relay.InvOrData (Tagged TxMsgContents TxId) TxMsgContents
         , Relay.InvMsg    (Tagged TxMsgContents TxId)
         , Relay.ReqOrRes  (Tagged TxMsgContents TxId)
         , Relay.ReqMsg    (Tagged TxMsgContents TxId)
         , Relay.MempoolMsg TxMsgContents
         ]
    )
