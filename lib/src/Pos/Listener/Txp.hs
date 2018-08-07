{-# LANGUAGE CPP        #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Server which handles transactions.
--
-- TODO rename this module. It doesn't define any listeners and doesn't deal
-- with a network.

module Pos.Listener.Txp
       ( handleTxDo
       , TxpMode
       ) where

import           Data.Tagged (Tagged (..))
import           Formatting (build, sformat, (%))
import           Node.Message.Class (Message)
import           Universum

import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Core.Txp (TxAux (..), TxId, TxMsgContents (..))
import           Pos.Crypto (ProtocolMagic, hash)
import           Pos.DB.Txp.MemState (MempoolExt, MonadTxpLocal, MonadTxpMem,
                     txpProcessTx)
import qualified Pos.Infra.Communication.Relay as Relay
import           Pos.Infra.Util.JsonLog.Events (JLTxR (..))
import           Pos.Util.Trace (Trace, traceWith)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logInfo)

-- Real tx processing
-- CHECK: @handleTxDo
-- #txProcessTransaction
handleTxDo
    :: TxpMode ctx m
    => TraceNamed m     -- ^ How to log transactions
    -> Trace m JLTxR    -- ^ JSON log
    -> ProtocolMagic
    -> TxpConfiguration
    -> TxAux            -- ^ Incoming transaction to be processed
    -> m Bool
handleTxDo logTrace0 jsonLogTrace pm txpConfig txAux = do
    let logTrace = appendName "handleTxDo" logTrace0
    let txId = hash (taTx txAux)
    res <- txpProcessTx logTrace pm txpConfig (txId, txAux)
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
