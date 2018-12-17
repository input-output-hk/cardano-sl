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

import           Pos.Chain.Genesis as Genesis (Config)
import           Pos.Chain.Txp (TxAux (..), TxId, TxMsgContents (..),
                     TxpConfiguration)
import           Pos.Crypto (hash)
import           Pos.DB.Txp.MemState (MempoolExt, MonadTxpLocal, MonadTxpMem,
                     txpProcessTx)
import qualified Pos.Infra.Communication.Relay as Relay
import           Pos.Infra.Util.JsonLog.Events (JLEvent (..), JLTxR (..))
import           Pos.Util.Wlog (WithLogger, logInfo)

-- Real tx processing
-- CHECK: @handleTxDo
-- #txProcessTransaction
handleTxDo
    :: TxpMode ctx m
    => Genesis.Config
    -> TxpConfiguration
    -> (JLEvent -> m ())  -- ^ How to log transactions
    -> TxAux              -- ^ Incoming transaction to be processed
    -> m Bool
handleTxDo genesisConfig txpConfig logTx txAux = do
    let txId = hash (taTx txAux)
    res <- txpProcessTx genesisConfig txpConfig (txId, txAux)
    let json me = logTx $ JLTxReceived $ JLTxR
            { jlrTxId  = sformat build txId
            , jlrError = me
            }
    case res of
        Right _ -> do
            logInfo $ sformat
                ("Transaction has been added to storage: " % build)
                txId
            json Nothing
            pure True
        Left er -> do
            logInfo $ sformat
                ( "Transaction hasn't been added to storage: "
                % build
                % " , reason: "
                % build
                )
                txId
                er
            json $ Just $ sformat build er
            pure False

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type TxpMode ctx m =
    ( MonadIO m
    , WithLogger m
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
