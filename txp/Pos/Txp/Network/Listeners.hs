{-# LANGUAGE CPP        #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Server which handles transactions.

module Pos.Txp.Network.Listeners
       ( txRelays
       , txInvReqDataParams
       , JLTxR (..)
       ) where

import qualified Data.HashMap.Strict as HM
import           Data.Tagged (Tagged (..), tagWith)
import           Formatting (build, sformat, (%))
import           Node.Message.Class (Message)
import           System.Wlog (WithLogger, logInfo)
import           Universum

import           Pos.Binary.Txp ()
import           Pos.Communication.Limits.Types (MessageLimited)
import           Pos.Communication.Relay (InvReqDataParams (..), MempoolParams (..), Relay (..))
import qualified Pos.Communication.Relay as Relay
import           Pos.Communication.Types.Protocol (MsgType (..))
import           Pos.Core.Txp (TxAux (..), TxId)
import           Pos.Crypto (hash)
import           Pos.Txp.MemState (MempoolExt, MonadTxpLocal, MonadTxpMem, getMemPool, txpProcessTx)
import           Pos.Txp.Network.Types (TxMsgContents (..))
import           Pos.Txp.Toil.Types (MemPool (..))

txInvReqDataParams
    :: TxpMode ctx m
    => (JLTxR -> m ())  -- ^ How to log transactions
    -> InvReqDataParams (Tagged TxMsgContents TxId) TxMsgContents m
txInvReqDataParams logTx =
    InvReqDataParams
       { invReqMsgType = MsgTransaction
       , contentsToKey = txContentsToKey
       , handleInv = \_ -> txHandleInv
       , handleReq = \_ -> txHandleReq
       , handleData = \_ -> txHandleData
       }
  where
    txContentsToKey = pure . Tagged . hash . taTx . getTxMsgContents
    txHandleInv (Tagged txId) =
        not . HM.member txId  . _mpLocalTxs <$> getMemPool
    txHandleReq (Tagged txId) =
        fmap TxMsgContents . HM.lookup txId . _mpLocalTxs <$> getMemPool
    txHandleData (TxMsgContents txAux) =
        handleTxDo logTx txAux

txRelays
    :: TxpMode ctx m
    => (JLTxR -> m ())  -- ^ How to log transactions
    -> [Relay m]
txRelays logTx = pure $
    InvReqData (KeyMempool (Proxy :: Proxy TxMsgContents)
                           (map tag . HM.keys . _mpLocalTxs <$> getMemPool)) $
               (txInvReqDataParams logTx)
  where
    tag = tagWith (Proxy :: Proxy TxMsgContents)

-- Real tx processing
-- CHECK: @handleTxDo
-- #txProcessTransaction
handleTxDo
    :: TxpMode ctx m
    => (JLTxR -> m ())  -- ^ How to log transactions
    -> TxAux            -- ^ Incoming transaction to be processed
    -> m Bool
handleTxDo logTx txAux = do
    let txId = hash (taTx txAux)
    res <- txpProcessTx (txId, txAux)
    let json me = logTx $ JLTxR
            { jlrTxId     = sformat build txId
            , jlrError    = me
            }
    case res of
        Right _ -> do
            logInfo $
                sformat ("Transaction has been added to storage: "%build) txId
            json Nothing
            pure True
        Left er -> do
            logInfo $
                sformat ("Transaction hasn't been added to storage: "%build%" , reason: "%build) txId er
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
    , MessageLimited (Relay.DataMsg TxMsgContents) m
    )

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

-- | Json log of one transaction being received by a node.
data JLTxR = JLTxR
    { jlrTxId  :: Text
    , jlrError :: Maybe Text
    } deriving Show
