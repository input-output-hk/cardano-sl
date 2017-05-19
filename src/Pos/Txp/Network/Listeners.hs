{-# LANGUAGE CPP #-}

-- | Server which handles transactions.

module Pos.Txp.Network.Listeners
       ( txListeners
       , txRelay
       , txInvReqDataParams
       ) where

import qualified Data.HashMap.Strict        as HM
import           Data.Tagged                (Tagged (..), tagWith)
import           Formatting                 (build, sformat, (%))
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Binary.Relay           ()
import           Pos.Communication.Limits   ()
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (ListenerSpec, OutSpecs)
import           Pos.Communication.Relay    (InvReqDataParams (..), MempoolParams (..),
                                             Relay (..), relayListeners)
import           Pos.Crypto                 (hash)
import           Pos.Statistics             (StatProcessTx (..), statlogCountEvent)
import           Pos.Txp.Core.Types         (TxAux (..), TxId)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Local     (eTxProcessTransaction)
#else
import           Pos.Txp.Logic              (txProcessTransaction)
#endif
import           Pos.Txp.MemState           (getMemPool)
import           Pos.Txp.Network.Types      (TxMsgContents (..))
import           Pos.Txp.Toil.Types         (MemPool (..))
import           Pos.WorkMode.Class         (WorkMode)

txListeners
    :: WorkMode ssc m
    => m ([ListenerSpec m], OutSpecs)
txListeners = relayListeners txRelay

txInvReqDataParams :: WorkMode ssc m
    => InvReqDataParams (Tagged TxMsgContents TxId) TxMsgContents m
txInvReqDataParams =
    InvReqDataParams
       { contentsToKey = txContentsToKey
       , handleInv = txHandleInv
       , handleReq = txHandleReq
       , handleData = txHandleData
       }
  where
    txContentsToKey = pure . Tagged . hash . taTx . getTxMsgContents
    txHandleInv (Tagged txId) =
        not . HM.member txId  . _mpLocalTxs <$> getMemPool
    txHandleReq (Tagged txId) =
        fmap TxMsgContents . HM.lookup txId . _mpLocalTxs <$> getMemPool
    txHandleData (TxMsgContents txAux) = handleTxDo txAux

txRelay
    :: WorkMode ssc m
    => Relay m
txRelay =
    InvReqData (KeyMempool (Proxy :: Proxy TxMsgContents)
                           (map tag . HM.keys . _mpLocalTxs <$> getMemPool)) $
               txInvReqDataParams
  where
    tag = tagWith (Proxy :: Proxy TxMsgContents)

-- Real tx processing
-- CHECK: @handleTxDo
-- #txProcessTransaction
handleTxDo
    :: WorkMode ssc m
    => TxAux -> m Bool
handleTxDo txAux = do
    let txId = hash (taTx txAux)
#ifdef WITH_EXPLORER
    res <- runExceptT $ eTxProcessTransaction (txId, txAux)
#else
    res <- runExceptT $ txProcessTransaction (txId, txAux)
#endif
    case res of
        Right _ -> do
            statlogCountEvent StatProcessTx 1
            logInfo $
                sformat ("Transaction has been added to storage: "%build) txId
            pure True
        Left er -> do
            logInfo $
                sformat ("Transaction hasn't been added to storage: "%build%" , reason: "%build) txId er
            pure False
