{-# LANGUAGE CPP #-}

-- | Server which handles transactions.

module Pos.Txp.Network.Listeners
       ( txListeners
       , txStubListeners
       ) where

import qualified Data.HashMap.Strict        as HM
import           Formatting                 (build, sformat, (%))
import           Serokell.Util.Verify       (VerificationRes (..))
import           System.Wlog                (WithLogger, logInfo)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Binary.Relay           ()
import           Pos.Communication.Limits   ()
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (ListenerSpec, OutSpecs, NodeId (..))
import           Pos.Communication.Relay    (Relay (..), RelayProxy (..), relayListeners,
                                             relayStubListeners)
import           Pos.Crypto                 (hash)
import           Pos.Statistics             (StatProcessTx (..), statlogCountEvent)
import           Pos.Txp.Core.Types         (TxAux, TxId)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Local     (eTxProcessTransaction)
#else
import           Pos.Txp.Logic              (txProcessTransaction)
#endif
import           Pos.Txp.MemState           (getMemPool, TransactionProvenance (..))
import           Pos.Txp.Network.Types      (TxMsgContents (..), TxMsgTag (..))
import           Pos.Txp.Toil.Types         (MemPool (..))
import           Pos.Util.JsonLog           (MonadJL (..), JLEvent (..), JLTxR (..))
import           Pos.Util.TimeWarp          (currentTime)
import           Pos.WorkMode               (WorkMode)

txProxy :: RelayProxy TxId TxMsgTag TxMsgContents
txProxy = RelayProxy

txListeners
    :: WorkMode ssc m
    => m ([ListenerSpec m], OutSpecs)
txListeners = relayListeners txProxy

txStubListeners
    :: WithLogger m
    => ([ListenerSpec m], OutSpecs)
txStubListeners = relayStubListeners txProxy

instance ( WorkMode ssc m
         ) => Relay m TxMsgTag TxId TxMsgContents where
    contentsToTag _ = pure TxMsgTag
    contentsToKey (TxMsgContents tx _ _) = pure $ hash tx

    verifyInvTag       _ = pure VerSuccess
    verifyReqTag       _ = pure VerSuccess
    verifyMempoolTag   _ = pure VerSuccess
    verifyDataContents _ = pure VerSuccess

    handleInv _ _ txId = not . HM.member txId  . _mpLocalTxs <$> getMemPool

    handleReq _ _ txId =
        fmap toContents . HM.lookup txId . _mpLocalTxs <$> getMemPool
      where
        toContents (tx, tw, td) = TxMsgContents tx tw td

    handleMempool _ _ = HM.keys . _mpLocalTxs <$> getMemPool

    handleData peer (TxMsgContents tx tw td) = handleTxDo peer (hash tx, (tx, tw, td))

-- Real tx processing
-- CHECK: @handleTxDo
-- #txProcessTransaction
handleTxDo
    :: WorkMode ssc m
    => NodeId
    -> (TxId, TxAux)
    -> m Bool
handleTxDo (NodeId (peer, _)) tx = do
    ts <- currentTime
#ifdef WITH_EXPLORER
    res <- runExceptT $ eTxProcessTransaction (FromPeer peer) tx
#else
    res <- runExceptT $ txProcessTransaction (FromPeer peer) tx
#endif
    let txId = fst tx
    let json me = jlLog $ JLTxReceived $ JLTxR
            { jlrTxId     = sformat build txId
            , jlrReceived = fromIntegral ts
            , jlrError    = me
            }
    case res of
        Right _ -> do
            statlogCountEvent StatProcessTx 1
            logInfo $
                sformat ("Transaction has been added to storage: "%build) txId
            json Nothing
            pure True
        Left er -> do
            logInfo $
                sformat ("Transaction hasn't been added to storage: "%build%" , reason: "%build) txId er
            json $ Just $ sformat build er
            pure False
