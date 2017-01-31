{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles transactions.

module Pos.Txp.Listeners
       ( txListeners
       , txStubListeners
       , processTx
       ) where

import qualified Data.HashMap.Strict         as HM
import           Formatting                  (build, sformat, stext, (%))
import           Serokell.Util.Verify        (VerificationRes (..))
import           System.Wlog                 (WithLogger, logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Binary.Relay            ()

import           Pos.Communication.Message   ()
import           Pos.Communication.Protocol  (ListenerSpec, OutSpecs)
import           Pos.Communication.Relay     (Relay (..), RelayProxy (..), relayListeners,
                                              relayStubListeners)
import           Pos.Crypto                  (hash)
import           Pos.Statistics              (StatProcessTx (..), statlogCountEvent)
import           Pos.Txp.Class               (getMemPool)
import           Pos.Txp.Logic               (processTx)
import           Pos.Txp.Types.Communication (TxMsgContents (..), TxMsgTag (..))
import           Pos.Txp.Types.Types         (MemPool (..), ProcessTxRes (..))
import           Pos.Types                   (TxAux, TxId)
import           Pos.WorkMode                (WorkMode)

txProxy :: RelayProxy TxId TxMsgTag TxMsgContents
txProxy = RelayProxy

txListeners
    :: WorkMode ssc m
    => ([ListenerSpec m], OutSpecs)
txListeners = relayListeners txProxy

txStubListeners
    :: WithLogger m
    => ([ListenerSpec m], OutSpecs)
txStubListeners = relayStubListeners txProxy

instance ( WorkMode ssc m
         ) => Relay m TxMsgTag TxId TxMsgContents where
    contentsToTag _ = pure TxMsgTag
    contentsToKey (TxMsgContents tx _ _) = pure $ hash tx

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    verifyDataContents _ = pure VerSuccess

    handleInv _ txId = not . HM.member txId  . localTxs <$> getMemPool

    handleReq _ txId = fmap toContents . HM.lookup txId . localTxs <$> getMemPool
      where
        toContents (tx, tw, td) = TxMsgContents tx tw td

    handleData (TxMsgContents tx tw td) = handleTxDo (hash tx, (tx, tw, td))

-- Real tx processing
-- CHECK: @handleTxDo
-- #processTx
handleTxDo
    :: WorkMode ssc m
    => (TxId, TxAux) -> m Bool
handleTxDo tx = do
    res <- processTx tx
    let txId = fst tx
    case res of
        PTRadded -> do
            statlogCountEvent StatProcessTx 1
            logInfo $
                sformat ("Transaction has been added to storage: "%build) txId
        PTRinvalid msg ->
            logWarning $
            sformat ("Transaction "%build%" failed to verify: "%stext) txId msg
        PTRknown ->
            logDebug $ sformat ("Transaction is already known: "%build) txId
        PTRoverwhelmed ->
            logInfo $ sformat ("Node is overwhelmed, can't add tx: "%build) txId
    return (res == PTRadded)
