{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for full-node implementation of Daedalus API

module Pos.Wallet.Web.Server.Full
       ( walletServeWebFull
       , walletServerOuts
       ) where

import           Universum

import           Control.Concurrent.STM        (TVar)
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Data.Tagged                   (Tagged (..))
import qualified Ether
import           Mockable                      (runProduction)
import           Network.Wai                   (Application)
import           Pos.Slotting.Ntp              (runSlotsRedirect)
import           Pos.Ssc.Extra.Class           (askSscMem)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           System.Wlog                   (logInfo, usingLoggerName)

import           Pos.Communication.PeerState   (PeerStateSnapshot, WithPeerState (..),
                                                getAllStates, peerStateFromSnapshot,
                                                runPeerStateHolder)
import           Pos.Communication.Protocol    (SendActions)
import           Pos.Constants                 (isDevelopment)
import           Pos.Context                   (NodeContext, getNodeContext,
                                                runContextHolder)
import           Pos.Crypto                    (noPassEncrypt)
import           Pos.DB                        (NodeDBs, getNodeDBs)
import           Pos.DB.DB                     (runDbCoreRedirect)
import           Pos.Delegation.Class          (DelegationWrap, askDelegationState)
import           Pos.Delegation.Holder         (runDelegationTFromTVar)
import           Pos.DHT.Real                  (KademliaDHTInstance)
import           Pos.Discovery                 (askDHTInstance, runDiscoveryKademliaT)
import           Pos.Genesis                   (genesisDevSecretKeys)
import           Pos.Slotting                  (NtpSlottingVar, SlottingVar,
                                                askFullNtpSlotting, askSlotting,
                                                runSlotsDataRedirect)
import           Pos.Ssc.Class                 (SscConstraint)
import           Pos.Ssc.Extra                 (SscState, runSscHolder)
import           Pos.Txp                       (GenericTxpLocalData, askTxpMem,
                                                runTxpHolder)
import           Pos.Update.DB                 (runDbLimitsRedirect)
import           Pos.Wallet.KeyStorage         (MonadKeys (..), addSecretKey,
                                                runKeyStorageRedirect)
import           Pos.Wallet.WalletMode         (runBlockchainInfoRedirect,
                                                runUpdatesRedirect)
import           Pos.Wallet.Web.Server.Methods (WalletWebHandler, walletApplication,
                                                walletServeImpl, walletServer,
                                                walletServerOuts)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, WalletWebSockets,
                                                getWalletWebSockets, runWalletWS)
import           Pos.Wallet.Web.State          (WalletState, WalletWebDB, runWalletWebDB)
import           Pos.Wallet.Web.State.State    (getWalletWebState)
import           Pos.WorkMode                  (RawRealModeK, TxpExtra_TMP)


walletServeWebFull
    :: forall ssc.
       (SscConstraint ssc)
    => SendActions (RawRealModeK ssc)
    -> Bool      -- whether to include genesis keys
    -> FilePath  -- to Daedalus acid-state
    -> Bool      -- Rebuild flag
    -> Word16
    -> RawRealModeK ssc ()
walletServeWebFull sendActions debug = walletServeImpl action
  where
    action :: WalletWebHandler (RawRealModeK ssc) Application
    action = do
        logInfo "DAEDALUS has STARTED!"
        when (isDevelopment && debug) $
            mapM_ (addSecretKey . noPassEncrypt) genesisDevSecretKeys
        walletApplication $ walletServer sendActions nat

type WebHandler ssc = WalletWebSockets (WalletWebDB (RawRealModeK ssc))

nat :: WebHandler ssc (WebHandler ssc :~> Handler)
nat = do
    ws         <- getWalletWebState
    tlw        <- askTxpMem
    ssc        <- askSscMem
    delWrap    <- askDelegationState
    psCtx      <- getAllStates
    nc         <- getNodeContext
    modernDB   <- getNodeDBs
    conn       <- getWalletWebSockets
    slotVar    <- askSlotting
    ntpSlotVar <- askFullNtpSlotting
    kinst      <- askDHTInstance
    pure $ NT (convertHandler nc modernDB tlw ssc ws delWrap
                              psCtx conn slotVar ntpSlotVar kinst)

convertHandler
    :: forall ssc a .
       NodeContext ssc              -- (.. insert monad `m` here ..)
    -> NodeDBs
    -> GenericTxpLocalData TxpExtra_TMP
    -> SscState ssc
    -> WalletState
    -> (TVar DelegationWrap)
    -> PeerStateSnapshot
    -> ConnectionsVar
    -> SlottingVar
    -> (Bool, NtpSlottingVar)
    -> KademliaDHTInstance
    -> WebHandler ssc a
    -> Handler a
convertHandler nc modernDBs tlw ssc ws delWrap psCtx
               conn slotVar ntpSlotVar kinst handler = do
    liftIO ( runProduction
           . usingLoggerName "wallet-api"
           . runContextHolder nc
           . flip Ether.runReadersT
                 ( Tagged @NodeDBs modernDBs
                 , Tagged @SlottingVar slotVar
                 , Tagged @(Bool, NtpSlottingVar) ntpSlotVar
                 )
           . runSlotsDataRedirect
           . runSlotsRedirect
           . runSscHolder ssc
           . runTxpHolder tlw
           . runDelegationTFromTVar delWrap
           . (\m -> flip runPeerStateHolder m =<< peerStateFromSnapshot psCtx)
           . runDbLimitsRedirect
           . runDbCoreRedirect
           . runKeyStorageRedirect
           . runUpdatesRedirect
           . runBlockchainInfoRedirect
           . runDiscoveryKademliaT kinst
           . runWalletWebDB ws
           . runWalletWS conn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
