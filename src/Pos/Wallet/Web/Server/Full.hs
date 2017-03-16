{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for full-node implementation of Daedalus API

module Pos.Wallet.Web.Server.Full
       ( walletServeWebFull
       , walletServerOuts
       ) where

import           Control.Concurrent.STM        (TVar)
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Mockable                      (runProduction)
import           Network.Wai                   (Application)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           System.Wlog                   (logInfo, usingLoggerName)
import           Universum

import           Pos.Communication.PeerState   (PeerStateSnapshot, WithPeerState (..),
                                                getAllStates, peerStateFromSnapshot,
                                                runPeerStateHolder)
import           Pos.Communication.Protocol    (SendActions)
import           Pos.Constants                 (isDevelopment)
import           Pos.Context                   (NodeContext, getNodeContext,
                                                runContextHolder)
import           Pos.Crypto                    (toEncrypted)
import           Pos.DB                        (NodeDBs, getNodeDBs, runDBHolder)
import           Pos.Delegation.Class          (DelegationWrap, askDelegationState)
import           Pos.Delegation.Holder         (runDelegationTFromTVar)
import           Pos.DHT.Real.Real             (runKademliaDHT)
import           Pos.DHT.Real.Types            (KademliaDHTInstance (..),
                                                getKademliaDHTInstance)
import           Pos.Genesis                   (genesisDevSecretKeys)
import           Pos.Slotting                  (NtpSlotting (..), NtpSlottingVar,
                                                SlottingHolder (..), SlottingVar,
                                                runNtpSlotting, runSlottingHolder)
import           Pos.Ssc.Class                 (SscConstraint)
import           Pos.Ssc.Extra                 (SscHolder (..), SscState, runSscHolder)
import           Pos.Txp                       (GenericTxpLocalData, askTxpMem,
                                                runTxpHolder)
import           Pos.Update.MemState.Holder    (runUSHolder)
import           Pos.Wallet.KeyStorage         (MonadKeys (..), addSecretKey)
import           Pos.Wallet.WalletMode         (MonadTxHistory)
import           Pos.Wallet.Web.Server.Methods (WalletWebHandler, walletApplication,
                                                walletServeImpl, walletServer,
                                                walletServerOuts)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar,
                                                MonadWalletWebSockets (..),
                                                WalletWebSockets, runWalletWS)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletState,
                                                WalletWebDB, runWalletWebDB)
import           Pos.WorkMode                  (VileRealMode)

walletServeWebFull
    :: forall txp ssc.
       (SscConstraint ssc, MonadTxHistory (VileRealMode txp ssc))
    => SendActions (VileRealMode txp ssc)
    -> Bool      -- whether to include genesis keys
    -> FilePath  -- to Daedalus acid-state
    -> Bool      -- Rebuild flag
    -> Word16
    -> VileRealMode txp ssc ()
walletServeWebFull sendActions debug = walletServeImpl action
  where
    action :: WalletWebHandler (VileRealMode txp ssc) Application
    action = do
        logInfo "DAEDALUS has STARTED!"
        when (isDevelopment && debug) $
            mapM_ (addSecretKey . toEncrypted) genesisDevSecretKeys
        walletApplication $ walletServer @ssc sendActions nat

type WebHandler txp ssc = WalletWebSockets (WalletWebDB (VileRealMode txp ssc))

nat :: WebHandler txp ssc (WebHandler txp ssc :~> Handler)
nat = do
    ws         <- getWalletWebState
    kinst      <- lift . lift . lift $ getKademliaDHTInstance
    tlw        <- askTxpMem
    ssc        <- lift . lift . lift . lift . lift . lift . lift $ SscHolder ask
    delWrap    <- askDelegationState
    psCtx      <- lift . lift $ getAllStates
    nc         <- getNodeContext
    modernDB   <- getNodeDBs
    conn       <- getWalletWebSockets
    slotVar    <- lift . lift . lift . lift . lift . lift . lift . lift . lift $ SlottingHolder ask
    ntpSlotVar <- lift . lift . lift . lift . lift . lift . lift . lift $ NtpSlotting ask
    pure $ Nat (convertHandler kinst nc modernDB tlw ssc ws delWrap psCtx conn slotVar ntpSlotVar)

convertHandler
    :: forall txp ssc a .
       KademliaDHTInstance
    -> NodeContext ssc              -- (.. insert monad `m` here ..)
    -> NodeDBs
    -> GenericTxpLocalData txp
    -> SscState ssc
    -> WalletState
    -> (TVar DelegationWrap)
    -> PeerStateSnapshot
    -> ConnectionsVar
    -> SlottingVar
    -> NtpSlottingVar
    -> WebHandler txp ssc a
    -> Handler a
convertHandler kinst nc modernDBs tlw ssc ws delWrap psCtx conn slotVar ntpSlotVar handler = do
    liftIO ( runProduction
           . usingLoggerName "wallet-api"
           . runDBHolder modernDBs
           . runContextHolder nc
           . runSlottingHolder slotVar
           . runNtpSlotting ntpSlotVar
           . runSscHolder ssc
           . runTxpHolder tlw
           . runDelegationTFromTVar delWrap
           . runUSHolder
           . runKademliaDHT kinst
           . (\m -> flip runPeerStateHolder m =<< peerStateFromSnapshot psCtx)
           . runWalletWebDB ws
           . runWalletWS conn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
