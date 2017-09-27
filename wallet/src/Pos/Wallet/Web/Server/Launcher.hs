{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Server launcher

module Pos.Wallet.Web.Server.Launcher
       ( walletApplication
       , walletServer
       , walletServeImpl
       , walletServerOuts

       , bracketWalletWebDB
       , bracketWalletWS
       ) where

import           Universum

import           Network.Wai                      (Application)
import           Serokell.AcidState.ExtendedState (ExtendedState)
import           Servant.Server                   (Handler, Server, serve)
import           Servant.Utils.Enter              ((:~>) (..))

import qualified Data.ByteString.Char8            as BS8
import           Pos.Communication                (OutSpecs, SendActions (..), sendTxOuts)
import           Pos.Launcher.Configuration       (HasConfigurations)
import           Pos.Util.TimeWarp                (NetworkAddress)
import           Pos.Wallet.SscType               (WalletSscType)
import           Pos.Wallet.Web.Account           (findKey, myRootAddresses)
import           Pos.Wallet.Web.Api               (WalletSwaggerApi, swaggerWalletApi)
import           Pos.Wallet.Web.Mode              (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending           (startPendingTxsResubmitter)
import           Pos.Wallet.Web.Server.Handlers   (servantHandlersWithSwagger)
import           Pos.Wallet.Web.Sockets           (ConnectionsVar, closeWSConnections,
                                                   getWalletWebSockets, initWSConnections,
                                                   launchNotifier, upgradeApplicationWS)
import           Pos.Wallet.Web.State             (closeState, openState)
import           Pos.Wallet.Web.State.Storage     (WalletStorage)
import           Pos.Wallet.Web.Tracking          (syncWalletsWithGState)
import           Pos.Web                          (TlsParams, serveImpl)

-- TODO [CSM-407]: Mixture of logic seems to be here

walletServeImpl
    :: MonadWalletWebMode m
    => m Application     -- ^ Application getter
    -> NetworkAddress    -- ^ IP and port to listen
    -> Maybe TlsParams
    -> m ()
walletServeImpl app (ip, port) =
    serveImpl app (BS8.unpack ip) port

walletApplication
    :: MonadWalletWebMode m
    => m (Server WalletSwaggerApi)
    -> m Application
walletApplication serv = do
    wsConn <- getWalletWebSockets
    upgradeApplicationWS wsConn . serve swaggerWalletApi <$> serv

walletServer
    :: forall m.
       ( MonadWalletWebMode m )
    => SendActions m
    -> m (m :~> Handler)
    -> m (Server WalletSwaggerApi)
walletServer sendActions natM = do
    nat <- natM
    syncWalletsWithGState @WalletSscType =<< mapM findKey =<< myRootAddresses
    startPendingTxsResubmitter sendActions
    launchNotifier nat
    return $ servantHandlersWithSwagger sendActions nat

bracketWalletWebDB
    :: ( MonadIO m
       , MonadMask m
       , HasConfigurations
       )
    => FilePath  -- ^ Path to wallet acid-state
    -> Bool      -- ^ Rebuild flag for acid-state
    -> (ExtendedState WalletStorage -> m a)
    -> m a
bracketWalletWebDB daedalusDbPath dbRebuild =
    bracket (openState dbRebuild daedalusDbPath)
            closeState

bracketWalletWS
    :: ( MonadIO m
       , MonadMask m
       )
    => (ConnectionsVar -> m a)
    -> m a
bracketWalletWS = bracket initWS closeWSConnections
  where
    initWS = putText "walletServeImpl initWsConnection" >> initWSConnections

walletServerOuts :: OutSpecs
walletServerOuts = sendTxOuts
