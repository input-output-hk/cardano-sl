{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server.Methods
       ( walletApplication
       , walletServer
       , walletServeImpl
       , walletServerOuts

       , bracketWalletWebDB
       , bracketWalletWS

       , addInitialRichAccount
       ) where

import           Universum

import           Pos.Wallet.Web      (TlsParams)
import           Pos.Wallet.Web.Mode (MonadWalletWebMode)

walletServeImpl
    :: MonadWalletWebMode m
    => m Application     -- ^ Application getter
    -> Word16            -- ^ Port to listen
    -> Maybe TlsParams
    -> m ()
walletServeImpl app =
    serveImpl app "127.0.0.1"

walletApplication
    :: MonadWalletWebMode m
    => m (Server WalletApi)
    -> m Application
walletApplication serv = do
    wsConn <- getWalletWebSockets
    upgradeApplicationWS wsConn . serve walletApi <$> serv

walletServer
    :: forall ctx m.
       ( MonadWalletWebMode m
       , HasLens GenesisUtxo ctx GenesisUtxo)
    => SendActions m
    -> m (m :~> Handler)
    -> m (Server WalletApi)
walletServer sendActions nat = do
    syncWalletsWithGState @WalletSscType =<< mapM getSKById =<< myRootAddresses
    nat >>= launchNotifier
    (`enter` servantHandlers sendActions) <$> nat

bracketWalletWebDB
    :: ( MonadIO m
       , MonadMask m
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
