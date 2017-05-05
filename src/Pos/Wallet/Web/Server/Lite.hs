{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for lite-wallet implementation of Daedalus API

module Pos.Wallet.Web.Server.Lite
       ( walletServeWebLite
       , walletServerOuts
       ) where

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import qualified Ether
import           Mockable                      (runProduction)
import           Network.Wai                   (Application)
import           Pos.Communication.Protocol    (SendActions)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import qualified STMContainers.Map             as SM
import           Universum

import           Pos.Communication             (NodeId)
import           Pos.Communication.PeerState   (PeerStateTag, runPeerStateRedirect)
import           Pos.Discovery                 (getPeers, runDiscoveryConstT)
import           Pos.Reporting.MemState        (emptyReportingContext)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Wallet.KeyStorage         (KeyData)
import           Pos.Wallet.State              (getWalletState, runWalletDB)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.WalletMode         (WalletStaticPeersMode,
                                                runBlockchainInfoNotImplemented)
import           Pos.Wallet.Web.Server.Methods (WalletWebHandler, walletApplication,
                                                walletServeImpl, walletServer,
                                                walletServerOuts)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, WalletWebSockets,
                                                getWalletWebSockets, runWalletWS)
import           Pos.Wallet.Web.State          (WalletState, WalletWebDB,
                                                getWalletWebState, runWalletWebDB)
import           System.Wlog                   (usingLoggerName)


type WebHandler = WalletWebSockets (WalletWebDB WalletStaticPeersMode)

type MainWalletState = WS.WalletState

walletServeWebLite
    :: forall ssc.
       SscHelpersClass ssc
    => Proxy ssc
    -> SendActions WalletStaticPeersMode
    -> FilePath
    -> Bool
    -> Word16
    -> WalletStaticPeersMode ()
walletServeWebLite _ sendActions = walletServeImpl action
  where
    action :: WalletWebHandler WalletStaticPeersMode Application
    action = walletApplication $ walletServer sendActions nat

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    wsConn <- getWalletWebSockets
    ws     <- getWalletWebState
    kd     <- Ether.ask'
    mws    <- getWalletState
    peers  <- getPeers
    pure $ NT (convertHandler mws kd ws wsConn peers)

convertHandler
    :: forall a .
       MainWalletState
    -> KeyData
    -> WalletState
    -> ConnectionsVar
    -> Set NodeId
    -> WebHandler a
    -> Handler a
convertHandler mws kd ws wsConn peers handler = do
    stateM <- liftIO SM.newIO
    liftIO ( runProduction
           . usingLoggerName "wallet-lite-api"
           . flip Ether.runReaderT emptyReportingContext
           . runWalletDB mws
           . flip Ether.runReaderT' kd
           . flip (Ether.runReaderT @PeerStateTag) stateM
           . runPeerStateRedirect
           . runBlockchainInfoNotImplemented
           . runDiscoveryConstT peers
           . runWalletWebDB ws
           . runWalletWS wsConn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
