{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for lite-wallet implementation of Daedalus API

module Pos.Wallet.Web.Server.Lite
       ( walletServeWebLite
       , walletServerOuts
       ) where

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Data.Tagged                   (Tagged (..))
import           Ether                         (ask, local)
import qualified Ether
import           Mockable                      (runProduction)
import           Mockable                      (Production)
import           Network.Wai                   (Application)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import qualified STMContainers.Map             as SM
import           Universum

import           Pos.Block.BListener           (runBListenerStub)
import           Pos.Communication             (NodeId)
import           Pos.Communication.PeerState   (PeerStateTag, runPeerStateRedirect)
import           Pos.Communication.Protocol    (SendActions, hoistSendActions)
import           Pos.Context                   (BlkSemaphore, NodeContext, NodeContextTag)
import           Pos.DB                        (NodeDBs)
import           Pos.Discovery                 (getPeers, runDiscoveryConstT)
import           Pos.Reporting.MemState        (ReportingContext, emptyReportingContext)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Wallet.KeyStorage         (KeyData)
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.State              (getWalletState)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.State.Limits       (runDbLimitsWalletRedirect)
import           Pos.Wallet.WalletMode         (FakeSsc, WalletStaticPeersMode,
                                                runBalancesWalletRedirect,
                                                runBlockchainInfoNotImplemented,
                                                runFakeSsc, runTxHistoryWalletRedirect,
                                                runUpdatesNotImplemented)
import           Pos.Wallet.Web.Server.Methods (WalletWebHandler, bracketWalletWS,
                                                bracketWalletWebDB, walletApplication,
                                                walletServeImpl, walletServer,
                                                walletServerOuts)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, getWalletWebSockets,
                                                runWalletWS)
import           Pos.Wallet.Web.State          (WalletState, getWalletWebState,
                                                runWalletWebDB)
import           System.Wlog                   (usingLoggerName)


type WebHandler = WalletWebHandler (WalletStaticPeersMode WalletSscType)

type MainWalletState = WS.WalletState

walletServeWebLite
    :: SscHelpersClass WalletSscType
    => Proxy WalletSscType
    -> SendActions (WalletStaticPeersMode WalletSscType)
    -> FilePath
    -> Bool
    -> Word16
    -> WalletStaticPeersMode WalletSscType ()
walletServeWebLite _ sendActions dbPath dbRebuild port =
    bracketWalletWebDB dbPath dbRebuild $ \db ->
        bracketWalletWS $ \conn -> do
            let runner = runWalletWebDB db . runWalletWS conn
            let hoistedSA :: SendActions (WalletWebHandler (WalletStaticPeersMode WalletSscType))
                hoistedSA = hoistSendActions (lift . lift) runner sendActions
            let action :: WalletWebHandler (WalletStaticPeersMode WalletSscType) Application
                action = walletApplication $ walletServer hoistedSA nat
            runner $ walletServeImpl action port

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    wsConn <- getWalletWebSockets
    ws     <- getWalletWebState
    kd     <- Ether.ask'
    mws    <- getWalletState
    peers  <- getPeers
    pure $ NT (convertHandler mws kd ws wsConn peers)

convertHandler
    :: MainWalletState
    -> KeyData
    -> WalletState
    -> ConnectionsVar
    -> Set NodeId
    -> WebHandler a
    -> Handler a
convertHandler mws kd ws wsConn peers handler = do
    stateM <- liftIO SM.newIO
    liftIO ( runProduction
           . runFakeSsc
           . usingLoggerName "wallet-lite-api"
           . flip Ether.runReadersT
                ( Tagged @PeerStateTag stateM
                , Tagged @KeyData kd
                , Tagged @MainWalletState mws
                , Tagged @ReportingContext emptyReportingContext )
           . runTxHistoryWalletRedirect
           . runBalancesWalletRedirect
           . runDbLimitsWalletRedirect
           . runPeerStateRedirect
           . runUpdatesNotImplemented
           . runBlockchainInfoNotImplemented
           . runBListenerStub
           . runDiscoveryConstT peers
           . runWalletWebDB ws
           . runWalletWS wsConn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

-- Stub implementations for lite wallet.
instance Ether.MonadReader NodeDBs NodeDBs Production where
    ask = error "Stub implementation for Lite Wallet"
    local = error "Stub implementation for Lite Wallet"

instance Ether.MonadReader
             BlkSemaphore
             BlkSemaphore
             (FakeSsc WalletSscType Production) where
    ask = error "Stub implementation for Lite Wallet"
    local = error "Stub implementation for Lite Wallet"

instance Ether.MonadReader
             NodeContextTag
             (NodeContext WalletSscType)
             (FakeSsc WalletSscType Production) where
    ask = error "Stub implementation for Lite Wallet"
    local = error "Stub implementation for Lite Wallet"
