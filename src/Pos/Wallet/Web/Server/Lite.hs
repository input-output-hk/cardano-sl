{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for lite-wallet implementation of Daedalus API

module Pos.Wallet.Web.Server.Lite
       ( walletServeWebLite
       , walletServerOuts
       ) where

import qualified Control.Monad.Catch           as Catch
import qualified Control.Monad.Ether           as Ether.E
import qualified Control.Monad.Ether.Implicit  as Ether
import           Control.Monad.Except          (MonadError (throwError))
import           Mockable                      (Production, runProduction)
import           Network.Wai                   (Application)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import qualified STMContainers.Map             as SM
import           Universum

import           Pos.Communication             (NodeId)
import           Pos.Communication.PeerState   (runPeerStateHolder)
import           Pos.Communication.Protocol    (SendActions, hoistSendActions)
import           Pos.Context                   (NodeContext)
import           Pos.DB                        (NodeDBs)
import           Pos.Discovery                 (getPeers, runDiscoveryConstT)
import           Pos.Reporting.MemState        (runWithoutReportingContext)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Util.Context              (ContextTagK (..))
import           Pos.Wallet                    (WalletSscType)
import           Pos.Wallet.KeyStorage         (KeyData, runKeyStorageRaw)
import           Pos.Wallet.State              (getWalletState, runWalletDB)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.WalletMode         (FakeSsc, WalletStaticPeersMode,
                                                runFakeSsc)
import           Pos.Wallet.Web.Server.Methods (WalletWebHandler, bracketWalletWS,
                                                bracketWalletWebDB, walletApplication,
                                                walletServeImpl, walletServer,
                                                walletServerOuts)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, WalletWebSockets,
                                                getWalletWebSockets, runWalletWS)
import           Pos.Wallet.Web.State          (WalletState, WalletWebDB,
                                                getWalletWebState, runWalletWebDB)
import           System.Wlog                   (usingLoggerName)


type WebHandler = WalletWebSockets (WalletWebDB (WalletStaticPeersMode WalletSscType))

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
    kd     <- Ether.ask
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
           . runWithoutReportingContext
           . runWalletDB mws
           . flip runKeyStorageRaw kd
           . runPeerStateHolder stateM
           . runDiscoveryConstT peers
           . runWalletWebDB ws
           . runWalletWS wsConn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

-- Stub implementations for lite wallet.
instance Ether.E.MonadReader NodeDBs NodeDBs Production where
    ask = error "Stub implementation for Lite Wallet"
    local = error "Stub implementation for Lite Wallet"

instance Ether.E.MonadReader
             'ContextTag
             (NodeContext WalletSscType)
             (FakeSsc WalletSscType Production) where
    ask = error "Stub implementation for Lite Wallet"
    local = error "Stub implementation for Lite Wallet"
