{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for lite-wallet implementation of Daedalus API

module Pos.Wallet.Web.Server.Lite
       ( walletServeWebLite
       , walletServerOuts
       ) where

import qualified Control.Monad.Catch           as Catch
import qualified Control.Monad.Ether.Implicit  as Ether
import           Control.Monad.Except          (MonadError (throwError))
import           Mockable                      (runProduction)
import           Mockable                      (Production)
import           Network.Wai                   (Application)
import           Pos.Communication.Protocol    (NodeId, SendActions, hoistSendActions)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import qualified STMContainers.Map             as SM
import           Universum

import           Pos.Communication.PeerState   (runPeerStateHolder)
import           Pos.Context                   (WithNodeContext)
import           Pos.DB                        (NodeDBs)
import           Pos.Reporting.MemState        (runWithoutReportingContext)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Wallet.KeyStorage         (KeyData, runKeyStorageRaw)
import           Pos.Wallet.State              (getWalletState, runWalletDB)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.WalletMode         (FakeSsc, WalletRealMode, runFakeSsc)
import           Pos.Wallet.Web.Server.Methods (WalletWebHandler, bracketWalletWS,
                                                bracketWalletWebDB, walletApplication,
                                                walletServeImpl, walletServer,
                                                walletServerOuts)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, WalletWebSockets,
                                                getWalletWebSockets, runWalletWS)
import           Pos.Wallet.Web.State          (WalletState, WalletWebDB,
                                                getWalletWebState, runWalletWebDB)
import           System.Wlog                   (usingLoggerName)


type WebHandler ssc = WalletWebSockets (WalletWebDB (WalletRealMode ssc))

type MainWalletState = WS.WalletState

walletServeWebLite
    :: forall ssc.
       SscHelpersClass ssc
    => Proxy ssc
    -> WalletRealMode ssc (Set NodeId)
    -> SendActions (WalletRealMode ssc)
    -> FilePath
    -> Bool
    -> Word16
    -> WalletRealMode ssc ()
walletServeWebLite _ getPeers sendActions dbPath dbRebuild port =
    bracketWalletWebDB dbPath dbRebuild $ \db ->
        bracketWalletWS $ \conn -> do
            let runner = runWalletWebDB db . runWalletWS conn
            let hoistedSA :: SendActions (WalletWebHandler (WalletRealMode ssc))
                hoistedSA = hoistSendActions (lift . lift) runner sendActions
            let action :: WalletWebHandler (WalletRealMode ssc) Application
                action = walletApplication $ walletServer getPeers hoistedSA nat
            runner $ walletServeImpl action port

nat :: WebHandler ssc (WebHandler ssc :~> Handler)
nat = do
    wsConn <- getWalletWebSockets
    ws    <- getWalletWebState
    kd    <- Ether.ask
    mws   <- getWalletState
    return $ NT (convertHandler mws kd ws wsConn)

convertHandler
    :: forall ssc a .
       MainWalletState
    -> KeyData
    -> WalletState
    -> ConnectionsVar
    -> WebHandler ssc a
    -> Handler a
convertHandler mws kd ws wsConn handler = do
    stateM <- liftIO SM.newIO
    liftIO ( runProduction
           . runFakeSsc
           . usingLoggerName "wallet-lite-api"
           . runWithoutReportingContext
           . runWalletDB mws
           . flip runKeyStorageRaw kd
           . runPeerStateHolder stateM
           . runWalletWebDB ws
           . runWalletWS wsConn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

-- Stub implementations for lite wallet.
instance Ether.MonadReader NodeDBs Production where
    --ask = undefined
    --local = undefined

instance WithNodeContext ssc (FakeSsc ssc Production) where
