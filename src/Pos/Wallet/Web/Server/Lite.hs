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
import           Pos.Communication.Protocol    (SendActions, NodeId)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import qualified STMContainers.Map             as SM
import           Universum

import           Pos.Communication.PeerState   (runPeerStateHolder)
import           Pos.Reporting.MemState        (runWithoutReportingContext)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Wallet.KeyStorage         (KeyData, runKeyStorageRaw)
import           Pos.Wallet.State              (getWalletState, runWalletDB)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.WalletMode         (WalletRealMode)
import           Pos.Wallet.Web.Server.Methods (walletApplication, walletServeImpl,
                                                walletServer, walletServerOuts)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, WalletWebSockets,
                                                getWalletWebSockets, runWalletWS)
import           Pos.Wallet.Web.State          (WalletState, WalletWebDB,
                                                getWalletWebState, runWalletWebDB)
import           System.Wlog                   (usingLoggerName)


type WebHandler = WalletWebSockets (WalletWebDB WalletRealMode)

type MainWalletState = WS.WalletState

walletServeWebLite
    :: forall ssc.
       SscHelpersClass ssc
    => Proxy ssc
    -> WalletRealMode (Set NodeId)
    -> SendActions WalletRealMode
    -> FilePath
    -> Bool
    -> Word16
    -> WalletRealMode ()
walletServeWebLite _ getPeers sendActions =
    walletServeImpl $ walletApplication $ walletServer getPeers sendActions nat

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    wsConn <- getWalletWebSockets
    ws    <- getWalletWebState
    kd    <- Ether.ask
    mws   <- getWalletState
    return $ NT (convertHandler mws kd ws wsConn)

convertHandler
    :: forall a .
       MainWalletState
    -> KeyData
    -> WalletState
    -> ConnectionsVar
    -> WebHandler a
    -> Handler a
convertHandler mws kd ws wsConn handler = do
    stateM <- liftIO SM.newIO
    liftIO ( runProduction
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
