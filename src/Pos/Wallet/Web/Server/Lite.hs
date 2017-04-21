{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for lite-wallet implementation of Daedalus API

module Pos.Wallet.Web.Server.Lite
       ( walletServeWebLite
       , walletServerOuts
       ) where

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Mockable                      (runProduction)
import           Pos.Communication.Protocol    (SendActions)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import qualified STMContainers.Map             as SM
import           Universum

import           Pos.Communication.PeerState   (runPeerStateHolder)
import           Pos.DHT.Real.Real             (runKademliaDHT)
import           Pos.DHT.Real.Types            (KademliaDHTInstance (..),
                                                getKademliaDHTInstance)
import           Pos.Reporting.MemState        (runWithoutReportingContext)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Wallet.KeyStorage         (KeyData, runKeyStorageRaw)
import           Pos.Wallet.State              (getWalletState, runWalletDB)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.WalletMode         (WalletRealMode)
import           Pos.Wallet.Web.Server.Methods (walletApplication, walletServeImpl,
                                                walletServer, walletServerOuts)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar,
                                                MonadWalletWebSockets (..),
                                                WalletWebSockets, runWalletWS)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletState,
                                                WalletWebDB, getWalletWebState,
                                                runWalletWebDB)
import           System.Wlog                   (usingLoggerName)


type WebHandler = WalletWebSockets (WalletWebDB WalletRealMode)

type MainWalletState = WS.WalletState

walletServeWebLite
    :: forall ssc.
       SscHelpersClass ssc
    => Proxy ssc
    -> SendActions WalletRealMode
    -> FilePath
    -> Bool
    -> Word16
    -> WalletRealMode ()
walletServeWebLite _ sendActions =
    walletServeImpl $ walletApplication $ walletServer sendActions nat

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    wsConn <- getWalletWebSockets
    ws    <- getWalletWebState
    kd    <- lift . lift . lift . lift $ ask
    kinst <- lift . lift $ getKademliaDHTInstance
    mws   <- getWalletState
    return $ NT (convertHandler kinst mws kd ws wsConn)

convertHandler
    :: forall a .
       KademliaDHTInstance
    -> MainWalletState
    -> KeyData
    -> WalletState
    -> ConnectionsVar
    -> WebHandler a
    -> Handler a
convertHandler kinst mws kd ws wsConn handler = do
    stateM <- liftIO SM.newIO
    liftIO ( runProduction
           . usingLoggerName "wallet-lite-api"
           . runWithoutReportingContext
           . runWalletDB mws
           . flip runKeyStorageRaw kd
           . runKademliaDHT kinst
           . runPeerStateHolder stateM
           . runWalletWebDB ws
           . runWalletWS wsConn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
