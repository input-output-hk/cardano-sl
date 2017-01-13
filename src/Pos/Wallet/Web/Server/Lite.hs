{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for lite-wallet implementation of Daedalus API

module Pos.Wallet.Web.Server.Lite
       ( walletServeWebLite
       ) where

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Node                          (SendActions)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           Universum

import           Mockable                      (runProduction)
import           Pos.Communication.BiP         (BiP)
import           Pos.DHT.Real.Real             (runKademliaDHT)
import           Pos.DHT.Real.Types            (KademliaDHTInstance (..), getKademliaDHTInstance)
import           Pos.Wallet.Context            (WalletContext, getWalletContext,
                                                runContextHolder)
import           Pos.Wallet.KeyStorage         (KeyData, runKeyStorageRaw)
import           Pos.Wallet.State              (getWalletState, runWalletDB)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.WalletMode         (WalletRealMode)
import           Pos.Wallet.Web.Server.Methods (walletApplication, walletServeImpl,
                                                walletServer)
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
    :: SendActions BiP WalletRealMode
    -> FilePath
    -> Bool
    -> Word16
    -> WalletRealMode ()
walletServeWebLite sendActions =
    walletServeImpl $ walletApplication $ walletServer sendActions nat

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    wsConn <- getWalletWebSockets
    ws    <- getWalletWebState
    kd    <- lift . lift . lift $ ask
    kinst <- lift . lift $ getKademliaDHTInstance
    wc    <- getWalletContext
    mws   <- getWalletState
    return $ Nat (convertHandler kinst wc mws kd ws wsConn)

convertHandler
    :: forall a .
       KademliaDHTInstance
    -> WalletContext
    -> MainWalletState
    -> KeyData
    -> WalletState
    -> ConnectionsVar
    -> WebHandler a
    -> Handler a
convertHandler kinst wc mws kd ws wsConn handler =
    liftIO ( runProduction
           . usingLoggerName "wallet-lite-api"
           . runContextHolder wc
           . runWalletDB mws
           . flip runKeyStorageRaw kd
           . runKademliaDHT kinst
           . runWalletWebDB ws
           . runWalletWS wsConn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
