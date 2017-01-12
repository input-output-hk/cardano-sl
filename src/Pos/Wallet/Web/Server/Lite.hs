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
import           Pos.DHT.Real.Real             (getKademliaDHTInstance, runKademliaDHT)
import           Pos.DHT.Real.Types            (KademliaDHTInstance (..))
import           Pos.Wallet.Context            (WalletContext, getWalletContext,
                                                runContextHolder)
import           Pos.Wallet.KeyStorage         (KeyData, runKeyStorageRaw)
import           Pos.Wallet.State              (getWalletState, runWalletDB)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.WalletMode         (WalletRealMode)
import           Pos.Wallet.Web.Server.Methods (walletApplication, walletServeImpl,
                                                walletServer)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletState,
                                                WalletWebDB, getWalletWebState,
                                                runWalletWebDB)
import           System.Wlog                   (usingLoggerName)

walletServeWebLite
    :: SendActions BiP WalletRealMode
    -> FilePath
    -> Bool
    -> Word16
    -> WalletRealMode ()
walletServeWebLite sendActions =
    walletServeImpl $ walletApplication $ walletServer sendActions nat

-- type WebHandler ssc = WalletWebDB (RawRealMode ssc)
type WebHandler = WalletWebDB WalletRealMode
type MainWalletState = WS.WalletState

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    ws    <- getWalletWebState
    kd    <- (lift . lift) ask
    kinst <- lift $ getKademliaDHTInstance
    wc    <- getWalletContext
    mws   <- getWalletState
    return $ Nat (convertHandler kinst wc mws kd ws)

convertHandler
    :: forall a .
       KademliaDHTInstance
    -> WalletContext
    -> MainWalletState
    -> KeyData
    -> WalletState
    -> WebHandler a
    -> Handler a
convertHandler kinst wc mws kd ws handler =
    liftIO ( runProduction
           . usingLoggerName "wallet-lite-api"
           . runContextHolder wc
           . runWalletDB mws
           . flip runKeyStorageRaw kd
           . runKademliaDHT kinst
           . runWalletWebDB ws
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
