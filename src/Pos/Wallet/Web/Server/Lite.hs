-- | Module for lite-wallet implementation of Daedalus API

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Wallet.Web.Server.Lite
       ( walletServeWebLite
       ) where

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Control.TimeWarp.Rpc          (Dialog, Transfer)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           Universum

import           Pos.Communication             (newMutSocketState)
import           Pos.DHT.Model                 (DHTPacking)
import           Pos.DHT.Real                  (KademliaDHTContext, getKademliaDHTCtx,
                                                runKademliaDHTRaw)
import           Pos.Launcher                  (runOurDialog)
import           Pos.Web.Server                (serveImpl)

import           Pos.Wallet.Context            (ContextHolder, WalletContext,
                                                getWalletContext, runContextHolder)
import           Pos.Wallet.KeyStorage         (KeyData, KeyStorage, runKeyStorageRaw)
import           Pos.Wallet.State              (WalletDB, getWalletState, runWalletDB)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.WalletMode         (SState, WalletRealMode)
import           Pos.Wallet.Web.Server.Methods (walletApplication, walletServer)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletState,
                                                WalletWebDB, getWalletWebState,
                                                runWalletWebDB)

type WebHandler = WalletWebDB WalletRealMode
type SubKademlia = KeyStorage
                   (WalletDB
                    (ContextHolder
                     (Dialog DHTPacking (Transfer SState))))

type MainWalletState = WS.WalletState

walletServeWebLite :: FilePath -> Word16 -> WalletRealMode ()
walletServeWebLite daedalusDbPath = serveImpl $
    walletApplication (walletServer nat) daedalusDbPath

convertHandler
    :: KademliaDHTContext SubKademlia
    -> WalletContext
    -> MainWalletState
    -> KeyData
    -> WalletState
    -> WebHandler a
    -> Handler a
convertHandler kctx wc mws kd ws handler =
    liftIO (runOurDialog newMutSocketState "wallet-api" .
            runContextHolder wc .
            runWalletDB mws .
            flip runKeyStorageRaw kd .
            runKademliaDHTRaw kctx .
            runWalletWebDB ws $
            handler)
    `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    ws <- getWalletWebState
    kd <- (lift . lift) ask
    kctx <- lift getKademliaDHTCtx
    wc <- getWalletContext
    mws <- getWalletState
    return $ Nat (convertHandler kctx wc mws kd ws)
