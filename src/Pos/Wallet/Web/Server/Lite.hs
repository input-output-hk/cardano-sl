{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for lite-wallet implementation of Daedalus API

module Pos.Wallet.Web.Server.Lite
       ( walletServeWebLite
       ) where

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           Universum


import           Pos.Wallet.Context            (ContextHolder, WalletContext,
                                                getWalletContext, runContextHolder)
import           Pos.Wallet.KeyStorage         (KeyData, KeyStorage, runKeyStorageRaw)
import           Pos.Wallet.State              (WalletDB, getWalletState, runWalletDB)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.WalletMode         (SState, WalletRealMode)
import           Pos.Wallet.Web.Server.Methods (walletApplication, walletServeImpl,
                                                walletServer)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletState,
                                                WalletWebDB, getWalletWebState,
                                                runWalletWebDB)

-- type WebHandler = WalletWebDB WalletRealMode
-- type SubKademlia = KeyStorage
--                    (WalletDB
--                     (ContextHolder
--                      (Dialog DHTPacking (Transfer SState))))
--
-- type MainWalletState = WS.WalletState

walletServeWebLite :: FilePath -> Bool -> Word16 -> WalletRealMode ()
walletServeWebLite = notImplemented -- walletServeImpl $ walletApplication $ walletServer nat

--convertHandler
--    :: KademliaDHTContext SubKademlia
--    -> WalletContext
--    -> MainWalletState
--    -> KeyData
--    -> WalletState
--    -> WebHandler a
--    -> Handler a
--convertHandler kctx wc mws kd ws handler =
--    liftIO (runOurDialog newMutSocketState "wallet-api" .
--            runContextHolder wc .
--            runWalletDB mws .
--            flip runKeyStorageRaw kd .
--            runKademliaDHTRaw kctx .
--            runWalletWebDB ws $
--            handler)
--    `Catch.catches`
--    excHandlers
--  where
--    excHandlers = [Catch.Handler catchServant]
--    catchServant = throwError
--
--nat :: WebHandler (WebHandler :~> Handler)
--nat = do
--    ws <- getWalletWebState
--    kd <- (lift . lift) ask
--    kctx <- lift getKademliaDHTCtx
--    wc <- getWalletContext
--    mws <- getWalletState
--    return $ Nat (convertHandler kctx wc mws kd ws)
