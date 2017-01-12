{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Wallet.Launcher.Runner
       ( runRawRealWallet
       , runWalletRealMode
       , runWalletReal
       , runWallet
       ) where

import           Formatting                (build, sformat, (%))
import           Mockable                  (Production, bracket, fork, sleepForever)
import           Node                      (Listener, SendActions)
import           System.Wlog               (logInfo, usingLoggerName)
import           Universum                 hiding (bracket)

import           Pos.Communication         (BiP (..))
import           Pos.DHT.Model             (DHTNodeType (..), discoverPeers)
import           Pos.DHT.Real              (runKademliaDHT)
import           Pos.Launcher              (BaseParams (..), LoggingParams (..),
                                            RealModeResources (..), addDevListeners,
                                            runServer)

import           Pos.Wallet.Context        (ctxFromParams, runContextHolder)
import           Pos.Wallet.KeyStorage     (runKeyStorage)
import           Pos.Wallet.Launcher.Param (WalletParams (..))
import           Pos.Wallet.State          (closeState, openMemState, openState,
                                            runWalletDB)
import           Pos.Wallet.WalletMode     (WalletMode, WalletRealMode)

-- TODO: Move to some `Pos.Wallet.Communication` and provide
-- meaningful listeners
allListeners
    -- :: (MonadDHTDialog SState m, WalletMode SscGodTossing m)
    -- => [ListenerDHT SState m]
    :: [a]
allListeners = []

-- TODO: Move to some `Pos.Wallet.Worker` and provide
-- meaningful ones
-- allWorkers :: WalletMode ssc m => [m ()]
allWorkers :: [a]
allWorkers = []

-- | WalletMode runner
runWalletRealMode
    :: RealModeResources
    -> WalletParams
    -> (SendActions BiP WalletRealMode -> WalletRealMode a)
    -> Production a
runWalletRealMode res wp@WalletParams {..} = runRawRealWallet res wp listeners
  where
    listeners = addDevListeners wpSystemStart allListeners

runWalletReal
    :: RealModeResources
    -> WalletParams
    -> [SendActions BiP WalletRealMode -> WalletRealMode ()]
    -> Production ()
runWalletReal res wp = runWalletRealMode res wp . runWallet

runWallet :: WalletMode ssc m => [SendActions BiP m -> m ()] -> SendActions BiP m -> m ()
runWallet plugins sendActions = do
    logInfo "Wallet is initialized!"
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: "%build) peers
    mapM_ fork allWorkers
    mapM_ (fork . ($ sendActions)) plugins
    sleepForever

runRawRealWallet
    :: RealModeResources
    -> WalletParams
    -> [Listener BiP WalletRealMode]
    -> (SendActions BiP WalletRealMode -> WalletRealMode a)
    -> Production a
runRawRealWallet res wp@WalletParams {..} listeners action =
    usingLoggerName lpRunnerTag .
    bracket openDB closeDB $ \db -> do
        runContextHolder (ctxFromParams wp) .
            runWalletDB db .
            runKeyStorage wpKeyFilePath .
            runKademliaDHT (rmDHT res) .
            runServer (rmTransport res) listeners $
                \sa -> logInfo "Started wallet, joining network" >> action sa
  where
    LoggingParams {..} = bpLoggingParams wpBaseParams
    openDB = maybe openMemState (openState wpRebuildDb) wpDbPath
    closeDB = closeState
