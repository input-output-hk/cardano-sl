{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Wallet.Launcher.Runner
       ( runRawRealWallet
       , runWalletRealMode
       , runWalletReal
       , runWallet
       ) where

import           Formatting                (build, sformat, (%))
import           Mockable                  (bracket, fork, runProduction, sleepForever)
import           Node                      (Listener, SendActions)
import           System.Wlog               (logInfo, usingLoggerName)
import           Universum                 hiding (bracket)

import           Pos.Communication         (BiP (..))
import           Pos.Launcher              (BaseParams (..), LoggingParams (..),
                                            addDevListeners, loggerBracket, runServer,
                                            setupLoggers)
import           Pos.NewDHT.Model          (DHTNodeType (..), discoverPeers)
import           Pos.NewDHT.Real           (KademliaDHTInstance, runKademliaDHT)

import           Pos.Wallet.Context        (ctxFromParams, runContextHolder)
import           Pos.Wallet.KeyStorage     (runKeyStorage)
import           Pos.Wallet.Launcher.Param (WalletParams (..))
import           Pos.Wallet.State          (closeState, openMemState, openState,
                                            runWalletDB)
import           Pos.Wallet.WalletMode     (SState, WalletMode, WalletRealMode)

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

-- | WalletMode runner.
runWalletRealMode
    :: KademliaDHTInstance
    -> WalletParams
    -> (SendActions BiP WalletRealMode -> WalletRealMode a)
    -> IO a
runWalletRealMode inst wp@WalletParams {..} = runRawRealWallet inst wp listeners
  where
    listeners = addDevListeners wpSystemStart allListeners

runWalletReal
    :: KademliaDHTInstance
    -> WalletParams
    -> [WalletRealMode ()]
    -> IO ()
runWalletReal inst wp = runWalletRealMode inst wp . runWallet

runWallet :: WalletMode ssc m => [m ()] -> SendActions BiP m -> m ()
runWallet plugins __sendActions = do
    logInfo "Wallet is initialized!"
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: "%build) peers
    mapM_ fork allWorkers
    mapM_ fork plugins
    sleepForever

runRawRealWallet
    :: KademliaDHTInstance
    -> WalletParams
    -> [Listener BiP WalletRealMode]
    -> (SendActions BiP WalletRealMode -> WalletRealMode a)
    -> IO a
runRawRealWallet inst wp@WalletParams {..} listeners action =
    loggerBracket lp .
    runProduction .
    usingLoggerName lpRunnerTag .
    bracket openDB closeDB $ \db -> do
        runContextHolder (ctxFromParams wp) .
            runWalletDB db .
            runKeyStorage wpKeyFilePath .
            runKademliaDHT inst .
            runServer (bpPort wpBaseParams) listeners $
                \sa -> logInfo "Started wallet, joining network" >> action sa
  where
    lp@LoggingParams {..} = bpLoggingParams wpBaseParams
    openDB = maybe openMemState (openState wpRebuildDb) wpDbPath
    closeDB = closeState
