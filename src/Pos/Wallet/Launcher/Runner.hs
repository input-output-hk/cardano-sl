{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Wallet.Launcher.Runner
       ( runRawRealWallet
       , runWalletRealMode
       , runWalletReal
       , runWallet
       ) where

import           Control.Monad.Trans.Resource (allocate, runResourceT)
import           Control.TimeWarp.Timed       (fork, sleepForever)
import           Formatting                   (build, sformat, (%))
import           System.Wlog                  (logInfo)
import           Universum

import           Pos.Communication            (newMutSocketState)
import           Pos.DHT.Model                (DHTNodeType (..), ListenerDHT,
                                               MonadDHTDialog, discoverPeers)
import           Pos.DHT.Real                 (KademliaDHTInstance)
import           Pos.Launcher                 (BaseParams (..), LoggingParams (..),
                                               addDevListeners, runKDHT, runOurDialog,
                                               setupLoggers)
import           Pos.Ssc.GodTossing           (SscGodTossing)

import           Pos.Wallet.Context           (ctxFromParams, runContextHolder)
import           Pos.Wallet.KeyStorage        (runKeyStorage)
import           Pos.Wallet.Launcher.Param    (WalletParams (..))
import           Pos.Wallet.State             (closeState, openMemState, openState,
                                               runWalletDB)
import           Pos.Wallet.WalletMode        (SState, WalletMode, WalletRealMode)

-- TODO: Move to some `Pos.Wallet.Communication` and provide
-- meaningful listeners
allListeners
    :: (MonadDHTDialog SState m, WalletMode SscGodTossing m)
    => [ListenerDHT SState m]
allListeners = []

-- TODO: Move to some `Pos.Wallet.Worker` and provide
-- meaningful ones
allWorkers :: WalletMode ssc m => [m ()]
allWorkers = []

-- | WalletMode runner.
runWalletRealMode
    :: KademliaDHTInstance
    -> WalletParams
    -> WalletRealMode a
    -> IO a
runWalletRealMode inst wp@WalletParams {..} = runRawRealWallet inst wp listeners
  where
    listeners = addDevListeners wpSystemStart allListeners

runWalletReal
    :: KademliaDHTInstance
    -> WalletParams
    -> [WalletRealMode ()]
    -> IO ()
runWalletReal inst wp  = runWalletRealMode inst wp . runWallet

runWallet :: WalletMode ssc m => [m ()] -> m ()
runWallet plugins = do
    logInfo "Wallet is initialized!"
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: "%build) peers
    mapM_ fork allWorkers
    mapM_ fork plugins
    sleepForever

runRawRealWallet
    :: KademliaDHTInstance
    -> WalletParams
    -> [ListenerDHT SState WalletRealMode]
    -> WalletRealMode a
    -> IO a
runRawRealWallet inst wp@WalletParams {..} listeners action = runResourceT $ do
    setupLoggers lp
    db <- snd <$> allocate openDB closeDB
    lift $
        runOurDialog newMutSocketState lpRunnerTag .
        runContextHolder (ctxFromParams wp) .
        runWalletDB db .
        runKeyStorage wpKeyFilePath .
        runKDHT inst wpBaseParams listeners $
        logInfo "Started wallet, joining network" >> action
  where
    lp@LoggingParams {..} = bpLoggingParams wpBaseParams
    openDB = maybe openMemState (openState wpRebuildDb) wpDbPath
    closeDB = closeState
