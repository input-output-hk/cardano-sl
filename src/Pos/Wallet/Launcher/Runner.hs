{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Wallet.Launcher.Runner
       ( runRawRealWallet
       , runWalletRealMode
       , runWalletReal
       , runWallet
       ) where

import           Control.Concurrent.STM.TVar      (newTVarIO)
import           Formatting                       (build, sformat, (%))
import           Mockable                         (Production, bracket, currentTime, fork,
                                                   sleepForever)
import           Node                             (Listener, SendActions)
import           System.Wlog                      (logInfo, usingLoggerName)
import           Universum                        hiding (bracket)

import           Pos.Communication                (BiP (..))
import           Pos.Communication.Types.Protocol (VerInfo)
import           Pos.DHT.Model                    (discoverPeers)
import           Pos.DHT.Real                     (runKademliaDHT)
import           Pos.Launcher                     (BaseParams (..), LoggingParams (..),
                                                   RealModeResources (..),
                                                   addDevListeners, runServer)
import           Pos.Slotting                     (SlottingState (..))

import           Pos.Types                        (unflattenSlotId)
import           Pos.Wallet.Context               (WalletContext (..), runContextHolder)
import           Pos.Wallet.KeyStorage            (runKeyStorage)
import           Pos.Wallet.Launcher.Param        (WalletParams (..))
import           Pos.Wallet.State                 (closeState, getSlotDuration,
                                                   openMemState, openState, runWalletDB)
import           Pos.Wallet.WalletMode            (WalletMode, WalletRealMode)

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
    -> ( SendActions BiP VerInfo WalletRealMode -> WalletRealMode a)
    -> Production a
runWalletRealMode res wp@WalletParams {..} = runRawRealWallet res wp listeners
  where
    listeners = addDevListeners wpSystemStart allListeners

runWalletReal
    :: RealModeResources
    -> WalletParams
    -> [ SendActions BiP VerInfo WalletRealMode -> WalletRealMode ()]
    -> Production ()
runWalletReal res wp = runWalletRealMode res wp . runWallet

runWallet :: WalletMode ssc m => [ SendActions BiP VerInfo m -> m ()] ->  SendActions BiP VerInfo m -> m ()
runWallet plugins sendActions = do
    logInfo "Wallet is initialized!"
    peers <- discoverPeers
    logInfo $ sformat ("Known peers: "%build) peers
    mapM_ fork allWorkers
    mapM_ (fork . ($ sendActions)) plugins
    sleepForever

runRawRealWallet
    :: RealModeResources
    -> WalletParams
    -> [Listener BiP VerInfo WalletRealMode]
    -> ( SendActions BiP VerInfo WalletRealMode -> WalletRealMode a)
    -> Production a
runRawRealWallet res WalletParams {..} listeners action =
    usingLoggerName lpRunnerTag .
    bracket openDB closeDB $ \db -> do
        slottingStateVar <- do
            ssSlotDuration <- runWalletDB db getSlotDuration
            ssNtpData <- (0,) <$> currentTime
            let ssNtpLastSlot = unflattenSlotId 0
            liftIO $ newTVarIO SlottingState{..}
        let walletContext
              = WalletContext
              { wcSystemStart = wpSystemStart
              , wcSlottingState = slottingStateVar
              }
        runContextHolder walletContext .
            runWalletDB db .
            runKeyStorage wpKeyFilePath .
            runKademliaDHT (rmDHT res) .
            runServer (rmTransport res) listeners $
                \sa -> logInfo "Started wallet, joining network" >> action sa
  where
    LoggingParams {..} = bpLoggingParams wpBaseParams
    openDB = maybe openMemState (openState wpRebuildDb) wpDbPath
    closeDB = closeState
