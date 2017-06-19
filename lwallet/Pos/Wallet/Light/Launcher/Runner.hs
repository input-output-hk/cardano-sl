module Pos.Wallet.Light.Launcher.Runner
       ( runWalletStaticPeers
       , runWallet
       ) where

import           Universum                       hiding (bracket)

import           Control.Monad.Fix               (MonadFix)
import           Data.Tagged                     (Tagged (..))
import qualified Ether
import           Formatting                      (sformat, shown, (%))
import           Mockable                        (MonadMockable, Production, bracket,
                                                  fork, sleepForever)
import           Network.Transport.Abstract      (Transport)
import qualified STMContainers.Map               as SM
import           System.Wlog                     (WithLogger, logDebug, logInfo,
                                                  usingLoggerName)

import           Pos.Block.BListener             (runBListenerStub)
import           Pos.Communication               (ActionSpec (..), MkListeners, NodeId,
                                                  OutSpecs, WorkerSpec)
import           Pos.Communication.PeerState     (PeerStateTag, runPeerStateRedirect)
import           Pos.DB                          (runDBPureRedirect)
import           Pos.DB.Block                    (runBlockDBRedirect)
import           Pos.Discovery                   (findPeers, runDiscoveryConstT)
import           Pos.Launcher                    (BaseParams (..), LoggingParams (..),
                                                  runServer)
import           Pos.Reporting.MemState          (ReportingContext, emptyReportingContext)
import           Pos.Util.TimeWarp               (runWithoutJsonLogT)
import           Pos.Util.Util                   ()
import           Pos.Wallet.KeyStorage           (KeyData, keyDataFromFile)
import           Pos.Wallet.Light.Launcher.Param (WalletParams (..))
import           Pos.Wallet.Light.Mode           (LightWalletMode (..))
import           Pos.Wallet.Light.Redirect       (runBalancesWalletRedirect,
                                                  runBlockchainInfoNotImplemented,
                                                  runTxHistoryWalletRedirect,
                                                  runUpdatesNotImplemented)
import           Pos.Wallet.Light.State          (closeState, openMemState, openState)
import           Pos.Wallet.Light.State.Acidic   (WalletState)
import           Pos.Wallet.Light.State.Core     (runGStateCoreWalletRedirect)
import           Pos.Wallet.WalletMode           (WalletMode)

-- TODO: Move to some `Pos.Wallet.Worker` and provide
-- meaningful ones
-- allWorkers :: WalletMode ssc m => [m ()]
allWorkers :: Monoid b => ([a], b)
allWorkers = mempty

-- | WalletMode runner
runLightWalletMode
    :: Transport LightWalletMode
    -> Set NodeId
    -> WalletParams
    -> (ActionSpec LightWalletMode a, OutSpecs)
    -> Production a
runLightWalletMode transport peers wp@WalletParams {..} =
    runRawStaticPeersWallet transport peers wp mempty

runWalletStaticPeers
    :: Transport LightWalletMode
    -> Set NodeId
    -> WalletParams
    -> ([WorkerSpec LightWalletMode], OutSpecs)
    -> Production ()
runWalletStaticPeers transport peers wp =
    runLightWalletMode transport peers wp . runWallet

runWallet
    :: WalletMode m
    => ([WorkerSpec m], OutSpecs)
    -> (WorkerSpec m, OutSpecs)
runWallet (plugins', pouts) = (,outs) . ActionSpec $ \vI sendActions -> do
    logInfo "Wallet is initialized!"
    peers <- findPeers
    logInfo $ sformat ("Known peers: "%shown) (toList peers)
    let unpackPlugin (ActionSpec action) = action vI sendActions
    mapM_ (fork . unpackPlugin) $ plugins' ++ workers'
    logDebug "Forked all plugins successfully"
    sleepForever
  where
    (workers', wouts) = allWorkers
    outs = wouts <> pouts

runRawStaticPeersWallet
    :: Transport LightWalletMode
    -> Set NodeId
    -> WalletParams
    -> MkListeners LightWalletMode
    -> (ActionSpec LightWalletMode a, OutSpecs)
    -> Production a
runRawStaticPeersWallet transport peers WalletParams {..}
                        listeners (ActionSpec action, outs) =
    usingLoggerName lpRunnerTag .
    runWithoutJsonLogT .
    bracket openDB closeDB $ \db -> do
        stateM <- liftIO SM.newIO
        keyData <- keyDataFromFile wpKeyFilePath
        flip Ether.runReadersT
            ( Tagged @PeerStateTag stateM
            , Tagged @KeyData keyData
            , Tagged @WalletState db
            , Tagged @ReportingContext emptyReportingContext ) .
            runDBPureRedirect .
            runBlockDBRedirect .
            runTxHistoryWalletRedirect .
            runBalancesWalletRedirect .
            runGStateCoreWalletRedirect .
            runPeerStateRedirect .
            runUpdatesNotImplemented .
            runBlockchainInfoNotImplemented .
            runBListenerStub .
            runDiscoveryConstT peers .
            (\(LightWalletMode m) -> m) .
            runServer_ transport listeners outs . ActionSpec $ \vI sa ->
            logInfo "Started wallet, joining network" >> action vI sa
  where
    LoggingParams {..} = bpLoggingParams wpBaseParams
    openDB =
        maybe
            (openMemState wpGenesisUtxo)
            (openState wpRebuildDb wpGenesisUtxo)
            wpDbPath
    closeDB = closeState
{-# NOINLINE runRawStaticPeersWallet #-}

runServer_
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => Transport m -> MkListeners m -> OutSpecs -> ActionSpec m b -> m b
runServer_ transport mkl outSpecs =
    runServer transport mkl outSpecs acquire release
  where
    acquire = const pass
    release = const pass
