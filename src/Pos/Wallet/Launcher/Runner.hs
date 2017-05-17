module Pos.Wallet.Launcher.Runner
       ( runRawStaticPeersWallet
       , runWalletStaticPeersMode
       , runWalletStaticPeers
       , runWallet
       ) where

import           Data.Tagged                 (untag)
import           Formatting                  (sformat, shown, (%))
import           Mockable                    (Production, bracket, fork, sleepForever)
import           Network.Transport.Abstract  (Transport)
import qualified STMContainers.Map           as SM
import           System.Wlog                 (logDebug, logInfo, usingLoggerName)
import           Universum                   hiding (bracket)

import           Pos.Communication           (ActionSpec (..), ListenersWithOut, NodeId,
                                              OutSpecs, PeerId, WorkerSpec,
                                              allStubListeners)
import           Pos.Communication.PeerState (runPeerStateHolder)
import           Pos.Discovery               (findPeers, runDiscoveryConstT)
import           Pos.Launcher                (BaseParams (..), LoggingParams (..),
                                              runServer_)
import           Pos.Reporting.MemState      (runWithoutReportingContext)
import           Pos.Ssc.GodTossing          (SscGodTossing)
import           Pos.Util.JsonLog            (usingJsonLogFilePath)
import           Pos.Util.Util               ()
import           Pos.Wallet.KeyStorage       (runKeyStorage)
import           Pos.Wallet.Launcher.Param   (WalletParams (..))
import           Pos.Wallet.State            (closeState, openMemState, openState,
                                              runWalletDB)
import           Pos.Wallet.WalletMode       (WalletMode, WalletStaticPeersMode)

-- TODO: Move to some `Pos.Wallet.Communication` and provide
-- meaningful listeners
allListeners
    :: ListenersWithOut WalletStaticPeersMode
allListeners = untag @SscGodTossing allStubListeners

-- TODO: Move to some `Pos.Wallet.Worker` and provide
-- meaningful ones
-- allWorkers :: WalletMode ssc m => [m ()]
allWorkers :: Monoid b => ([a], b)
allWorkers = ([], mempty)

-- | WalletMode runner
runWalletStaticPeersMode
    :: PeerId
    -> Transport WalletStaticPeersMode
    -> Set NodeId
    -> WalletParams
    -> (ActionSpec WalletStaticPeersMode a, OutSpecs)
    -> Production a
runWalletStaticPeersMode peerId transport peers wp@WalletParams {..} =
    runRawStaticPeersWallet peerId transport peers wp allListeners

runWalletStaticPeers
    :: PeerId
    -> Transport WalletStaticPeersMode
    -> Set NodeId
    -> WalletParams
    -> ([WorkerSpec WalletStaticPeersMode], OutSpecs)
    -> Production ()
runWalletStaticPeers peerId transport peers wp =
    runWalletStaticPeersMode peerId transport peers wp . runWallet

runWallet
    :: WalletMode ssc m
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
    :: PeerId
    -> Transport WalletStaticPeersMode
    -> Set NodeId
    -> WalletParams
    -> ListenersWithOut WalletStaticPeersMode
    -> (ActionSpec WalletStaticPeersMode a, OutSpecs)
    -> Production a
runRawStaticPeersWallet peerId transport peers WalletParams {..}
                        listeners (ActionSpec action, outs) =
    usingJsonLogFilePath (flip (,) alwaysLog <$> wpJLFilePath) . usingLoggerName lpRunnerTag . bracket openDB closeDB $ \db -> do
        stateM <- liftIO SM.newIO
        runWithoutReportingContext .
            runWalletDB db .
            runKeyStorage wpKeyFilePath .
            runPeerStateHolder stateM .
            runDiscoveryConstT peers .
            runServer_ peerId transport listeners outs . ActionSpec $ \vI sa ->
            logInfo "Started wallet, joining network" >> action vI sa
  where
    alwaysLog = const (pure True)
    LoggingParams {..} = bpLoggingParams wpBaseParams
    openDB =
        maybe
            (openMemState wpGenesisUtxo)
            (openState wpRebuildDb wpGenesisUtxo)
            wpDbPath
    closeDB = closeState
