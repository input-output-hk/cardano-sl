
module Pos.Wallet.Launcher.Runner
       ( runRawRealWallet
       , runWalletRealMode
       , runWalletReal
       , runWallet
       ) where

import           Data.Tagged                 (untag)
import           Formatting                  (sformat, shown, (%))
import           Mockable                    (Production, bracket, fork, sleepForever)
import qualified STMContainers.Map           as SM
import           System.Wlog                 (logDebug, logInfo, usingLoggerName)
import           Universum                   hiding (bracket)

import           Pos.Communication           (ActionSpec (..), ListenersWithOut, OutSpecs,
                                              PeerId, WorkerSpec, allStubListeners)
import           Pos.Communication.PeerState (runPeerStateHolder)
import           Pos.Launcher                (BaseParams (..), LoggingParams (..),
                                              RealModeResources (..), runServer_)
import           Pos.Reporting.MemState      (runWithoutReportingContext)
import           Pos.Ssc.GodTossing          (SscGodTossing)
import           Pos.Util.Util               ()
import           Pos.Wallet.KeyStorage       (runKeyStorage)
import           Pos.Wallet.Launcher.Param   (WalletParams (..))
import           Pos.Wallet.State            (closeState, openMemState, openState,
                                              runWalletDB)
import           Pos.Wallet.WalletMode       (WalletMode, WalletRealMode)

-- TODO: Move to some `Pos.Wallet.Communication` and provide
-- meaningful listeners
allListeners
    :: ListenersWithOut WalletRealMode
allListeners = untag @SscGodTossing allStubListeners

-- TODO: Move to some `Pos.Wallet.Worker` and provide
-- meaningful ones
-- allWorkers :: WalletMode ssc m => [m ()]
allWorkers :: Monoid b => ([a], b)
allWorkers = ([], mempty)

-- | WalletMode runner
runWalletRealMode
    :: PeerId
    -> RealModeResources WalletRealMode
    -> WalletParams
    -> (ActionSpec WalletRealMode a, OutSpecs)
    -> Production a
runWalletRealMode peerId res wp@WalletParams {..} = runRawRealWallet peerId res wp allListeners
  -- where
  --   listeners = addDevListeners wpSystemStart allListeners

runWalletReal
    :: PeerId
    -> RealModeResources WalletRealMode
    -> WalletParams
    -> ([WorkerSpec WalletRealMode], OutSpecs)
    -> Production ()
runWalletReal peerId res wp = runWalletRealMode peerId res wp . runWallet res

runWallet
    :: WalletMode ssc m
    => RealModeResources m
    -> ([WorkerSpec m], OutSpecs)
    -> (WorkerSpec m, OutSpecs)
runWallet res (plugins', pouts) = (,outs) . ActionSpec $ \vI sendActions -> do
    logInfo "Wallet is initialized!"
    peers <- rmFindPeers res
    logInfo $ sformat ("Known peers: "%shown) (toList peers)
    let unpackPlugin (ActionSpec action) = action vI sendActions
    mapM_ (fork . unpackPlugin) $ plugins' ++ workers'
    logDebug "Forked all plugins successfully"
    sleepForever
  where
    (workers', wouts) = allWorkers
    outs = wouts <> pouts

runRawRealWallet
    :: PeerId
    -> RealModeResources WalletRealMode
    -> WalletParams
    -> ListenersWithOut WalletRealMode
    -> (ActionSpec WalletRealMode a, OutSpecs)
    -> Production a
runRawRealWallet peerId res WalletParams {..} listeners (ActionSpec action, outs) =
    usingLoggerName lpRunnerTag . bracket openDB closeDB $ \db -> do
        stateM <- liftIO SM.newIO
        runWithoutReportingContext .
            runWalletDB db .
            runKeyStorage wpKeyFilePath .
            runPeerStateHolder stateM .
            runServer_ peerId (rmTransport res) listeners outs . ActionSpec $ \vI sa ->
            logInfo "Started wallet, joining network" >> action vI sa
  where
    LoggingParams {..} = bpLoggingParams wpBaseParams
    openDB =
        maybe
            (openMemState wpGenesisUtxo)
            (openState wpRebuildDb wpGenesisUtxo)
            wpDbPath
    closeDB = closeState
