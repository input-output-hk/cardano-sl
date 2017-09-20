module Pos.Wallet.Light.Launcher.Runner
       ( runWalletStaticPeers
       , runWallet
       ) where

import           Universum                       hiding (bracket)

import           Control.Monad.Fix               (MonadFix)
import qualified Control.Monad.Reader            as Mtl
import qualified Data.Set                        as Set
import           Mockable                        (MonadMockable, Production, fork,
                                                  sleepForever)
import           Network.Transport.Abstract      (Transport)
import           Node                            (noReceiveDelay, simpleNodeEndPoint)
import           System.Wlog                     (WithLogger, logDebug, logInfo)

import           Pos.Communication               (ActionSpec (..), MkListeners, NodeId,
                                                  OutSpecs, WorkerSpec)
import           Pos.Launcher                    (BaseParams (..), LoggingParams (..), OQ,
                                                  initQueue, runServer,
                                                  HasConfigurations)
import           Pos.Network.Types               (NetworkConfig, Topology (..),
                                                  defaultNetworkConfig)
import           Pos.Reporting.MemState          (emptyReportingContext)
import           Pos.Util.JsonLog                (JsonLogConfig (..))
import           Pos.Util.Util                   ()
import           Pos.Wallet.KeyStorage           (keyDataFromFile)
import           Pos.Wallet.Light.Launcher.Param (WalletParams (..))
import           Pos.Wallet.Light.Mode           (LightWalletContext (..),
                                                  LightWalletMode)

-- TODO: Move to some `Pos.Wallet.Worker` and provide
-- meaningful ones
-- allWorkers :: WalletMode ssc m => [m ()]
allWorkers :: Monoid b => ([a], b)
allWorkers = mempty

-- | WalletMode runner
runLightWalletMode
    :: HasConfigurations
    => NetworkConfig kademlia
    -> Transport LightWalletMode
    -> Set NodeId
    -> WalletParams
    -> (ActionSpec LightWalletMode a, OutSpecs)
    -> Production a
runLightWalletMode networkConfig transport peers wp@WalletParams {..} =
    runRawStaticPeersWallet networkConfig transport peers wp mempty

runWalletStaticPeers
    :: HasConfigurations
    => Transport LightWalletMode
    -> Set NodeId
    -> WalletParams
    -> ([WorkerSpec LightWalletMode], OutSpecs)
    -> Production ()
runWalletStaticPeers transport peers wp =
    runLightWalletMode networkConfig transport peers wp . runWallet
  where
    networkConfig :: NetworkConfig kademlia
    networkConfig = defaultNetworkConfig $ TopologyLightWallet (Set.toList peers)

runWallet
    :: (MonadMockable m, WithLogger m)
    => ([WorkerSpec m], OutSpecs)
    -> (WorkerSpec m, OutSpecs)
runWallet (plugins', pouts) = (,outs) . ActionSpec $ \vI sendActions -> do
    logInfo "Wallet is initialized!"
    let unpackPlugin (ActionSpec action) = action vI sendActions
    mapM_ (fork . unpackPlugin) $ plugins' ++ workers'
    logDebug "Forked all plugins successfully"
    sleepForever
  where
    (workers', wouts) = allWorkers
    outs = wouts <> pouts

runRawStaticPeersWallet
    :: HasConfigurations
    => NetworkConfig kademlia
    -> Transport LightWalletMode
    -> Set NodeId
    -> WalletParams
    -> MkListeners LightWalletMode
    -> (ActionSpec LightWalletMode a, OutSpecs)
    -> Production a
runRawStaticPeersWallet networkConfig transport peers WalletParams {..}
                        listeners (ActionSpec action, outs) = do
        keyData <- keyDataFromFile wpKeyFilePath
        oq <- initQueue networkConfig Nothing
        flip Mtl.runReaderT
            ( LightWalletContext
                keyData
                emptyReportingContext
                peers
                JsonLogDisabled
                lpRunnerTag
            ) .
            runServer_ transport listeners outs oq . ActionSpec $ \vI sa ->
            logInfo "Started wallet, joining network" >> action vI sa
  where
    LoggingParams {..} = bpLoggingParams wpBaseParams

runServer_
    :: (HasConfigurations, MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => Transport m -> MkListeners m -> OutSpecs -> OQ m -> ActionSpec m b -> m b
runServer_ transport mkl outSpecs oq =
    runServer (simpleNodeEndPoint transport) (const noReceiveDelay) (const mkl)
        outSpecs acquire release oq
  where
    acquire = const pass
    release = const pass
