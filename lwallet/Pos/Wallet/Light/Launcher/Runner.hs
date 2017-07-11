module Pos.Wallet.Light.Launcher.Runner
       ( runWalletStaticPeers
       , runWallet
       ) where

import           Universum                       hiding (bracket)

import           Control.Monad.Fix               (MonadFix)
import qualified Control.Monad.Reader            as Mtl
import           Formatting                      (sformat, shown, (%), int)
import           Mockable                        (MonadMockable, Production, bracket,
                                                  mapConcurrently)
import           Network.Transport.Abstract      (Transport)
import           Node                            (noReceiveDelay, simpleNodeEndPoint)
import qualified STMContainers.Map               as SM
import           System.Wlog                     (WithLogger, logDebug, logInfo)

import           Pos.Communication               (ActionSpec (..), MkListeners, NodeId,
                                                  OutSpecs, WorkerSpec)
import           Pos.Discovery                   (findPeers)
import           Pos.Launcher                    (BaseParams (..), LoggingParams (..),
                                                  runServer)
import           Pos.Reporting.MemState          (emptyReportingContext)
import           Pos.Util.JsonLog                (JsonLogConfig (..))
import           Pos.Util.Util                   ()
import           Pos.Wallet.KeyStorage           (keyDataFromFile)
import           Pos.Wallet.Light.Launcher.Param (WalletParams (..))
import           Pos.Wallet.Light.Mode           (LightWalletContext (..),
                                                  LightWalletMode)
import           Pos.Wallet.Light.State          (closeState, openMemState, openState)
import           Pos.Wallet.WalletMode           (MonadWallet)

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
    :: MonadWallet ssc ctx m
    => ([WorkerSpec m], OutSpecs)
    -> (WorkerSpec m, OutSpecs)
runWallet (plugins', pouts) = (,outs) . ActionSpec $ \vI sendActions -> do
    logInfo "Wallet is initialized!"
    peers <- findPeers
    logInfo $ sformat ("Known peers: "%shown) (toList peers)
    logDebug $ sformat ("Forking "%int%" plugins ...") (length peers)
    let unpackPlugin (ActionSpec action) = action vI sendActions
    void (mapConcurrently unpackPlugin (plugins' ++ workers'))
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
    bracket openDB closeDB $ \db -> do
        stateM <- liftIO SM.newIO
        keyData <- keyDataFromFile wpKeyFilePath
        flip Mtl.runReaderT
            ( LightWalletContext
                stateM
                keyData
                db
                emptyReportingContext
                peers
                JsonLogDisabled
                lpRunnerTag
            ) .
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

runServer_
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => Transport m -> MkListeners m -> OutSpecs -> ActionSpec m b -> m b
runServer_ transport mkl outSpecs =
    runServer (simpleNodeEndPoint transport) (const noReceiveDelay) mkl
        outSpecs acquire release
  where
    acquire = const pass
    release = const pass
