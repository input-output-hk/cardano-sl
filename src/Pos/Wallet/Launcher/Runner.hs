module Pos.Wallet.Launcher.Runner
       ( runRawStaticPeersWallet
       , runWalletStaticPeersMode
       , runWalletStaticPeers
       , runWallet
       ) where

import           Universum                   hiding (bracket)

import           Data.Tagged                 (Tagged (..))
import qualified Ether
import           Formatting                  (sformat, shown, (%))
import           Mockable                    (Production, bracket, fork, sleepForever)
import           Network.Transport.Abstract  (Transport)
import qualified STMContainers.Map           as SM
import           System.Wlog                 (logDebug, logInfo, usingLoggerName)

import           Pos.Block.BListener         (runBListenerStub)
import           Pos.Communication           (ActionSpec (..), MkListeners, NodeId,
                                              OutSpecs, WorkerSpec)
import           Pos.Communication.PeerState (PeerStateTag, runPeerStateRedirect)
import           Pos.DB                      (runDBPureRedirect)
import           Pos.DB.Block                (runBlockDBRedirect)
import           Pos.Discovery               (findPeers, runDiscoveryConstT)
import           Pos.Launcher                (BaseParams (..), LoggingParams (..),
                                              runServer_)
import           Pos.Reporting.MemState      (ReportingContext, emptyReportingContext)
import           Pos.Util.Util               ()
import           Pos.Wallet.KeyStorage       (KeyData, keyDataFromFile)
import           Pos.Wallet.Launcher.Param   (WalletParams (..))
import           Pos.Wallet.State            (closeState, openMemState, openState)
import           Pos.Wallet.State.Acidic     (WalletState)
import           Pos.Wallet.State.Core       (runGStateCoreWalletRedirect)
import           Pos.Wallet.WalletMode       (WalletMode, WalletStaticPeersMode,
                                              runBalancesWalletRedirect,
                                              runBlockchainInfoNotImplemented,
                                              runTxHistoryWalletRedirect,
                                              runUpdatesNotImplemented)

-- TODO: Move to some `Pos.Wallet.Worker` and provide
-- meaningful ones
-- allWorkers :: WalletMode ssc m => [m ()]
allWorkers :: Monoid b => ([a], b)
allWorkers = mempty

-- | WalletMode runner
runWalletStaticPeersMode
    :: Transport WalletStaticPeersMode
    -> Set NodeId
    -> WalletParams
    -> (ActionSpec WalletStaticPeersMode a, OutSpecs)
    -> Production a
runWalletStaticPeersMode transport peers wp@WalletParams {..} =
    runRawStaticPeersWallet transport peers wp mempty

runWalletStaticPeers
    :: Transport WalletStaticPeersMode
    -> Set NodeId
    -> WalletParams
    -> ([WorkerSpec WalletStaticPeersMode], OutSpecs)
    -> Production ()
runWalletStaticPeers transport peers wp =
    runWalletStaticPeersMode transport peers wp . runWallet

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
    :: Transport WalletStaticPeersMode
    -> Set NodeId
    -> WalletParams
    -> MkListeners WalletStaticPeersMode
    -> (ActionSpec WalletStaticPeersMode a, OutSpecs)
    -> Production a
runRawStaticPeersWallet transport peers WalletParams {..}
                        listeners (ActionSpec action, outs) =
    usingLoggerName lpRunnerTag . bracket openDB closeDB $ \db -> do
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
