{- | A collection of plugins used by this edge node. -}
{-# LANGUAGE TupleSections #-}
module Cardano.Wallet.Server.Plugins (
      Plugin
    , acidCleanupWorker
    , conversation
    , walletBackend
    ) where

import           Universum

import           Cardano.Wallet.API             as API
import           Cardano.Wallet.Server          as API
import           Cardano.Wallet.Server.CLI      (WalletBackendParams (..), isDebugMode,
                                                 walletAcidInterval, walletDbOptions)

import qualified Control.Concurrent.STM         as STM
import           Network.Wai                    (Application)
import           Pos.Wallet.Web                 (cleanupAcidStatePeriodically,
                                                 syncWalletsWithGState, wwmcSendActions)
import           Pos.Wallet.Web.Account         (findKey, myRootAddresses)
import           Pos.Wallet.Web.Methods.Restore (addInitialRichAccount)
import           Pos.Wallet.Web.Pending.Worker  (startPendingTxsResubmitter)
import qualified Pos.Wallet.Web.Server.Runner   as V0
import           Pos.Wallet.Web.Sockets         (getWalletWebSockets, launchNotifier,
                                                 upgradeApplicationWS)
import           Servant                        (Handler, Server, serve)
import           Servant.Utils.Enter            ((:~>) (..))
import           System.Wlog                    (logInfo, modifyLoggerName)

import qualified Data.ByteString.Char8          as BS8
import           Pos.Communication              (ActionSpec (..), OutSpecs, SendActions,
                                                 WorkerSpec, sendTxOuts, worker)
import           Pos.Context                    (HasNodeContext)

import           Pos.Launcher.Configuration     (HasConfigurations)
import           Pos.Util.CompileInfo           (HasCompileInfo)
import           Pos.Wallet.Web.Mode            (MonadFullWalletWebMode,
                                                 MonadWalletWebMode,
                                                 MonadWalletWebSockets, WalletWebMode,
                                                 WalletWebModeContext)
import           Pos.Wallet.Web.Server.Launcher (walletServeImpl, walletServerOuts)
import           Pos.Web                        (TlsParams, serveImpl, serveWeb)
import           Pos.WorkMode                   (WorkMode)

-- A @Plugin@ running in the monad @m@.
type Plugin m = ([WorkerSpec m], OutSpecs)

-- | A @Plugin@ to periodically compact & snapshot the acid-state database.
acidCleanupWorker :: HasConfigurations
                  => WalletBackendParams
                  -> Plugin WalletWebMode
acidCleanupWorker WalletBackendParams{..} =
    first one $ worker mempty $ const $
    modifyLoggerName (const "acidcleanup") $
    cleanupAcidStatePeriodically (walletAcidInterval walletDbOptions)

-- | The @Plugin@ which defines part of the conversation protocol for this node.
conversation :: (HasConfigurations, HasCompileInfo) => WalletBackendParams -> Plugin WalletWebMode
conversation wArgs = (, mempty) $ map (\act -> ActionSpec $ \__vI __sA -> act) (pluginsMonitoringApi wArgs)
  where
    pluginsMonitoringApi :: (WorkMode ctx m , HasNodeContext ctx , HasConfigurations, HasCompileInfo)
                         => WalletBackendParams
                         -> [m ()]
    pluginsMonitoringApi WalletBackendParams {..}
        | enableMonitoringApi = [serveWeb monitoringApiPort (Just walletTLSParams)]
        | otherwise = []

-- | A @Plugin@ to start the wallet backend API.
walletBackend :: (HasConfigurations, HasCompileInfo)
              => WalletBackendParams
              -> Plugin WalletWebMode
walletBackend WalletBackendParams {..} =
    first one $ worker walletServerOuts $ \sendActions -> do
      walletServeImpl
        (getApplication sendActions)
        walletAddress
        (Just walletTLSParams)
  where
    -- Gets the Wai `Application` to run.
    getApplication :: SendActions WalletWebMode -> WalletWebMode Application
    getApplication sendActions = do
      logInfo "DAEDALUS has STARTED!"
      saVar <- asks wwmcSendActions
      atomically $ STM.putTMVar saVar sendActions
      when (isDebugMode walletRunMode) $ addInitialRichAccount 0
      wsConn <- getWalletWebSockets
      natV0 <- V0.nat
      syncWalletsWithGState =<< mapM findKey =<< myRootAddresses
      startPendingTxsResubmitter
      launchNotifier natV0
      return $ upgradeApplicationWS wsConn $ serve API.walletAPI (API.walletServer natV0)
