{- | A collection of plugins used by this edge node.
     A @Plugin@ is essentially a set of actions which will be run in
     a particular monad, at some point in time.
-}
{-# LANGUAGE TupleSections #-}
module Cardano.Wallet.Server.Plugins (
      Plugin
    , acidCleanupWorker
    , conversation
    , walletBackend
    , resubmitterPlugin
    , notifierPlugin
    ) where

import           Universum

import           Cardano.Wallet.API as API
import           Cardano.Wallet.Server as API
import           Cardano.Wallet.Server.CLI (RunMode, WalletBackendParams (..), isDebugMode,
                                            walletAcidInterval, walletDbOptions)

import qualified Control.Concurrent.STM as STM
import           Network.Wai (Application, Middleware)
import           Network.Wai.Middleware.Cors (cors, corsMethods, corsRequestHeaders,
                                              simpleCorsResourcePolicy, simpleMethods)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Pos.Wallet.Web (cleanupAcidStatePeriodically, syncWalletsWithGState,
                                 wwmcSendActions)
import           Pos.Wallet.Web.Account (findKey, myRootAddresses)
import           Pos.Wallet.Web.Methods.Restore (addInitialRichAccount)
import           Pos.Wallet.Web.Pending.Worker (startPendingTxsResubmitter)
import qualified Pos.Wallet.Web.Server.Runner as V0
import           Pos.Wallet.Web.Sockets (getWalletWebSockets, launchNotifier, upgradeApplicationWS)
import           Servant (serve)
import           System.Wlog (logInfo, modifyLoggerName)

import           Pos.Communication (ActionSpec (..), OutSpecs, SendActions, WorkerSpec, worker)
import           Pos.Context (HasNodeContext)

import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.Server.Launcher (walletServeImpl, walletServerOuts)
import           Pos.Web (serveWeb)
import           Pos.WorkMode (WorkMode)

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
        | enableMonitoringApi = [serveWeb monitoringApiPort walletTLSParams]
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
        -- Disable TLS if in debug mode.
        (if (isDebugMode walletRunMode) then Nothing else walletTLSParams)
  where
    -- Gets the Wai `Application` to run.
    getApplication :: SendActions WalletWebMode -> WalletWebMode Application
    getApplication sendActions = do
      logInfo "DAEDALUS has STARTED!"
      saVar <- asks wwmcSendActions
      atomically $ STM.putTMVar saVar sendActions
      when (isDebugMode walletRunMode) $ addInitialRichAccount 0
      wsConn <- getWalletWebSockets
      ctx <- V0.walletWebModeContext
      syncWalletsWithGState =<< mapM findKey =<< myRootAddresses
      launchNotifier (V0.convertHandler ctx)
      let app = upgradeApplicationWS wsConn $ serve API.walletAPI (API.walletServer (V0.convertHandler ctx))
      return $ withMiddleware walletRunMode app

-- | A @Plugin@ to resubmit pending transactions.
resubmitterPlugin :: (HasConfigurations, HasCompileInfo) => Plugin WalletWebMode
resubmitterPlugin = ([ActionSpec $ \_ _ -> startPendingTxsResubmitter], mempty)

-- | A @Plugin@ to notify frontend via websockets.
notifierPlugin :: (HasConfigurations, HasCompileInfo) => Plugin WalletWebMode
notifierPlugin = ([ActionSpec $ \_ _ -> V0.notifierPlugin], mempty)

-- | "Attaches" the middleware to this 'Application', if any.
-- When running in debug mode, chances are we want to at least allow CORS to test the API
-- with a Swagger editor, locally.
withMiddleware :: RunMode -> Application -> Application
withMiddleware wrm app
  | isDebugMode wrm = logStdoutDev . corsMiddleware $ app
  | otherwise = app

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = "PUT" : simpleMethods
        }
