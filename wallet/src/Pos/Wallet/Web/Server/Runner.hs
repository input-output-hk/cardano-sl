{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `RealMode`-related part of full-node implementation of
-- Daedalus API.

module Pos.Wallet.Web.Server.Runner
       ( walletServeWebFull
       , runWRealMode
       , runWWebMode
       , walletWebModeContext
       , convertHandler
       , notifierPlugin
       , ExtraNodeArgs(..)

       -- * Re-Exports
       , CommonNodeArgs(..)
       , NodeArgs(..)
       ) where

import           Universum

import           Control.Concurrent.STM (newTQueueIO)
import           Control.Monad.Except (MonadError (throwError))
import           Data.Default (Default (..))
import           Network.Wai (Application)
import           Ntp.Client (NtpStatus)
import           Servant.Server (Handler)
import           System.Wlog (LoggerName (..), logInfo, usingLoggerName)

import           Cardano.NodeIPC (startNodeJsIPC)
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Params (getNodeParams, gtSscParams)
import           Pos.Core (Timestamp (..))
import           Pos.Core.Configuration (epochSlots)
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Infra.Reporting (noReporter)
import           Pos.Infra.Shutdown.Class (HasShutdownContext (shutdownContext))
import           Pos.Infra.Util.JsonLog.Events (jsonLogConfigFromHandle)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations,
                     NodeParams (..), NodeResources (..),
                     allocateNodeResources, releaseNodeResources,
                     withConfigurations)
import           Pos.Launcher.Runner (runRealMode)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Util.Util (HasLens (..))
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Methods (addInitialRichAccount)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..),
                     WalletWebModeContextTag, realModeToWalletWebMode,
                     walletWebModeToRealMode)
import           Pos.Wallet.Web.Server.Launcher (walletApplication,
                     walletServeImpl, walletServer)
import           Pos.Wallet.Web.Sockets (ConnectionsVar, launchNotifier)
import           Pos.Wallet.Web.State (WalletDB)
import           Pos.Wallet.Web.State.Acidic (closeState, openState)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Web (TlsParams)
import           Pos.WorkMode (RealModeContext (..))

import qualified Control.Exception.Safe as E
import qualified Control.Monad.Reader as Mtl

data ExtraNodeArgs = ExtraNodeArgs
    { _nodePath    :: !FilePath
    , _walletPath  :: !FilePath
    , _configPath  :: !FilePath
    , _configKey   :: !Text
    , _systemStart :: Timestamp
    , _loggerName  :: LoggerName
    }

-- | 'WalletWebMode' headless one-shot runner. If differs from `runWRealMode`
-- by several aspects:
--
--     - It runs the given action and returns, whereas `runWRealMode`
--       actually starts a server waiting for an explicit shutdown.
--
--     - It doesn't run the full diffusion layer, so it's made only for
--       isolate actions on the wallet.
--
--     - It only requires 'external' configurations parameters, as they'd be
--       passed to a node and it handles the internal logic. This makes it much
--       easier to use as one can merely specify a configuration file, a few
--       paths and magic happens.
runWWebMode
    :: forall a
    .  CommonNodeArgs
    -> NodeArgs
    -> ExtraNodeArgs
    -> ((HasConfigurations, HasCompileInfo) => WalletWebMode a)
    -> IO a
runWWebMode commonNodeArgs nodeArgs ExtraNodeArgs{..} action = do
    let configOpts = ConfigurationOptions
            { cfoFilePath    = _configPath
            , cfoKey         = _configKey
            , cfoSystemStart = Just _systemStart
            , cfoSeed        = Nothing
            }

    withCompileInfo $ withConfigurations Nothing configOpts
        $ \_ pm -> bracket (openState True _walletPath) closeState
        $ \db -> do
            params <- getNodeParams _loggerName commonNodeArgs nodeArgs
            let (Just vssSK) = npUserSecret params ^. usVss
            let ssc          = gtSscParams commonNodeArgs vssSK (npBehaviorConfig params)
            let txp          = txpGlobalSettings pm
            let mode         = initNodeDBs pm epochSlots
            let acquire      = allocateNodeResources params ssc txp mode
            bracket acquire releaseNodeResources $ \res -> do
                ctx <- WalletWebModeContext
                    <$> pure db
                    <*> newTVarIO def
                    <*> liftIO newTQueueIO
                    <*> newRealModeContext res

                action `runReaderT` ctx
  where
    newRealModeContext :: NodeResources WalletMempoolExt -> IO (RealModeContext ())
    newRealModeContext NodeResources{..} = RealModeContext
        <$> pure nrDBs
        <*> pure nrSscState
        <*> pure nrTxpState
        <*> pure nrDlgState
        <*> jsonLogConfigFromHandle stdout
        <*> pure (LoggerName "integration_tests")
        <*> pure nrContext
        <*> pure noReporter


-- | 'WalletWebMode' runner.
runWRealMode
    :: forall a .
       ( HasConfigurations
       , HasCompileInfo
       )
    => ProtocolMagic
    -> WalletDB
    -> ConnectionsVar
    -> SyncQueue
    -> NodeResources WalletMempoolExt
    -> (Diffusion WalletWebMode -> WalletWebMode a)
    -> IO a
runWRealMode pm db conn syncRequests res action =
    runRealMode pm res $ \diffusion ->
        walletWebModeToRealMode db conn syncRequests $
            action (hoistDiffusion realModeToWalletWebMode (walletWebModeToRealMode db conn syncRequests) diffusion)

walletServeWebFull
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => ProtocolMagic
    -> Diffusion WalletWebMode
    -> TVar NtpStatus
    -> Bool                    -- ^ whether to include genesis keys
    -> NetworkAddress          -- ^ IP and Port to listen
    -> Maybe TlsParams
    -> WalletWebMode ()
walletServeWebFull pm diffusion ntpStatus debug address mTlsParams = do
    ctx <- view shutdownContext
    let
      portCallback :: Word16 -> IO ()
      portCallback port = usingLoggerName "NodeIPC" $ flip runReaderT ctx $ startNodeJsIPC port
    walletServeImpl action address mTlsParams Nothing (Just portCallback)
  where
    action :: WalletWebMode Application
    action = do
        logInfo "Wallet Web API has STARTED!"
        when debug $ addInitialRichAccount 0

        wwmc <- walletWebModeContext
        walletApplication $
            walletServer @WalletWebModeContext @WalletWebMode pm diffusion ntpStatus (convertHandler wwmc)

walletWebModeContext :: WalletWebMode WalletWebModeContext
walletWebModeContext = view (lensOf @WalletWebModeContextTag)

convertHandler
    :: WalletWebModeContext
    -> WalletWebMode a
    -> Handler a
convertHandler wwmc handler =
    liftIO (walletRunner handler) `E.catches` excHandlers
  where

    walletRunner :: forall a . WalletWebMode a -> IO a
    walletRunner act =
        Mtl.runReaderT act wwmc

    excHandlers = [E.Handler catchServant]
    catchServant = throwError

notifierPlugin :: (HasConfigurations) => WalletWebMode ()
notifierPlugin = do
    wwmc <- walletWebModeContext
    launchNotifier (convertHandler wwmc)
