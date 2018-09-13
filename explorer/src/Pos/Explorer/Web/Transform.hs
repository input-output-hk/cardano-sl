{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Explorer.Web.Transform
       ( ExplorerProd
       , runExplorerProd
       , liftToExplorerProd
       , explorerServeWebReal
       , explorerPlugin
       , notifierPlugin
       ) where

import           Universum

import qualified Control.Exception.Safe as E
import           Control.Monad.Except (MonadError (throwError))
import qualified Control.Monad.Reader as Mtl
import           Servant.Server (Handler, hoistServer)

import           Pos.Chain.Block (HasBlockConfiguration)
import           Pos.Chain.Ssc (HasSscConfiguration)
import           Pos.Chain.Update (HasUpdateConfiguration)
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core as Core (Config (..))
import           Pos.DB.Txp (MempoolExt, MonadTxpLocal (..))
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Reporting (MonadReporting (..))
import           Pos.Recovery ()
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.WorkMode (RealMode, RealModeContext (..))

import           Pos.Explorer.BListener (ExplorerBListener,
                     runExplorerBListener)
import           Pos.Explorer.ExtraContext (ExtraContext, ExtraContextT,
                     makeExtraCtx, runExtraContextT)
import           Pos.Explorer.Socket.App (NotifierSettings, notifierApp)
import           Pos.Explorer.Txp (ExplorerExtraModifier, eTxNormalize,
                     eTxProcessTransaction)
import           Pos.Explorer.Web.Api (explorerApi)
import           Pos.Explorer.Web.Server (explorerApp, explorerHandlers,
                     explorerServeImpl)

-----------------------------------------------------------------
-- Transformation to `Handler`
-----------------------------------------------------------------

type RealModeE = RealMode ExplorerExtraModifier
type ExplorerProd = ExtraContextT (ExplorerBListener RealModeE)

type instance MempoolExt ExplorerProd = ExplorerExtraModifier

instance MonadTxpLocal RealModeE where
    txpNormalize = eTxNormalize
    txpProcessTx = eTxProcessTransaction

instance MonadTxpLocal ExplorerProd where
    txpNormalize pm = lift . lift . txpNormalize pm
    txpProcessTx coreConfig txpConfig = lift . lift . txpProcessTx coreConfig txpConfig

-- | Use the 'RealMode' instance.
-- FIXME instance on a type synonym.
instance MonadReporting ExplorerProd where
    report = lift . lift . report

runExplorerProd :: ExtraContext -> ExplorerProd a -> RealModeE a
runExplorerProd extraCtx = runExplorerBListener . runExtraContextT extraCtx

liftToExplorerProd :: RealModeE a -> ExplorerProd a
liftToExplorerProd = lift . lift

type HasExplorerConfiguration =
    ( HasBlockConfiguration
    , HasNodeConfiguration
    , HasUpdateConfiguration
    , HasSscConfiguration
    , HasCompileInfo
    )

notifierPlugin
    :: HasExplorerConfiguration
    => Core.Config
    -> NotifierSettings
    -> Diffusion ExplorerProd
    -> ExplorerProd ()
notifierPlugin coreConfig settings _ = notifierApp coreConfig settings

explorerPlugin
    :: HasExplorerConfiguration
    => Core.Config
    -> Word16
    -> Diffusion ExplorerProd
    -> ExplorerProd ()
explorerPlugin coreConfig = flip $ explorerServeWebReal coreConfig

explorerServeWebReal
    :: HasExplorerConfiguration
    => Core.Config
    -> Diffusion ExplorerProd
    -> Word16
    -> ExplorerProd ()
explorerServeWebReal coreConfig diffusion port = do
    rctx <- ask
    let handlers = explorerHandlers coreConfig diffusion
        server   = hoistServer
            explorerApi
            (convertHandler coreConfig rctx)
            handlers
        app = explorerApp (pure server)
    explorerServeImpl app port

convertHandler
    :: Core.Config
    -> RealModeContext ExplorerExtraModifier
    -> ExplorerProd a
    -> Handler a
convertHandler coreConfig rctx handler =
    let extraCtx = makeExtraCtx coreConfig
        ioAction = realRunner $
                   runExplorerProd extraCtx
                   handler
    in liftIO ioAction `E.catches` excHandlers
  where
    realRunner :: forall t . RealModeE t -> IO t
    realRunner act = Mtl.runReaderT act rctx

    excHandlers = [E.Handler catchServant]
    catchServant = throwError
