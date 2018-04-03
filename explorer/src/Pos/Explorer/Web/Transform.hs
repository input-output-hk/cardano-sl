{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

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
import           Mockable (runProduction)
import           Servant.Server (Handler, hoistServer)

import           Pos.Block.Configuration (HasBlockConfiguration)
import           Pos.Communication (OutSpecs)
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (HasConfiguration)
import           Pos.Diffusion.Types (Diffusion)
import           Pos.Recovery ()
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Txp (HasTxpConfiguration, MempoolExt, MonadTxpLocal (..))
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Mockable ()
import           Pos.Worker.Types (WorkerSpec, worker)
import           Pos.WorkMode (RealMode, RealModeContext (..))

import           Pos.Explorer.BListener (ExplorerBListener, runExplorerBListener)
import           Pos.Explorer.ExtraContext (ExtraContext, ExtraContextT, makeExtraCtx,
                                            runExtraContextT)
import           Pos.Explorer.Socket.App (NotifierSettings, notifierApp)
import           Pos.Explorer.Txp (ExplorerExtraModifier, eTxNormalize, eTxProcessTransaction)
import           Pos.Explorer.Web.Api (explorerApi)
import           Pos.Explorer.Web.Server (explorerApp, explorerHandlers, explorerServeImpl)

-----------------------------------------------------------------
-- Transformation to `Handler`
-----------------------------------------------------------------

type RealModeE = RealMode ExplorerExtraModifier
type ExplorerProd = ExtraContextT (ExplorerBListener RealModeE)

type instance MempoolExt ExplorerProd = ExplorerExtraModifier

instance (HasConfiguration, HasTxpConfiguration, HasCompileInfo) =>
         MonadTxpLocal RealModeE where
    txpNormalize = eTxNormalize
    txpProcessTx = eTxProcessTransaction

instance (HasConfiguration, HasTxpConfiguration, HasCompileInfo) =>
         MonadTxpLocal ExplorerProd where
    txpNormalize = lift $ lift txpNormalize
    txpProcessTx = lift . lift . txpProcessTx

runExplorerProd :: ExtraContext -> ExplorerProd a -> RealModeE a
runExplorerProd extraCtx = runExplorerBListener . runExtraContextT extraCtx

liftToExplorerProd :: RealModeE a -> ExplorerProd a
liftToExplorerProd = lift . lift

type HasExplorerConfiguration =
    ( HasConfiguration
    , HasBlockConfiguration
    , HasNodeConfiguration
    , HasUpdateConfiguration
    , HasSscConfiguration
    , HasCompileInfo
    )

notifierPlugin
    :: HasExplorerConfiguration
    => NotifierSettings
    -> ([WorkerSpec ExplorerProd], OutSpecs)
notifierPlugin = first pure . worker mempty .
    \settings _sa -> notifierApp settings

explorerPlugin
    :: HasExplorerConfiguration
    => Word16
    -> ([WorkerSpec ExplorerProd], OutSpecs)
explorerPlugin port =
    first pure $ worker mempty $
    (\sa -> explorerServeWebReal sa port)

explorerServeWebReal
    :: HasExplorerConfiguration
    => Diffusion ExplorerProd
    -> Word16
    -> ExplorerProd ()
explorerServeWebReal diffusion port = do
    rctx <- ask
    let handlers = explorerHandlers diffusion
        server = hoistServer explorerApi (convertHandler rctx) handlers
        app = explorerApp (pure server)
    explorerServeImpl app port

convertHandler
    :: HasConfiguration
    => RealModeContext ExplorerExtraModifier
    -> ExplorerProd a
    -> Handler a
convertHandler rctx handler =
    let extraCtx = makeExtraCtx
        ioAction = realRunner $
                   runExplorerProd extraCtx
                   handler
    in liftIO ioAction `E.catches` excHandlers
  where
    realRunner :: forall t . RealModeE t -> IO t
    realRunner act = runProduction $ Mtl.runReaderT act rctx

    excHandlers = [E.Handler catchServant]
    catchServant = throwError
