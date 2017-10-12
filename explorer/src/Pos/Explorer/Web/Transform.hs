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

import qualified Control.Monad.Catch              as Catch (Handler (..), catches)
import           Control.Monad.Except             (MonadError (throwError))
import qualified Control.Monad.Reader             as Mtl
import           Mockable                         (runProduction)
import           Servant.Server                   (Handler)
import           Servant.Utils.Enter              ((:~>) (..), enter)

import           Pos.Communication                (OutSpecs, SendActions, WorkerSpec,
                                                   worker)
import           Pos.Configuration                (HasNodeConfiguration)
import           Pos.Core                         (HasConfiguration)
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.Recovery                     ()
import           Pos.Ssc.GodTossing               (SscGodTossing)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Txp                          (MempoolExt, MonadTxpLocal (..))
import           Pos.Update.Configuration         (HasUpdateConfiguration)
import           Pos.Util.CompileInfo             (HasCompileInfo)
import           Pos.WorkMode                     (RealMode, RealModeContext (..))

import           Pos.Explorer                     (ExplorerBListener, ExplorerExtra,
                                                   eTxNormalize, eTxProcessTransaction,
                                                   runExplorerBListener)
import           Pos.Explorer.ExtraContext        (ExtraContext, ExtraContextT,
                                                   makeExtraCtx, runExtraContextT)
import           Pos.Explorer.Socket.App          (NotifierSettings, notifierApp)
import           Pos.Explorer.Web.Server          (explorerApp, explorerHandlers,
                                                   explorerServeImpl)

-----------------------------------------------------------------
-- Transformation to `Handler`
-----------------------------------------------------------------

type RealModeE = RealMode SscGodTossing ExplorerExtra
type ExplorerProd = ExtraContextT (ExplorerBListener RealModeE)

type instance MempoolExt ExplorerProd = ExplorerExtra

instance (HasConfiguration, HasInfraConfiguration) => MonadTxpLocal RealModeE where
    txpNormalize = eTxNormalize
    txpProcessTx = eTxProcessTransaction

instance (HasConfiguration, HasInfraConfiguration) => MonadTxpLocal ExplorerProd where
    txpNormalize = lift $ lift txpNormalize
    txpProcessTx = lift . lift . txpProcessTx

runExplorerProd :: ExtraContext -> ExplorerProd a -> RealModeE a
runExplorerProd extraCtx = runExplorerBListener . runExtraContextT extraCtx

liftToExplorerProd :: RealModeE a -> ExplorerProd a
liftToExplorerProd = lift . lift

notifierPlugin
    :: ( HasConfiguration
       , HasNodeConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasGtConfiguration
       , HasCompileInfo
       )
    => NotifierSettings
    -> ([WorkerSpec ExplorerProd], OutSpecs)
notifierPlugin = first pure . worker mempty .
    \settings _sa -> notifierApp @SscGodTossing settings

explorerPlugin
    :: ( HasConfiguration
       , HasNodeConfiguration
       , HasGtConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasCompileInfo
       )
    => Word16
    -> ([WorkerSpec ExplorerProd], OutSpecs)
explorerPlugin port =
    first pure $ worker mempty $
    (\sa -> explorerServeWebReal sa port)

explorerServeWebReal
    :: ( HasConfiguration
       , HasNodeConfiguration
       , HasGtConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasCompileInfo
       )
    => SendActions ExplorerProd
    -> Word16
    -> ExplorerProd ()
explorerServeWebReal sendActions = explorerServeImpl
    (explorerApp $ flip enter (explorerHandlers sendActions) <$> nat)

nat :: HasConfiguration => ExplorerProd (ExplorerProd :~> Handler)
nat = do
    rctx <- ask
    pure $ NT (convertHandler rctx)

convertHandler
    :: HasConfiguration
    => RealModeContext SscGodTossing ExplorerExtra
    -> ExplorerProd a
    -> Handler a
convertHandler rctx handler =
    let extraCtx = makeExtraCtx
        ioAction = realRunner $
                   runExplorerProd extraCtx
                   handler
    in liftIO ioAction `Catch.catches` excHandlers
  where
    realRunner :: forall t . RealModeE t -> IO t
    realRunner act = runProduction $ Mtl.runReaderT act rctx

    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
