{-# LANGUAGE RankNTypes    #-}
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

import qualified Control.Monad.Catch       as Catch (Handler (..), catches)
import           Control.Monad.Except      (MonadError (throwError))
import qualified Control.Monad.Reader      as Mtl
import           Mockable                  (runProduction)
import           Servant.Server            (Handler)
import           Servant.Utils.Enter       ((:~>) (..), enter)

import           Pos.Communication         (OutSpecs, SendActions, WorkerSpec, worker)
import           Pos.Context               (NodeContext)
import           Pos.Core                  (HasCoreConstants)
import           Pos.Recovery              ()
import           Pos.Ssc.GodTossing        (SscGodTossing)
import           Pos.Util.Util             (lensOf)
import           Pos.WorkMode              (RealMode, RealModeContext (..))

import           Pos.Explorer              (ExplorerBListener, runExplorerBListener)
import           Pos.Explorer.ExtraContext (ExtraContext, ExtraContextT, makeExtraCtx,
                                            runExtraContextT)
import           Pos.Explorer.Socket.App   (NotifierSettings, notifierApp)
import           Pos.Explorer.Web.Server   (explorerApp, explorerHandlers,
                                            explorerServeImpl)

-----------------------------------------------------------------
-- Transformation to `Handler`
-----------------------------------------------------------------

type ExplorerProd = ExtraContextT (ExplorerBListener (RealMode SscGodTossing))

runExplorerProd :: ExtraContext -> ExplorerProd a -> RealMode SscGodTossing a
runExplorerProd extraCtx = runExplorerBListener . runExtraContextT extraCtx

liftToExplorerProd :: RealMode SscGodTossing a -> ExplorerProd a
liftToExplorerProd = lift . lift

notifierPlugin :: HasCoreConstants => NotifierSettings -> ([WorkerSpec ExplorerProd], OutSpecs)
notifierPlugin = first pure . worker mempty .
    \settings _sa -> notifierApp @SscGodTossing settings

explorerPlugin
    :: HasCoreConstants
    => Word16
    -> ([WorkerSpec ExplorerProd], OutSpecs)
explorerPlugin port =
    first pure $ worker mempty $
    (\sa -> explorerServeWebReal sa port)

explorerServeWebReal
    :: HasCoreConstants
    => SendActions ExplorerProd
    -> Word16
    -> ExplorerProd ()
explorerServeWebReal sendActions = explorerServeImpl
    (explorerApp $ flip enter (explorerHandlers sendActions) <$> nat)

nat :: ExplorerProd (ExplorerProd :~> Handler)
nat = do
    rctx <- ask
    pure $ NT (convertHandler rctx)

convertHandler
    :: RealModeContext SscGodTossing
    -> ExplorerProd a
    -> Handler a
convertHandler rctx handler =
    let extraCtx = makeExtraCtx $ view (lensOf @NodeContext) rctx
        ioAction = realRunner $
                   runExplorerProd extraCtx
                   handler
    in liftIO ioAction `Catch.catches` excHandlers
  where

    realRunner :: forall t . RealMode SscGodTossing t -> IO t
    realRunner act = runProduction $ Mtl.runReaderT act rctx

    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
