{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

-- | Web server.

module Pos.Web.Server
       ( serveWebBase
       , applicationBase
       , serveWebGT
       , applicationGT
       ) where

import qualified Control.Monad.Catch                  as Catch
import           Control.Monad.Except                 (MonadError (throwError))
import           Control.TimeWarp.Timed               (TimedIO, runTimedIO)
import           Formatting                           (ords, sformat, stext, (%))
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.API                          ((:<|>) ((:<|>)), FromHttpApiData)
import           Servant.Server                       (Handler, ServantErr (errBody),
                                                       Server, ServerT, err404, serve)
import           Servant.Utils.Enter                  ((:~>) (Nat), enter)
import           Universum

import           Pos.Slotting                         (getCurrentSlot)
import           Pos.Ssc.Class                        (SscConstraint)
import           Pos.Ssc.GodTossing                   (SscGodTossing)
import qualified Pos.State                            as St
import           Pos.Types                            (EpochIndex (..), SlotId (siEpoch),
                                                       SlotLeaders, headerHash)
import           Pos.Web.Api                          (BaseNodeApi, GodTossingApi,
                                                       GtNodeApi, baseNodeApi, gtNodeApi)
import           Pos.WorkMode                         (ContextHolder, DBHolder,
                                                       NodeContext, WorkMode,
                                                       getNodeContext, ncPublicKey,
                                                       runContextHolder, runDBHolder)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

-- [CSL-152]: I want SscConstraint to be part of WorkMode.
type MyWorkMode ssc m = (WorkMode ssc m, SscConstraint ssc)

serveWebBase :: MyWorkMode ssc m => Word16 -> m ()
serveWebBase = serveImpl applicationBase

applicationBase :: MyWorkMode ssc m => m Application
applicationBase = do
    server <- servantServerBase
    return $ serve baseNodeApi server

serveWebGT :: MyWorkMode SscGodTossing m => Word16 -> m ()
serveWebGT = serveImpl applicationGT

applicationGT :: MyWorkMode SscGodTossing m => m Application
applicationGT = do
    server <- servantServerGT
    return $ serve gtNodeApi server

-- [CSL-217]: do not hardcode logStdoutDev.
serveImpl :: MonadIO m => m Application -> Word16 -> m ()
serveImpl application port =
    liftIO . run (fromIntegral port) . logStdoutDev =<< application

----------------------------------------------------------------------------
-- Servant infrastructure
----------------------------------------------------------------------------

type WebHandler ssc = ContextHolder (DBHolder ssc TimedIO)

convertHandler
    :: forall ssc a.
       NodeContext -> St.NodeState ssc -> WebHandler ssc a -> Handler a
convertHandler nc ns handler =
    liftIO (runTimedIO (runDBHolder ns (runContextHolder nc handler))) `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: MyWorkMode ssc m => m (WebHandler ssc :~> Handler)
nat = do
    nc <- getNodeContext
    ns <- St.getNodeState
    return $ Nat (convertHandler nc ns)

servantServerBase :: forall ssc m . MyWorkMode ssc m => m (Server (BaseNodeApi ssc))
servantServerBase = flip enter baseServantHandlers <$> (nat @ssc @m)

servantServerGT :: forall m . WorkMode SscGodTossing m => m (Server GtNodeApi)
servantServerGT = flip enter (baseServantHandlers :<|> gtServantHandlers) <$>
    (nat @SscGodTossing @m)

----------------------------------------------------------------------------
-- Base handlers
----------------------------------------------------------------------------

baseServantHandlers
    :: SscConstraint ssc
    => ServerT (BaseNodeApi ssc) (WebHandler ssc)
baseServantHandlers =
    getCurrentSlot :<|> getLeaders :<|> (ncPublicKey <$> getNodeContext) :<|>
    (headerHash <$> St.getHeadBlock)

getLeaders :: SscConstraint ssc => Maybe EpochIndex -> WebHandler ssc SlotLeaders
getLeaders e = maybe (throwM err) pure =<< getLeadersDo e
  where
    epochStr = maybe "current" (sformat ords) e
    err =
        err404
        { errBody =
            encodeUtf8 $
            sformat ("Leaders are not know for "%stext%" epoch") epochStr
        }

getLeadersDo
    :: SscConstraint ssc
    => Maybe EpochIndex -> WebHandler ssc (Maybe SlotLeaders)
getLeadersDo Nothing  = St.getLeaders . siEpoch =<< getCurrentSlot
getLeadersDo (Just e) = St.getLeaders e

----------------------------------------------------------------------------
-- GodTossing handlers
----------------------------------------------------------------------------

gtServantHandlers :: ServerT GodTossingApi (WebHandler SscGodTossing)
gtServantHandlers = toggleGtParticipation

toggleGtParticipation :: Bool -> WebHandler SscGodTossing ()
toggleGtParticipation = const pass

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance FromHttpApiData EpochIndex
