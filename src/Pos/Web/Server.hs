{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

-- | Web server.

module Pos.Web.Server
       ( serveWeb
       ) where

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Except     (MonadError (throwError))
import           Control.TimeWarp.Timed   (TimedIO, runTimedIO)
import           Formatting               (ords, sformat, stext, (%))
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant.API              ((:<|>) ((:<|>)), FromHttpApiData)
import           Servant.Server           (Handler, ServantErr (errBody), Server, ServerT,
                                           err404, serve)
import           Servant.Utils.Enter      ((:~>) (Nat), enter)
import           Universum

import           Pos.Slotting             (getCurrentSlot)
import           Pos.Ssc.Class            (SscConstraint)
import qualified Pos.State                as St
import           Pos.Types                (EpochIndex (..), SlotId (siEpoch), SlotLeaders,
                                           headerHash)
import           Pos.Web.Api              (NodeApi, nodeApi)
import           Pos.WorkMode             (ContextHolder, DBHolder, NodeContext, WorkMode,
                                           getNodeContext, ncPublicKey, runContextHolder,
                                           runDBHolder)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

-- [CSL-152]: I want SscConstraint to be part of WorkMode.
type MyWorkMode ssc m = (WorkMode ssc m, SscConstraint ssc)

serveWeb :: MyWorkMode ssc m => Word16 -> m ()
serveWeb port = liftIO . run (fromIntegral port) =<< application

application :: MyWorkMode ssc m => m Application
application = do
    server <- servantServer
    return $ serve nodeApi server

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

servantServer :: forall ssc m . MyWorkMode ssc m => m (Server (NodeApi ssc))
servantServer = flip enter servantHandlers <$> (nat @ssc @m)

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

servantHandlers
    :: SscConstraint ssc
    => ServerT (NodeApi ssc) (WebHandler ssc)
servantHandlers =
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
-- Orphan instances
----------------------------------------------------------------------------

deriving instance FromHttpApiData EpochIndex
