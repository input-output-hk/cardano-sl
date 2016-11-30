{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Web server.

module Pos.Web.Server
       ( serveWeb
       ) where

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Except     (MonadError (throwError))
import           Control.TimeWarp.Timed   (TimedIO, runTimedIO)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant.Server           (Handler, Server, ServerT, serve)
import           Servant.Utils.Enter      ((:~>) (Nat), enter)
import           Universum

import           Pos.Slotting             (getCurrentSlot)
import           Pos.Web.Api              (NodeApi, nodeApi)
import           Pos.WorkMode             (ContextHolder, NodeContext, WorkMode,
                                           getNodeContext, runContextHolder)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

serveWeb :: WorkMode ssc m => Word16 -> m ()
serveWeb port = liftIO . run (fromIntegral port) =<< application

application :: WorkMode ssc m => m Application
application = do
    server <- servantServer
    return $ serve nodeApi server

----------------------------------------------------------------------------
-- Servant infrastructure
----------------------------------------------------------------------------

type WebHandler = ContextHolder TimedIO

convertHandler :: forall a . NodeContext -> WebHandler a -> Handler a
convertHandler nc handler =
    liftIO (runTimedIO (runContextHolder nc handler)) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: WorkMode ssc m => m (WebHandler :~> Handler)
nat = do
    nc <- getNodeContext
    return $ Nat (convertHandler nc)

servantServer :: forall ssc m . WorkMode ssc m => m (Server NodeApi)
servantServer = flip enter servantHandlers <$> (nat @ssc @m)

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

servantHandlers :: ServerT NodeApi WebHandler
servantHandlers = getCurrentSlot
