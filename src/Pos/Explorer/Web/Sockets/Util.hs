module Pos.Explorer.Web.Sockets.Util
    ( EventName (..)
    , emit
    , emitJSON
    , emitTo
    , emitJSONTo
    , on_
    , on

    , runPeriodicallyUnless
    , forkAccompanion
    ) where

import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, writeTVar)
import           Control.Monad.Catch         (MonadCatch)
import           Control.Monad.Reader        (MonadReader)
import           Control.Monad.State         (MonadState)
import           Control.Monad.Trans         (MonadIO)
import           Data.Aeson.Types            (Array, FromJSON, ToJSON)
import           Data.Text                   (Text)
import           Data.Time.Units             (TimeUnit (..))
import           Formatting                  (sformat, shown, (%))
import           Mockable                    (Fork, Mockable, fork)
import qualified Network.SocketIO            as S
import           System.Wlog                 (WithLogger, logWarning)
import           Universum                   hiding (on)

-- * Provides type-safity for event names in some socket-io functions.

class EventName a where
    toName :: a -> Text

emit
    :: (ToJSON event, EventName name, MonadReader S.Socket m, MonadIO m)
    => name -> event -> m ()
emit eventName event = S.emit (toName eventName) event

emitTo
    :: (ToJSON event, EventName name, MonadIO m)
    => S.Socket -> name -> event -> m ()
emitTo sock eventName event = S.emitTo sock (toName eventName) event

emitJSON
    :: (EventName name, MonadReader S.Socket m, MonadIO m)
    => name -> Array -> m ()
emitJSON eventName event = S.emitJSON (toName eventName) event

emitJSONTo
    :: (EventName name, MonadIO m)
    => S.Socket -> name -> Array -> m ()
emitJSONTo sock eventName event = S.emitJSONTo sock (toName eventName) event

on_ :: (MonadState S.RoutingTable m, EventName name)
    => name -> S.EventHandler a -> m ()
on_ eventName handler = S.on (toName eventName) handler

on :: (MonadState S.RoutingTable m, FromJSON event, EventName name)
   => name -> (event -> S.EventHandler a) -> m ()
on eventName handler = S.on (toName eventName) handler

-- * Misc

runPeriodicallyUnless
    :: (MonadIO m, MonadCatch m, WithLogger m, TimeUnit t)
    => t -> m Bool -> s -> StateT s m () -> m ()
runPeriodicallyUnless delay stop initState action =
    let loop st = unlessM stop $ do
            st' <- execStateT action st
                `catchAll` \e -> handler e $> st
            liftIO $ threadDelay $ fromIntegral $ toMicroseconds delay
            loop st'
    in  loop initState
  where
    handler = logWarning . sformat ("Periodic action failed: "%shown)

-- | Fork a side action.
-- It's given a flag, whether main action has completed.
forkAccompanion
    :: (MonadIO m, MonadMask m, Mockable Fork m)
    => (m Bool -> m ()) -> m a -> m a
forkAccompanion accompanion main = do
    stopped <- liftIO $ newTVarIO False
    let whetherStopped = liftIO $ readTVarIO stopped
    bracket_ (fork $ accompanion whetherStopped)
             (atomically $ writeTVar stopped True)
             main
