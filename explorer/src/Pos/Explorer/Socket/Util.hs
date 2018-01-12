{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Explorer.Socket.Util
    ( EventName (..)
    , emit
    , emitJSON
    , emitTo
    , emitJSONTo
    , on_
    , on

    , runPeriodicallyUnless
    , forkAccompanion
    , regroupBySnd
    ) where

import           Control.Monad.Reader (MonadReader)
import           Control.Monad.State (MonadState)
import           Control.Monad.Trans (MonadIO)
import           Data.Aeson.Types (Array, FromJSON, ToJSON)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Time.Units (TimeUnit (..))
import           Formatting (sformat, shown, (%))
import           Network.EngineIO.Wai (WaiMonad)

import           Mockable (Fork, Mockable, fork)
import qualified Network.SocketIO as S
import           Serokell.Util.Concurrent (threadDelay)
import           System.Wlog (CanLog (..), WithLogger, logWarning)
import           Universum hiding (on)

-- * Provides type-safety for event names in some socket-io functions.

class EventName a where
    toName :: a -> Text

-- ** Socket-io functions which works with `EventName name` rather than
-- with plain `Text`.

emit
    :: (ToJSON event, EventName name, MonadReader S.Socket m, MonadIO m)
    => name -> event -> m ()
emit eventName =
    S.emit $ toName eventName
    -- logDebug . sformat ("emit "%stext) $ toName eventName

emitTo
    :: (ToJSON event, EventName name, MonadIO m)
    => S.Socket -> name -> event -> m ()
emitTo sock eventName = S.emitTo sock (toName eventName)

emitJSON
    :: (EventName name, MonadReader S.Socket m, MonadIO m)
    => name -> Array -> m ()
emitJSON eventName = S.emitJSON (toName eventName)

emitJSONTo
    :: (EventName name, MonadIO m)
    => S.Socket -> name -> Array -> m ()
emitJSONTo sock eventName = S.emitJSONTo sock (toName eventName)

on_ :: (MonadState S.RoutingTable m, EventName name)
    => name -> S.EventHandler a -> m ()
on_ eventName = S.on (toName eventName)

on :: (MonadState S.RoutingTable m, FromJSON event, EventName name)
   => name -> (event -> S.EventHandler a) -> m ()
on eventName = S.on (toName eventName)

-- * Instances

instance CanLog WaiMonad where
    dispatchMessage logName sev msg = liftIO $ dispatchMessage logName sev msg

-- * Misc

-- | Runs an action periodically.
-- It's provided with a flag whether repetition should be stopped.
-- Action is launched with state. If action fails, state remains unmodified.
runPeriodicallyUnless
    :: (MonadIO m, MonadCatch m, WithLogger m, TimeUnit t)
    => t -> m Bool -> s -> StateT s m () -> m ()
runPeriodicallyUnless delay stop initState action =
    let loop st = unlessM stop $ do
            st' <- execStateT action st
                `catchAny` \e -> handler e $> st
            threadDelay delay
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
    stopped <- newTVarIO False
    let whetherStopped = readTVarIO stopped
    bracket_ (fork $ accompanion whetherStopped)
             (atomically $ writeTVar stopped True)
             main

regroupBySnd
    :: forall a b l. (Ord b, Container l, Element l ~ b)
    => [(a, l)] -> M.Map b [a]
regroupBySnd info =
    let entries :: [(b, a)]
        entries = fmap swap $ concatMap sequence $ toList <<$>> info
    in  fmap ($ []) $ M.fromListWith (.) $ fmap (second $ (++) . pure) entries
