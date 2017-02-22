-- | Some utils for convinient socket-io usage.

module Pos.Explorer.Web.Sockets.Util
    ( EventName (..)
    , emit
    , emitTo
    , on_
    , on
    ) where

import           Control.Monad.Reader (MonadReader)
import           Control.Monad.State  (MonadState)
import           Control.Monad.Trans  (MonadIO)
import           Data.Aeson.Types     (FromJSON, ToJSON)
import           Data.Text            (Text)
import qualified Network.SocketIO     as S

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

on_ :: (MonadState S.RoutingTable m, EventName name)
    => name -> S.EventHandler a -> m ()
on_ eventName handler = S.on (toName eventName) handler

on :: (MonadState S.RoutingTable m, FromJSON event, EventName name)
   => name -> (event -> S.EventHandler a) -> m ()
on eventName handler = S.on (toName eventName) handler
