{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : JsonLog.Event
Description : JSON log messages
License:      MIT
Maintainer:   lars.bruenjes@iohk.io
Stability:    experimental
Portability:  GHC

This module defines types of JSON log events
and functions to work with them.

Seeing as method @'JsonLog.CanJsonLog.jsonLog'@ allows to log events of arbitrary types
which can be serialized to JSON, for the /parsing/ of JSON logs,
three scenarios are supported:

  (1) If /all/ events are known to be of the same type @a@,
  then the @'FromJSON'@ instance of @'JLTimed' a@ can be used.

  (2) If all events are known to be in a /list of types/,
  parsing can be done using function @'handleEvent'@,
  which takes a list of handlers for different types of events.

  (3) Finally, if the event types are unknown at runtime,
  the @'FromJSON'@ instance of @'JLTimedEvent'@ can be used
  to parse timestamp and JSON content of /untyped/ events.
-}

module JsonLog.Event
    ( JLTimed (..)
    , JLTimedEvent (..)
    , toEvent
    , fromEvent
    , timedIO
    , Handler (..)
    , handleEvent
    ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson
import           Data.Time.Units (Microsecond)

import           Mockable.CurrentTime (realTime)

-- | A /typed/ JSON log events.
data JLTimed a = JLTimed
    { jltTimestamp :: !Microsecond -- ^ The timestamp.
    , jltContent   :: !a           -- ^ The typed event content.
    }
    deriving (Show, Functor)

instance ToJSON a => ToJSON (JLTimed a) where

    toJSON = toJSON . toEvent

instance FromJSON a => FromJSON (JLTimed a) where

    parseJSON v = do
        JLTimedEvent (JLTimed ts v') <- parseJSON v
        x                            <- parseJSON v'
        return $ JLTimed ts x

-- | A wrapper around @'JLTimed' 'Value'@, representing
-- /untyped/ JSON log events.
newtype JLTimedEvent = JLTimedEvent { runJLTimedEvent :: JLTimed Value }
    deriving Show

instance ToJSON JLTimedEvent where

    toJSON (JLTimedEvent (JLTimed ts v)) = object
        [ "timestamp" .= (fromIntegral ts :: Integer)
        , "event"     .= v
        ]

instance FromJSON JLTimedEvent where

    parseJSON = withObject "JLTimedEvent" $ \v -> JLTimedEvent <$>
        (JLTimed
            <$> (f <$> v .: "timestamp")
            <*>        v .: "event")
      where
        f :: Integer -> Microsecond
        f = fromIntegral

-- | Converts a /typed/- into an /untyped/ event.
toEvent :: ToJSON a => JLTimed a -> JLTimedEvent
toEvent = JLTimedEvent . fmap toJSON

-- | Tries to parse a /typed/ event from an /untyped/ one.
-- Returns a @'Right'@, containing the typed event, if conversion was successful,
-- otherwise a @'Left'@, containing the unparsed JSON.
fromEvent :: forall a. FromJSON a => JLTimedEvent -> Either (JLTimed Value) (JLTimed a)
fromEvent = f . runJLTimedEvent
  where
    f :: JLTimed Value -> Either (JLTimed Value) (JLTimed a)
    f e@(JLTimed ts v) = case fromJSON v of
        Error _   -> Left e
        Success x -> Right (JLTimed ts x)

-- | Creates a timed event, given some content,
-- by adding the current time as timestamp.
timedIO :: MonadIO m => a -> m (JLTimed a)
timedIO x = realTime >>= \ts -> return (JLTimed ts x)

-- | A value of type @'Handler' a@ handles
-- /typed/ events (of type @'JLTimed' b@ for /some/ @b@),
-- by converting them into a value of type @a@.
data Handler a where

    Handler :: FromJSON b => (JLTimed b -> a) -> Handler a

-- | Handles /untyped/ events, given a (possibly empty) list
-- of handlers for /typed/ events.
handleEvent :: (JLTimed Value -> a) -- ^ Handles events for which no handler is specified.
            -> [Handler a]          -- ^ A list of handlers for various event types.
            -> JLTimedEvent         -- ^ The untyped event to handle.
            -> a
handleEvent def hs e = go hs
  where
    go []                = def $ runJLTimedEvent e
    go (Handler h : hs') = case fromEvent e of
        Right x -> h x
        _       -> go hs'
