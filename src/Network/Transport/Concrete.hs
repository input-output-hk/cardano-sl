{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Transport.Concrete
       ( concrete
       , concreteEvent
       , concreteEndPoint
       ) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Network.Transport          as NT

import           Network.Transport.Abstract

-- | Use a concrete network-transport within the abstract framework,
--   specializing it to some MonadIO.
--   You also have to give a special variant of 'newEndPoint' which takes a
--   'QDisc m t'.
concrete
    :: ( MonadIO m )
    => (forall t . m t -> IO t)
    -> NT.Transport
    -> (forall t . NT.Transport -> QDisc IO t -> IO (Either (NT.TransportError NT.NewEndPointErrorCode) (EndPoint IO t)))
    -> Transport m
concrete lowerIO transport ntNewEndPoint = Transport {
      newEndPoint = concreteNewEndPoint . ntNewEndPoint transport . concreteQDisc lowerIO
    , closeTransport = liftIO $ NT.closeTransport transport
    }

concreteQDisc
    :: (forall t . m t -> IO t)
    -> QDisc m t
    -> QDisc IO t
concreteQDisc lowerIO qdisc = QDisc {
      qdiscEnqueue = lowerIO . qdiscEnqueue qdisc
    , qdiscDequeue = lowerIO $ qdiscDequeue qdisc
    }

concreteNewEndPoint
    :: ( MonadIO m )
    => IO (Either (TransportError NewEndPointErrorCode) (EndPoint IO t))
    -> m (Either (TransportError NewEndPointErrorCode) (EndPoint m t))
concreteNewEndPoint ntNewEndPoint = (fmap . fmap) concreteEndPoint (liftIO ntNewEndPoint)

concreteEndPoint :: ( MonadIO m ) => EndPoint IO t -> EndPoint m t
concreteEndPoint ep = EndPoint {
      receive = liftIO $ receive ep
    , address = address ep
    , connect = concreteConnect (connect ep)
    , closeEndPoint = liftIO $ closeEndPoint ep
    }

concreteEvent :: NT.Event -> Event
concreteEvent ev = case ev of
    NT.Received eid chunks -> Received eid chunks
    NT.ConnectionClosed eid -> ConnectionClosed eid
    NT.ConnectionOpened eid reliability address -> ConnectionOpened eid reliability address
    NT.EndPointClosed -> EndPointClosed
    NT.ErrorEvent (TransportError err str) -> ErrorEvent (TransportError (EventError err) str)
    _ -> ErrorEvent (TransportError UnsupportedEvent "Unsupported event")

concreteConnect
    :: ( MonadIO m )
    => (EndPointAddress -> Reliability -> ConnectHints -> IO (Either (TransportError ConnectErrorCode) (Connection IO)))
    -> (EndPointAddress -> Reliability -> ConnectHints -> m (Either (TransportError ConnectErrorCode) (Connection m)))
concreteConnect ntConnect endPointAddress reliability hints = do
    choice <- liftIO $ ntConnect endPointAddress reliability hints
    pure (fmap concreteConnection choice)

concreteConnection :: ( MonadIO m ) => Connection IO -> Connection m
concreteConnection ntConnection = Connection {
      send = liftIO . send ntConnection
    , close = liftIO $ close ntConnection
    }
