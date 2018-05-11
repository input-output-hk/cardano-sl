{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Transport.Concrete
       ( concrete
       , concreteNewEndPoint
       , concreteEndPoint
       , concreteEvent
       , concreteConnect
       , concreteConnection
       ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Network.Transport as NT

import           Network.Transport.Abstract

-- | Use a concrete network-transport within the abstract framework,
--   specializing it to some MonadIO.
concrete :: ( MonadIO m ) => NT.Transport -> Transport m
concrete transport = Transport {
      newEndPoint = concreteNewEndPoint (NT.newEndPoint transport)
    , closeTransport = liftIO $ NT.closeTransport transport
    }

concreteNewEndPoint
    :: ( MonadIO m )
    => IO (Either (TransportError NewEndPointErrorCode) NT.EndPoint)
    -> m (Either (TransportError NewEndPointErrorCode) (EndPoint m))
concreteNewEndPoint ntNewEndPoint = (fmap . fmap) concreteEndPoint (liftIO ntNewEndPoint)

concreteEndPoint :: ( MonadIO m ) => NT.EndPoint -> EndPoint m
concreteEndPoint ep = EndPoint {
      receive = fmap concreteEvent (liftIO $ NT.receive ep)
    , address = NT.address ep
    , connect = concreteConnect (NT.connect ep)
    , closeEndPoint = liftIO $ NT.closeEndPoint ep
    }

concreteEvent :: NT.Event -> Event
concreteEvent ev = case ev of
    NT.Received eid chunks -> Received eid chunks
    NT.ConnectionClosed eid -> ConnectionClosed eid
    NT.ConnectionOpened eid reliability address -> ConnectionOpened eid reliability address
    NT.EndPointClosed -> EndPointClosed
    NT.ErrorEvent (TransportError err str) -> ErrorEvent (TransportError (EventErrorCode err) str)
    _ -> ErrorEvent (TransportError UnsupportedEvent "Unsupported event")

concreteConnect
    :: ( MonadIO m )
    => (EndPointAddress -> Reliability -> ConnectHints -> IO (Either (TransportError ConnectErrorCode) NT.Connection))
    -> (EndPointAddress -> Reliability -> ConnectHints -> m (Either (TransportError ConnectErrorCode) (Connection m)))
concreteConnect ntConnect endPointAddress reliability hints = do
    choice <- liftIO $ ntConnect endPointAddress reliability hints
    pure (fmap concreteConnection choice)

concreteConnection :: ( MonadIO m ) => NT.Connection -> Connection m
concreteConnection ntConnection = Connection {
      send = liftIO . NT.send ntConnection
    , close = liftIO $ NT.close ntConnection
    , bundle = NT.bundle ntConnection
    }
