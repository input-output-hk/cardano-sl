{-# LANGUAGE RankNTypes #-}

module Network.Transport.Concrete.TCP (

      concrete

    ) where

import Control.Monad.IO.Class (MonadIO)
import Network.Transport.Abstract
import qualified Network.Transport.Concrete as C
import qualified Network.Transport as NT
import qualified Network.Transport.TCP as TCP

-- | Use a TCP transport and its internals to make a transport.
--   The internals are necessary because they can accept a QDisc, but the
--   network-transport API does not.
concrete
    :: ( MonadIO m )
    => (forall t . m t -> IO t)
    -> (NT.Transport, TCP.TransportInternals)
    -> Transport m
concrete lowerIO (transport, internals) = C.concrete lowerIO transport ntNewEndPoint
    where
    ntNewEndPoint _ qdisc = do
        choice <- TCP.newEndPointInternal internals (concreteTCPQDisc qdisc)
        case choice of
            Left err -> return (Left err)
            Right tcpEndPoint -> return (Right (concreteTCPEndPoint tcpEndPoint))

concreteTCPEndPoint
    :: ( )
    => TCP.TCPEndPoint t
    -> EndPoint IO t
concreteTCPEndPoint tcpEndPoint = EndPoint {
      receive = TCP.tcpReceive tcpEndPoint
    , address = TCP.tcpAddress tcpEndPoint
    , connect = \addr reliability hints -> do
          outcome <- TCP.tcpConnect tcpEndPoint addr reliability hints
          return $ fmap concreteTCPConnection outcome
    , closeEndPoint = TCP.tcpCloseEndPoint tcpEndPoint
    }

concreteTCPConnection
    :: ( )
    => NT.Connection
    -> Connection IO
concreteTCPConnection conn = Connection {
      send = NT.send conn
    , close = NT.close conn
    }

concreteTCPQDisc
    :: ( )
    => QDisc IO t
    -> TCP.QDisc t
concreteTCPQDisc qdisc = TCP.QDisc {
      TCP.qdiscEnqueue = qdiscEnqueue qdisc . C.concreteEvent
    , TCP.qdiscDequeue = qdiscDequeue qdisc
    }
