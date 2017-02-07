{-# LANGUAGE RankNTypes #-}

module Network.Transport.Concrete.TCP (

      concreteQDisc

    ) where

import Network.Transport.Abstract
import qualified Network.Transport.Concrete as C
import qualified Network.Transport.TCP as TCP

-- | Use a QDisc m t as a TCP QDisc (in IO) by giving a way to run the
--   monad `m` into `IO`.
concreteQDisc
    :: (forall t . m t -> IO t)
    -> QDisc m t
    -> TCP.QDisc t
concreteQDisc lowerIO qdisc = TCP.QDisc {
      TCP.qdiscDequeue = lowerIO $ qdiscDequeue qdisc
    , TCP.qdiscEnqueue = \addr event -> lowerIO . qdiscEnqueue qdisc (C.concreteEvent event)
    }
