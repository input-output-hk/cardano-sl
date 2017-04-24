{-# LANGUAGE RankNTypes #-}

module Pos.Launcher.Resources
       ( RealModeResources(..)
       , hoistResources
       ) where

import Universum
import Network.Transport.Abstract (Transport, hoistTransport)
import Pos.Communication (NodeId)

data RealModeResources m = RealModeResources
    { rmTransport :: Transport m
    , rmGetPeers :: m (Set NodeId)
    , rmFindPeers :: m (Set NodeId)
    }

hoistResources
  :: ( Functor n )
  => (forall t . m t -> n t)
  -> RealModeResources m
  -> RealModeResources n
hoistResources f res = res
    { rmTransport = hoistTransport f (rmTransport res)
    , rmGetPeers = f (rmGetPeers res)
    , rmFindPeers = f (rmFindPeers res)
    }
