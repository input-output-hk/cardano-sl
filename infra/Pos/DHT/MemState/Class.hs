{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.DHT.MemState.Class
       ( MonadDhtMem
       , askDhtMem
       ) where

import qualified Control.Monad.Ether.Implicit as Ether
import           Pos.DHT.MemState.Types       (DhtContext)

type MonadDhtMem = Ether.MonadReader DhtContext

askDhtMem :: MonadDhtMem m => m DhtContext
askDhtMem = Ether.ask
