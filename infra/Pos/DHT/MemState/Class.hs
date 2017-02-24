module Pos.DHT.MemState.Class
       ( MonadDhtMem (..)
       ) where

import           Pos.DHT.MemState.Types (DhtContext)

class MonadDhtMem m where
    askDhtMem :: m DhtContext
