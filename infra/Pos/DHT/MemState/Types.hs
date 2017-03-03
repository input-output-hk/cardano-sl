module Pos.DHT.MemState.Types
       ( DhtContext (..)
       ) where

import           Universum

data DhtContext = DhtContext
    { _dhtKademliadDump :: !FilePath
    }
