{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-| Implementation of peer discovery using using Kademlia Distributed Hash Table.
    For more details regarding DHT see this package on hackage:
    <https://hackage.haskell.org/package/kademlia>
-}

module Pos.NewDHT.Real
       ( module Pos.NewDHT.Real.Types
       , module Pos.NewDHT.Real.Real
       ) where

import           Pos.NewDHT.Real.Real
import           Pos.NewDHT.Real.Types
