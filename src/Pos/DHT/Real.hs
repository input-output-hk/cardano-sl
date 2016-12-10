{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-| Implementation of peer discovery using using Kademlia Distributed Hash Table.
    For more details regarding DHT see this package on hackage:
    <https://hackage.haskell.org/package/kademlia>
-}

module Pos.DHT.Real
       ( module Pos.DHT.Real.Types
       , module Pos.DHT.Real.Real
       ) where

import           Pos.DHT.Real.Real
import           Pos.DHT.Real.Types
