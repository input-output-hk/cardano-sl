-- | Txp communication layer re-exports.

module Pos.Txp.Network
       ( module Pos.Txp.Network.Listeners
       , module Pos.Txp.Network.Types
       ) where

import           Pos.Arbitrary.Txp.Network ()
import           Pos.Txp.Network.Listeners
import           Pos.Txp.Network.Types
