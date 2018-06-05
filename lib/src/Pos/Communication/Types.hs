-- | Types used for communication.

module Pos.Communication.Types
       ( -- * Messages and socket state
         module Pos.Infra.Communication.Types.Protocol
       , module Pos.Infra.Communication.Types.Relay
       , module Pos.Txp.Network.Types
       ) where

import           Pos.Infra.Communication.Types.Protocol
import           Pos.Infra.Communication.Types.Relay
import           Pos.Txp.Network.Types
