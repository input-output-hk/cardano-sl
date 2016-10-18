-- | Function related to Mpc messages.

module Pos.Types.Mpc
       ( verifyOpening
       ) where

import           Universum

import           Pos.Types.Types (Commitment, Opening)

verifyOpening :: Commitment -> Opening -> Bool
verifyOpening = notImplemented
