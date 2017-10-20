-- | Listeners for network events in SSC algorithm implementation.

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Pos.Communication.Relay (Relay)
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Mode            (SscMode)


-- | Class for defining listeners in DHT @SSC@ implementation.
class Ssc => SscListenersClass where
    sscRelays
        :: SscMode ctx m
        => [Relay m]
