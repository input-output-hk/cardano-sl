-- | Listeners for network events in SSC algorithm implementation.

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Pos.Communication.Relay (Relay)
import           Pos.Ssc.Mode            (SscMode)


-- | Class for defining listeners in DHT @SSC@ implementation.
class SscListenersClass where
    sscRelays
        :: SscMode ctx m
        => [Relay m]
