-- | Listeners for network events in SSC algorithm implementation.

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Data.Tagged             (Tagged)

import           Pos.Communication.Relay (Relay)
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Mode            (SscMode)


-- | Class for defining listeners in DHT @SSC@ implementation.
class Ssc ssc => SscListenersClass ssc where
    sscRelays
        :: SscMode ssc m
        => Tagged ssc [Relay m]
