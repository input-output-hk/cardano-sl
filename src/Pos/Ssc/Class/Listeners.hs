-- | Listeners for network events in SSC algorithm implementation.

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Data.Tagged             (Tagged)

import           Pos.Communication.Relay (Relay)
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.WorkMode.Class      (WorkMode)


-- | Class for defining listeners in DHT @SSC@ implementation.
class Ssc ssc => SscListenersClass ssc where
    sscRelays
        :: WorkMode ssc m
        => Tagged ssc [Relay m]
