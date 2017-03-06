-- | Listeners for network events in SSC algorithm implementation.

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Data.Tagged                (Tagged)
import           System.Wlog                (WithLogger)

import           Pos.Communication.Protocol (ListenerSpec, OutSpecs)
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.WorkMode               (WorkMode)


-- | Class for defining listeners in DHT @SSC@ implementation.
class Ssc ssc => SscListenersClass ssc where
    sscListeners
        :: WorkMode ssc m
        => m (Tagged ssc ([ListenerSpec m], OutSpecs))

    sscStubListeners
        :: WithLogger m
        => Tagged ssc ([ListenerSpec m], OutSpecs)
