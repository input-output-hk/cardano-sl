-- | Listeners for network events in SSC algorithm implementation.

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Data.Proxy                       (Proxy (..))
import           Data.Tagged                      (Tagged)
import           Node                             (Listener)
import           System.Wlog                      (WithLogger)

import           Pos.Communication.BiP            (BiP)
import           Pos.Communication.Types.Protocol (PeerId)
import           Pos.Ssc.Class.Types              (Ssc (..))
import           Pos.WorkMode                     (WorkMode)


-- | Class for defining listeners in DHT @SSC@ implementation.
class Ssc ssc => SscListenersClass ssc where
    sscListeners
        :: WorkMode ssc m
        => Tagged ssc [Listener BiP PeerId m]

    sscStubListeners
        :: WithLogger m
        => Proxy ssc -> [Listener BiP PeerId m]
