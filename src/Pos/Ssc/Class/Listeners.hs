-- | Listeners for network events in SSC algorithm implementation.

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Data.Tagged                   (Tagged)

import           Pos.Communication.Types.State (MutPeerState)
import           Pos.DHT.Model                 (ListenerDHT (..), MonadDHTDialog)
import           Pos.Ssc.Class.Types           (Ssc (..))
import           Pos.WorkMode                  (WorkMode)

-- | Class for defining listeners in DHT @SSC@ implementation.
class Ssc ssc => SscListenersClass ssc where
    sscListeners :: (MonadDHTDialog (MutPeerState ssc) m, WorkMode ssc m)
                 => Tagged ssc [ListenerDHT (MutPeerState ssc) m]
