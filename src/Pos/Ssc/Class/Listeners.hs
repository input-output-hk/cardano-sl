-- | Listeners for network events in SSC algorithm implementation.

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Data.Tagged           (Tagged)

import           Node                  (Listener)
import           Pos.Binary.Class      (Bi)
import           Pos.Communication.BiP (BiP)
import           Pos.Ssc.Class.Types   (Ssc (..))
import           Pos.WorkMode          (NewWorkMode)

-- | Class for defining listeners in DHT @SSC@ implementation.
class Ssc ssc => SscListenersClass ssc where
    sscListeners
        :: ( NewWorkMode ssc m
           )
        => Tagged ssc [Listener BiP m]
