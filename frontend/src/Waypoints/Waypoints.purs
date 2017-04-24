module Waypoints where

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2, runFn2)
import Prelude

foreign import data WAYPOINT :: !
foreign import data Waypoint :: *

foreign import waypointImpl :: forall a eff. Fn2 WaypointSelector (WaypointHandler eff a)
    (Eff (waypoint :: WAYPOINT | eff) Waypoint)

newtype WaypointSelector = WaypointSelector String
type WaypointHandler eff a = Eff (waypoint :: WAYPOINT | eff) (a Unit)

waypoint :: forall a eff. WaypointSelector -> (WaypointHandler eff a) ->
    Eff (waypoint :: WAYPOINT | eff) Waypoint
waypoint = runFn2 waypointImpl
