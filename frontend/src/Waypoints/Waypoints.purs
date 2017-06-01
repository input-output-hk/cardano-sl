module Waypoints where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic (class Generic, gShow)

foreign import data WAYPOINT :: !
foreign import data Waypoint :: *

foreign import waypointImpl :: forall eff. Fn2 WaypointSelector (WaypointHandler eff)
    (Eff (waypoint :: WAYPOINT | eff) Waypoint)

newtype WaypointSelector = WaypointSelector String
-- type WaypointHandler eff = Eff (waypoint :: WAYPOINT | eff) Unit
type WaypointHandler eff = WaypointDirection -> Eff (waypoint :: WAYPOINT | eff) Unit

newtype WaypointDirection = WaypointDirection String

-- TODO(jk) Use sum type to compare WaypointDirection
-- type WaypointDirectionUp = "up"
-- type WaypointDirectionDown = "down"
--
-- data WaypointDirections
--     = WaypointDirectionUp
--     | WaypointDirectionDown

derive instance gWaypointDirection :: Generic WaypointDirection
instance sWaypointDirection :: Show WaypointDirection where
    show = gShow

waypoint :: forall eff. WaypointSelector -> (WaypointHandler eff) ->
    Eff (waypoint :: WAYPOINT | eff) Waypoint
waypoint = runFn2 waypointImpl
