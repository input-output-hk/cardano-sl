module Waypoints where

import Prelude
import Control.Monad.Eff (Eff)
import DOM.Node.Types (ElementId)
import Data.Function.Eff (EffFn3, runEffFn3)
import Data.Generic (class Generic, gShow)

foreign import data WAYPOINT :: !
foreign import data Waypoint :: *


-- type WaypointHandler eff = Eff (waypoint :: WAYPOINT | eff) Unit
type WaypointHandler eff = WaypointDirection -> Eff (waypoint :: WAYPOINT | eff) Unit

type WaypointOffset = Int

defaultWaypointOffset :: WaypointOffset
defaultWaypointOffset = 0


-- type WPOffsetPixel = Int
-- type WPOffsetPercentage = String

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

foreign import waypointImpl :: forall eff. EffFn3 (waypoint :: WAYPOINT | eff) ElementId (WaypointHandler eff) WaypointOffset Waypoint

-- | Initialize a `Waypoint`
waypoint :: forall eff. ElementId -> (WaypointHandler eff) -> Eff (waypoint :: WAYPOINT | eff) Waypoint
waypoint elemId handler = runEffFn3 waypointImpl elemId handler defaultWaypointOffset

-- | Initialize a `Waypoint` adding an offset, too
waypoint' :: forall eff. ElementId -> (WaypointHandler eff) -> WaypointOffset -> Eff (waypoint :: WAYPOINT | eff) Waypoint
waypoint' = runEffFn3 waypointImpl
