module Explorer.Types.App where

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.SocketIO.Client (SocketIO)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Network.HTTP.Affjax (AJAX)
import Waypoints (WAYPOINT)

type AppEffects eff =
    ( dom :: DOM
    , ajax :: AJAX
    , socket :: SocketIO
    , now :: NOW
    , waypoint :: WAYPOINT
    , history :: HISTORY
    , console :: CONSOLE
    | eff
    )
