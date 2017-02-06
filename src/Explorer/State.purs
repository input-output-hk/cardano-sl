module Explorer.State where

import Control.SocketIO.Client (Host)
import Explorer.I18n.Lang (Language(..))
import Explorer.Routes (Route(..))
import Explorer.Types.State (DashboardAPICode(..), State)


initialState :: State
initialState =
    { lang: English
    , route: Dashboard
    , socket:
        { connected: false
        }
    , viewStates:
        { dashboard:
            { blocksExpanded: false
            , transactionsExpanded: false
            , selectedApiCode: Curl
            , searchInput: false
            }
        }
    }

hostname :: Host
hostname = "http://localhost:9999"
