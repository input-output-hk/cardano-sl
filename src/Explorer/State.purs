module Explorer.State where

import Explorer.I18n.Lang (Language(..))
import Explorer.Routes (Route(..))
import Explorer.Types.State (DashboardAPICode(..), State)


initialState :: State
initialState =
    { lang: English
    , route: Dashboard
    , viewStates: {
        dashboard:
        { blocksExpanded: false
        , transactionsExpanded: false
        , selectedApiCode: Curl
        }
      }
    }
