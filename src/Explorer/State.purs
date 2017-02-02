module Explorer.State where

import Prelude
import Data.Lens (Lens')
import Explorer.I18n.Lang (Language(..))
import Explorer.Lenses.State (blocksExpanded, dashboard, selectedApiCode, transactionsExpanded, viewStates)
import Explorer.Routes (Route(..))
import Explorer.Types.State (DashboardAPICode(..), State, DashboardViewState)


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

-- dashboard lenses

dashboardViewState :: Lens' State DashboardViewState
dashboardViewState = viewStates <<< dashboard

dashboardBlocksExpanded :: Lens' State Boolean
dashboardBlocksExpanded = dashboardViewState <<< blocksExpanded

dashboardTransactionsExpanded :: Lens' State Boolean
dashboardTransactionsExpanded = dashboardViewState <<< transactionsExpanded

dashboardSelectedApiCode :: Lens' State DashboardAPICode
dashboardSelectedApiCode = dashboardViewState <<< selectedApiCode
