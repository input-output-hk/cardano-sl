module Explorer.State where

import Prelude
import Data.Lens (Lens')
import Explorer.I18n.Lang (Language(..))
import Explorer.Lenses.State (blocksExpanded, dashboard, selectedApiCode, viewStates)
import Explorer.Routes (Route(..))
import Explorer.Types.State (DashboardAPICode(..), State, DashBoardViewState)


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

dashboardViewState :: Lens' State DashBoardViewState
dashboardViewState = viewStates <<< dashboard

dashboardBlocksExpanded :: Lens' State Boolean
dashboardBlocksExpanded = dashboardViewState <<< blocksExpanded

dashboardSelectedApiCode :: Lens' State DashboardAPICode
dashboardSelectedApiCode = dashboardViewState <<< selectedApiCode
