module Explorer.View.Dashboard.Lenses where

import Prelude
import Data.Lens (Lens')
import Explorer.Lenses.State (blocksExpanded, dashboard, selectedApiCode, transactionsExpanded, viewStates)
import Explorer.Types.State (DashboardAPICode, DashboardViewState, State)

-- lenses

dashboardViewState :: Lens' State DashboardViewState
dashboardViewState = viewStates <<< dashboard

dashboardBlocksExpanded :: Lens' State Boolean
dashboardBlocksExpanded = dashboardViewState <<< blocksExpanded

dashboardTransactionsExpanded :: Lens' State Boolean
dashboardTransactionsExpanded = dashboardViewState <<< transactionsExpanded

dashboardSelectedApiCode :: Lens' State DashboardAPICode
dashboardSelectedApiCode = dashboardViewState <<< selectedApiCode
