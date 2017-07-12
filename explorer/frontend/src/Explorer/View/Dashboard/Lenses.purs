module Explorer.View.Dashboard.Lenses where

import Prelude
import Data.Lens (Lens')
import Explorer.Lenses.State (dbViewBlocksExpanded, dashboard, dbViewSelectedApiCode, dbViewTxsExpanded, viewStates)
import Explorer.Types.State (DashboardAPICode, DashboardViewState, State)

-- lenses

dashboardViewState :: Lens' State DashboardViewState
dashboardViewState = viewStates <<< dashboard

dashboardBlocksExpanded :: Lens' State Boolean
dashboardBlocksExpanded = dashboardViewState <<< dbViewBlocksExpanded

dashboardTransactionsExpanded :: Lens' State Boolean
dashboardTransactionsExpanded = dashboardViewState <<< dbViewTxsExpanded

dashboardSelectedApiCode :: Lens' State DashboardAPICode
dashboardSelectedApiCode = dashboardViewState <<< dbViewSelectedApiCode
