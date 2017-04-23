module Explorer.View.Dashboard.Transactions (transactionsView) where

import Prelude
import Data.Array (length, null, slice)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.NominalDiffTime.Lenses (_NominalDiffTime)
import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (dbExploreTransactions, cCollapse, cNoData, cExpand, common, dashboard, cTransactionFeed, cDateFormat) as I18nL
import Explorer.Lenses.State (lang, latestTransactions)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), State, CTxEntries)
import Explorer.Util.Time (prettyDate)
import Explorer.View.Common (currencyCSSClass, noData)
import Explorer.View.Dashboard.Lenses (dashboardTransactionsExpanded)
import Explorer.View.Dashboard.Shared (headerView)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))
import Pos.Core.Lenses.Types (_Coin, getCoin)
import Pos.Explorer.Web.ClientTypes (CTxEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (cteId, cteAmount, cteTimeIssued, _CTxId, _CHash)
import Pux.Html (Html, div, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Html.Events (onClick, MouseEvent) as P
import Pux.Router (link) as P

maxTransactionRows :: Int
maxTransactionRows = 10

minTransactionRows :: Int
minTransactionRows = 5

transactionsView :: State -> P.Html Action
transactionsView state =
    P.div
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
          [ P.className "explorer-dashboard__container" ]
          [ headerView state headerOptions
          , P.div
              [ P.className $ "transactions__waiting" <> visibleWaitingClazz ]
              [ P.text $ translate (I18nL.common <<< I18nL.cNoData) lang' ]
          , P.div
              [ P.className $ "transactions__container" <> visibleTxClazz ]
              $ map (transactionRow state) $ transactions'
          , P.div
            [ P.className $ "transactions__footer" <> visibleTxClazz ]
            [ P.div
                [ P.className $ "btn-expand" <> visibleBtnExpandClazz
                , P.onClick clickHandler
                ]
                [ P.text expandLabel]
            ]
          ]
        ]
    where
      lang' = state ^. lang
      expanded = state ^. dashboardTransactionsExpanded
      expandable = length transactions > minTransactionRows
      expandLabel = if expanded
          then translate (I18nL.common <<< I18nL.cCollapse) lang'
          else translate (I18nL.common <<< I18nL.cExpand) lang'
      headerOptions = HeaderOptions
          { headline: translate (I18nL.common <<< I18nL.cTransactionFeed) lang'
          , link: Just $ HeaderLink { label: translate (I18nL.dashboard <<< I18nL.dbExploreTransactions) lang'
                                    , action: NoOp
                                    }
          }
      transactions = state ^. latestTransactions
      noTransactions = null transactions
      visibleTxClazz = if noTransactions then " hide" else ""
      visibleWaitingClazz = if not noTransactions then " hide" else ""
      visibleBtnExpandClazz = if expandable then "" else " disabled"

      clickHandler :: P.MouseEvent -> Action
      clickHandler _ =
        if expandable
        then DashboardExpandTransactions $ not expanded
        else NoOp

      transactions' :: CTxEntries
      transactions' = if expanded
          then slice 0 maxTransactionRows transactions
          else slice 0 minTransactionRows transactions

transactionRow :: State -> CTxEntry -> P.Html Action
transactionRow state (CTxEntry entry) =
    let format = translate (I18nL.common <<< I18nL.cDateFormat) $ state ^. lang
        dateValue = fromMaybe noData <<< prettyDate format $ entry ^. cteTimeIssued
    in
    P.div
        [ P.className "transactions__row" ]
        [ P.link (toUrl <<< Tx $ entry ^. cteId)
              [ P.className "transactions__column hash" ]
              [ P.text $ entry ^. (cteId <<< _CTxId <<< _CHash) ]
        , transactionColumn dateValue "date"
        , transactionColumn (show $ entry ^. (cteAmount <<< _Coin <<< getCoin))
              <<< currencyCSSClass $ Just ADA
        ]

transactionColumn :: String -> String -> P.Html Action
transactionColumn value clazzName =
    P.div
        [ P.className $ "transactions__column " <> clazzName ]
        [ P.text value ]
