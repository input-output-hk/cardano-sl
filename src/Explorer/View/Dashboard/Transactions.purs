module Explorer.View.Dashboard.Transactions (transactionsView) where

import Prelude
import Data.Array (null, slice)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.NominalDiffTime.Lenses (_NominalDiffTime)
import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (dbExploreTransactions, cCollapse, cNoData, cExpand, common, dashboard, cTransactionFeed) as I18nL
import Explorer.Lenses.State (lang, latestTransactions)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), State, CTxEntries)
import Explorer.View.Common (currencyCSSClass)
import Explorer.View.Dashboard.Lenses (dashboardTransactionsExpanded)
import Explorer.View.Dashboard.Shared (headerView)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))
import Pos.Explorer.Web.ClientTypes (CTxEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (cteId, cteAmount, cteTimeIssued, _CTxId, _CHash)
import Pos.Types.Lenses.Core (_Coin, getCoin)
import Pux.Html (Html, div, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Html.Events (onClick) as P
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
                [ P.className "btn-expand"
                , P.onClick <<< const <<< DashboardExpandTransactions $ not expanded ]
                [ P.text expandLabel]
            ]
          ]
        ]
    where
      lang' = state ^. lang
      expanded = state ^. dashboardTransactionsExpanded
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
      visibleTxClazz = if noTransactions then " invisible" else ""
      visibleWaitingClazz = if not noTransactions then " invisible" else ""
      transactions' :: CTxEntries
      transactions' = if expanded
          then slice 0 maxTransactionRows transactions
          else slice 0 minTransactionRows transactions

transactionRow :: State -> CTxEntry -> P.Html Action
transactionRow state (CTxEntry entry) =
    let txId = entry ^. (cteId <<< _CTxId <<< _CHash) in
    P.div
        [ P.className "transactions__row" ]
        [ P.link (toUrl <<< Transaction $ entry ^. cteId <<< _CTxId)
              [ P.className "transactions__column hash" ]
              [ P.text $ entry ^. (cteId <<< _CTxId <<< _CHash) ]
        , transactionColumn (show <<< unwrap $ entry ^. (cteTimeIssued <<< _NominalDiffTime)) ""
        , transactionColumn (show $ entry ^. (cteAmount <<< _Coin <<< getCoin)) <<< currencyCSSClass $ Just ADA
        ]

transactionColumn :: String -> String -> P.Html Action
transactionColumn value clazzName =
    P.div
        [ P.className $ "transactions__column " <> clazzName ]
        [ P.text value ]
