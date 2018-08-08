module Explorer.View.Dashboard.Transactions
    ( transactionsView
    , maxTransactionRows
    ) where

import Prelude

import Data.Array (length, slice)
import Data.Foldable (for_)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)

import Network.RemoteData (RemoteData(..), isSuccess)

import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (dbExploreTransactions, cCollapse, cNoData, cExpand, common, dashboard, cTransactionFeed, cDateFormat) as I18nL
import Explorer.Lenses.State (lang, latestTransactions)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), State, CTxEntries)
import Explorer.Util.String (formatADA)
import Explorer.Util.Time (prettyDate)
import Explorer.View.Common (currencyCSSClass, noData)
import Explorer.View.Dashboard.Lenses (dashboardTransactionsExpanded)
import Explorer.View.Dashboard.Shared (headerView)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))

import Pos.Explorer.Web.ClientTypes (CTxEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (cteId, cteAmount, cteTimeIssued, _CTxId, _CHash)

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.Events (onClick, DOMEvent) as P

import Text.Smolder.HTML (a, div, span, table, thead, tbody, tr, td, th) as S
import Text.Smolder.HTML.Attributes (className, href) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((#!), (!))

maxTransactionRows :: Int
maxTransactionRows = 10

minTransactionRows :: Int
minTransactionRows = 5

transactionsView :: State -> P.HTML Action
transactionsView state =
  S.div ! S.className "pure-u-1-1" $ do
    headerView state headerOptions
    S.div ! S.className ("transactions__waiting" <> visibleWaitingClazz)
          $ S.text (translate (I18nL.common <<< I18nL.cNoData) lang')
    S.table ! S.className "pure-table pure-table-horizontal" $ do
      S.thead $
        S.tr $ do
          S.th $ S.text "Hash"
          S.th $ S.text "Date"
          S.th $ S.text "Value"
      S.tbody $ for_ transactions' (transactionRow state)
    S.div ! S.className ("transactions__footer" <> visibleTxClazz)
          $ S.a ! S.className ("pure-button pure-button-primary btn-expand" <> visibleBtnExpandClazz)
                  #! P.onClick clickHandler
                  $ S.text expandLabel
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
          , icon: Just "fa-exchange"
          }
      transactions = case state ^. latestTransactions of
                        Success txs -> txs
                        _ -> []
      successTxs = isSuccess $ state ^. latestTransactions
      visibleTxClazz = if successTxs then "" else " hide"
      visibleWaitingClazz = if successTxs then " hide" else ""
      visibleBtnExpandClazz = if expandable then "" else " disabled"

      clickHandler :: P.DOMEvent -> Action
      clickHandler _ =
        if expandable
        then DashboardExpandTransactions $ not expanded
        else NoOp

      transactions' :: CTxEntries
      transactions' = if expanded
          then slice 0 maxTransactionRows transactions
          else slice 0 minTransactionRows transactions

transactionRow :: State -> CTxEntry -> P.HTML Action
transactionRow state (CTxEntry entry) =
    let lang' = state ^. lang
        format = translate (I18nL.common <<< I18nL.cDateFormat) lang'
        dateValue = fromMaybe noData $ prettyDate format =<< entry ^. cteTimeIssued
        txRoute = Tx $ entry ^. cteId
    in
    S.tr $ do
        S.td $
          S.a ! S.href (toUrl txRoute)
              #! P.onClick (Navigate $ toUrl txRoute)
            $ S.text (entry ^. (cteId <<< _CTxId <<< _CHash))
        S.td $ S.text dateValue
        S.td $ S.span  ! S.className (currencyCSSClass $ Just ADA)
          $ S.text (formatADA (entry ^. cteAmount) lang')
