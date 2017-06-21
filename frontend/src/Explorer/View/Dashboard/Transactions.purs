module Explorer.View.Dashboard.Transactions
    ( transactionsView
    , maxTransactionRows
    ) where

import Prelude
import Data.Array (length, slice)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Network.RemoteData (RemoteData(..), isSuccess)

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.Events (onClick, MouseEvent) as P

import Text.Smolder.HTML (div, text, span)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (#!), (!))

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


maxTransactionRows :: Int
maxTransactionRows = 10

minTransactionRows :: Int
minTransactionRows = 5

transactionsView :: State -> P.HTML Action
transactionsView state =
    div ! className "explorer-dashboard__wrapper"
        div ! className "explorer-dashboard__container" $ do
            h eaderView state headerOptions
            div ! className $ "transactions__waiting" <> visibleWaitingClazz
                $ text (translate (I18nL.common <<< I18nL.cNoData) lang')
            div ! className ("transactions__container" <> visibleTxClazz)
                $ map (transactionRow state) $ transactions'
            div ! className ("transactions__footer" <> visibleTxClazz) $ do
                div ! className ("btn-expand" <> visibleBtnExpandClazz)
                    #! onClick clickHandler
                    $ text expandLabel
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
      transactions = case state ^. latestTransactions of
                        Success txs -> txs
                        _ -> []
      successTxs = isSuccess $ state ^. latestTransactions
      visibleTxClazz = if successTxs then "" else " hide"
      visibleWaitingClazz = if successTxs then " hide" else ""
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

transactionRow :: State -> CTxEntry -> P.HTML Action
transactionRow state (CTxEntry entry) =
    let lang' = state ^. lang
        format = translate (I18nL.common <<< I18nL.cDateFormat) lang'
        dateValue = fromMaybe noData <<< prettyDate format $ entry ^. cteTimeIssued
        txRoute = Tx $ entry ^. cteId
    in
    div ! className "transactions__row" $ do
        div ! className "transactions__column--hash-container" $ do
            a ! href (toUrl txRoute)
              #! onClick (toUrl txRoute)
              ! className "hash"
              $ text $ entry ^. (cteId <<< _CTxId <<< _CHash)
        div ! className "transactions__column--date"
            $ text dateValue
        div ! className "transactions__column--currency"
            $ span ! className (currencyCSSClass $ Just ADA)
                   $ text (formatADA (entry ^. cteAmount) lang')
