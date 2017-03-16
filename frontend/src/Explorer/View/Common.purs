module Explorer.View.Common (
    placeholderView
    , transactionHeaderView
    , transactionHeaderView'
    , transactionBodyView
    , transactionBodyView'
    , emptyTxHeaderView
    , currencyCSSClass
    , paginationView
    , transactionPaginationView
    ) where

import Prelude
import Data.Int (binary, fromString, toStringAs)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.NominalDiffTime.Lenses (_NominalDiffTime)
import Data.Tuple (Tuple(..))
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), State)
import Explorer.Util.Factory (mkCAddress)
import Pos.Core.Lenses.Types (_Coin, getCoin)
import Pos.Core.Types (Coin(..))
import Pos.Explorer.Web.ClientTypes (CAddress(..), CTxEntry(..), CTxSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, _CHash, _CTxId, cteAmount, cteId, cteTimeIssued, ctsId, ctsInputs, ctsOutputs, ctsTotalInput, ctsTxTimeIssued)
import Pux.Html (Html, text, div, a, p, span, input) as P
import Pux.Html.Attributes (className, href, value, disabled, type_, min, max) as P
import Pux.Html.Events (onChange, onFocus, FormEvent, MouseEvent, Target, onClick) as P
import Pux.Router (link) as P

-- transactions


emptyTxHeaderView :: State -> P.Html Action
emptyTxHeaderView _ =
    P.div
        [ P.className "transaction-header"]
        [ ]

transactionHeaderView :: CTxEntry -> P.Html Action
transactionHeaderView (CTxEntry entry) =
    P.div
          [ P.className "transaction-header"]
          [ P.link
              (toUrl Dashboard )
              [ P.className "hash" ]
              [ P.text $ entry ^. (cteId <<< _CTxId <<< _CHash) ]
          , P.div
              [ P.className "date"]
              [ P.text <<< show $ entry ^. (cteTimeIssued <<< _NominalDiffTime)
              ]
          , P.div
              [ P.className "amount-container" ]
              [ P.a
                  [ P.className "amount bg-ada"
                  , P.href "#" ]
                  [ P.text <<< show $ entry ^. (cteAmount <<< _Coin <<< getCoin) ]
              ]
          ]

transactionHeaderView' :: CTxSummary -> P.Html Action
transactionHeaderView' (CTxSummary txSummary) =
    P.div
          [ P.className "transaction-header"]
          [ P.div
              [ P.className "hash" ]
              [ P.text $ txSummary ^. (ctsId <<< _CTxId <<< _CHash) ]
          , P.div
              [ P.className "date"]
              [ P.text <<< show $ txSummary ^. (ctsTxTimeIssued <<< _NominalDiffTime)
              ]
          , P.div
              [ P.className "amount-container" ]
              [ P.a
                  [ P.className "amount bg-ada"
                  , P.href "#" ]
                  [ P.text <<< show $ txSummary ^. (ctsTotalInput <<< _Coin <<< getCoin) ]
              ]
          ]

transactionBodyView :: State -> P.Html Action
transactionBodyView state =
    P.div
        [ P.className "transaction-body" ]
        [ P.div
          [ P.className "from-hash-container" ]
          [ P.a
              [ P.className "from-hash", P.href "#" ]
              [ P.text "zrVjWkH9pgc9Ng13dXD6C4KQVqnZGFTmuZ" ]
          ]
        , P.div
              [ P.className "to-hash-container bg-transaction-arrow" ]
              [ P.link (toUrl <<< Address $ mkCAddress "1NPj2Y8yswHLuw8Yr1FDdobKAW6WVkUZy9")
                    [ P.className "to-hash"]
                    [ P.text "1NPj2Y8yswHLuw8Yr1FDdobKAW6WVkUZy9" ]
              , P.link (toUrl <<< Address $ mkCAddress "1NPj2Y8yswHLuw8Yr1FDdobKasdfadsfaf")
                    [ P.className "to-hash"]
                    [ P.text "1NPj2Y8yswHLuw8Yr1FDdobKasdfadsfaf" ]
              , P.link (toUrl <<< Address $ mkCAddress "1NPj2Y8yswHLuw8Yr1FDdobKasdfadsfaf")
                    [ P.className "to-hash"]
                    [ P.text "1NPj2Y8yswHLuw8Yr1FDdobKasdfadsfaf" ]
              ]
        , P.div
              [ P.className "to-alias-container" ]
              [ P.p
                  [ P.className "to-alias" ]
                  [ P.text "to red" ]
              , P.p
                  [ P.className "to-alias" ]
                  [ P.text "to blue" ]
              , P.p
                  [ P.className "to-alias" ]
                  [ P.text "to grey" ]
              ]
        , P.div
              [ P.className "amount-container" ]
              [ P.span
                  [ P.className "amount bg-ada-dark" ]
                  [ P.text "131,100"]
              ]
        ]

transactionBodyView' :: CTxSummary -> P.Html Action
transactionBodyView' (CTxSummary txSummary) =
    P.div
        [ P.className "transaction-body" ]
        [ P.div
            [ P.className "from-hash-container" ]
            <<< map transactionFromView $ txSummary ^. ctsInputs
        , P.div
            [ P.className "to-hash-container bg-transaction-arrow" ]
            <<< map transactionToView $ txSummary ^. ctsOutputs
        , P.div
            [ P.className "to-alias-container" ]
            <<< map transactionAliasView $ txSummary ^. ctsOutputs
        , P.div
              [ P.className "amount-container" ]
              <<< map transactionCoinView $ txSummary ^. ctsOutputs
        ]


transactionFromView :: Tuple CAddress Coin -> P.Html Action
transactionFromView (Tuple (CAddress cAddress) _) =
    P.link (toUrl <<< Address $ mkCAddress cAddress)
        [ P.className "from-hash" ]
        [ P.text cAddress ]

transactionToView :: Tuple CAddress Coin -> P.Html Action
transactionToView (Tuple (CAddress cAddress) _) =
    P.link (toUrl <<< Address $ mkCAddress cAddress)
          [ P.className "to-hash"]
          [ P.text cAddress ]

transactionAliasView :: Tuple CAddress Coin -> P.Html Action
transactionAliasView (Tuple (CAddress cAddress) (Coin coin)) =
    P.p
        [ P.className "to-alias" ]
        [ P.text "to red" ]

transactionCoinView :: Tuple CAddress Coin -> P.Html Action
transactionCoinView (Tuple _ (Coin coin)) =
    P.div
        [ P.className "amount-wrapper" ]
        [ P.span
            [ P.className "amount bg-ada-dark" ]
            [ P.text <<< show $ coin ^. getCoin ]
        ]

-- pagination

type PaginationViewProps =
    { label :: String
    , currentPage :: Int
    , maxPage :: Int
    , changePageAction :: (Int -> Action)
    , onFocusAction :: (P.Target -> Action)
    }

transactionPaginationView :: PaginationViewProps -> P.Html Action
transactionPaginationView props =
    P.div
        [ P.className "transaction-pagination"]
        [ paginationView props ]

paginationView :: PaginationViewProps -> P.Html Action
paginationView props =
    P.div
        [ P.className "pagination" ]
        [ P.div
            [ P.className "pagination__wrapper" ]
            [ P.div
                [ P.className $ "btn-page" <> disablePrevBtnClazz
                , P.onClick prevClickHandler ]
                [ P.div
                    [ P.className "icon bg-triangle-left" ]
                    []
                ]
            , P.input
                [ P.className "page-number"
                , P.value <<< show $ props.currentPage
                , P.disabled $ props.maxPage == minPage
                , P.min $ toStringAs binary minPage
                , P.max $ toStringAs binary props.maxPage
                , P.onChange changeHandler
                , P.onFocus $ props.onFocusAction <<< _.target
                ]
                []
            , P.p
                [ P.className "label" ]
                [ P.text props.label ]
            , P.input
                [ P.className "page-number"
                , P.disabled true
                , P.type_ "number"
                , P.value $ show props.maxPage
                ]
                []
            , P.div
                [ P.className $ "btn-page" <> disableNextBtnClazz
                  , P.onClick nextClickHandler ]
                [ P.div
                    [ P.className "icon bg-triangle-right" ]
                    []
                ]
            ]
        ]
        where
          minPage = 1
          disablePrevBtnClazz = if props.currentPage == minPage then " disabled" else ""
          disableNextBtnClazz = if props.currentPage == props.maxPage then " disabled" else ""
          nextClickHandler :: P.MouseEvent -> Action
          nextClickHandler _ =
              if props.currentPage < props.maxPage then
              props.changePageAction $ props.currentPage + 1
              else
              NoOp

          prevClickHandler :: P.MouseEvent -> Action
          prevClickHandler _ =
              if props.currentPage > minPage then
              props.changePageAction $ props.currentPage - 1
              else
              NoOp

          changeHandler :: P.FormEvent -> Action
          changeHandler ev =
              let value = fromMaybe props.currentPage <<< fromString <<< _.value $ _.target ev in
              if value >= minPage && value <= props.maxPage
              then props.changePageAction value
              else NoOp


-- helper

currencyCSSClass :: Maybe CCurrency -> String
currencyCSSClass mCurrency =
  case mCurrency of
      Just ADA -> " ada bg-ada-dark"
      Just USD -> " usd bg-usd-dark"
      _ -> ""

-- TODO (jk) Remove placeholderView if all views are implemented
placeholderView :: String -> P.Html Action
placeholderView label =
    P.div
        [ P.className "explorer-dashboard__content" ]
        [ P.text label ]
