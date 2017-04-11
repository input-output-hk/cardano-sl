module Explorer.View.Dashboard.Hero (heroView) where

import Prelude
import Data.Int (fromString)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Tuple (Tuple(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cAddress, cTransaction, common, hero, hrTitle, hrSearch, hrSubtitle, hrTime) as I18nL
import Explorer.Lenses.State (lang, searchInput, searchQuery, searchTimeQuery, selectedSearch)
import Explorer.State (maxSlotInEpoch)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (Search(..), State)
import Explorer.View.Dashboard.Lenses (dashboardViewState)
import Pux.Html (Html, div, text, h1, h2, ul, li, label, input) as P
import Pux.Html.Attributes (checked, className, htmlFor, id_, maxLength, name, type_, placeholder, value) as P
import Pux.Html.Events (onChange, onClick, onFocus, onBlur, onKey) as P

inputEpochName :: String
inputEpochName = "inp_epoch"

inputSlotName :: String
inputSlotName = "inp_slot"

heroView :: State -> P.Html Action
heroView state =
    let
        searchInputFocused = state ^. (dashboardViewState <<< searchInput)
        selectedSearch' = state ^. selectedSearch
        focusedClazz = if searchInputFocused then " focused " else ""
        addrHiddenClazz = if selectedSearch' == SearchTime  then " hide " else ""
        epochHiddenClazz = if selectedSearch' /= SearchTime  then " hide " else ""
        searchIconClazz = if searchInputFocused then " bg-icon-search-hover" else " bg-icon-search"
        lang' = state ^. lang
    in
    P.div
        [ P.className "explorer-dashboard__hero" ]
        [ P.div
            [ P.className "hero-container" ]
            [ P.h1
                [ P.className "hero-headline" ]
                [ P.text $ translate (I18nL.hero <<< I18nL.hrTitle) lang' ]
            , P.h2
                [ P.className "hero-subheadline"]
                [ P.text $ translate (I18nL.hero <<< I18nL.hrSubtitle) lang' ]
            , P.div
                [ P.className $ "hero-search__container" <> focusedClazz ]
                [ P.input
                    [ P.className $ "hero-search__input hero-search__input--address-tx"
                                  <> addrHiddenClazz
                                  <> focusedClazz
                    , P.type_ "text"
                    , P.placeholder $ if searchInputFocused
                                      then ""
                                      else translate (I18nL.hero <<< I18nL.hrSearch) lang'
                    , P.onFocus <<< const $ DashboardFocusSearchInput true
                    , P.onBlur <<< const $ DashboardFocusSearchInput false
                    , P.onChange $ UpdateSearchValue <<< _.value <<< _.target
                    , P.onKey "enter" $ const DashboardSearch
                    , P.value $ state ^. searchQuery
                    ]
                    []
                , P.div
                    [ P.className $ "hero-search__wrapper" <> epochHiddenClazz ]
                    [ P.label
                        [ P.className $ "hero-search__label"
                        , P.htmlFor inputEpochName
                        ]
                        [ P.text "Epoch" ]
                    , P.input
                        [ P.className $ "hero-search__input hero-search__input--epoch"
                                      <> focusedClazz
                        , P.type_ "text"
                        , P.name inputEpochName
                        , P.onFocus <<< const $ DashboardFocusSearchInput true
                        , P.onBlur <<< const $ DashboardFocusSearchInput false
                        , P.onChange $ UpdateSearchEpochValue <<< _.value <<< _.target
                        , P.onKey "enter" $ const DashboardSearchTime
                        , P.value $ case state ^. searchTimeQuery of
                                    Tuple (Just epoch) _ -> show epoch
                                    _ -> ""
                        ]
                        []
                    , P.label
                        [ P.className $ "hero-search__label"
                        , P.htmlFor inputSlotName
                        ]
                        [ P.text "Slot" ]
                    , P.input
                        [ P.className $ "hero-search__input hero-search__input--slot"
                                      <> focusedClazz
                        , P.type_ "text"
                        , P.name inputSlotName
                        , P.maxLength <<< show <<< length $ show maxSlotInEpoch
                        , P.onFocus <<< const $ DashboardFocusSearchInput true
                        , P.onBlur <<< const $ DashboardFocusSearchInput false
                        , P.onChange $ UpdateSearchSlotValue <<< _.value <<< _.target
                        , P.onKey "enter" $ const DashboardSearchTime
                        , P.value $ case state ^. searchTimeQuery of
                                    Tuple _ (Just slot) -> show slot
                                    _ -> ""
                        ]
                        []
                    ]
                , P.ul
                    [ P.className "hero-search-nav__container"]
                    <<< map (\item -> searchItemView item $ state ^. selectedSearch)
                        $ mkSearchItems lang'
                , P.div
                    [ P.className $ "hero-search__btn" <> searchIconClazz <> focusedClazz
                    , P.onClick <<< const $ if selectedSearch' == SearchTime
                                            then DashboardSearchTime
                                            else DashboardSearch
                    ]
                    []
                ]
            ]
        ]


type SearchItem =
  { value :: Search
  , label :: String
  }

type SearchItems = Array SearchItem

mkSearchItems :: Language -> SearchItems
mkSearchItems lang =
    [ { value: SearchAddress
      , label: translate (I18nL.common <<< I18nL.cAddress) lang
      }
    , { value: SearchTx
      , label: translate (I18nL.common <<< I18nL.cTransaction) lang
      }
    , { value: SearchTime
      , label: translate (I18nL.hero <<< I18nL.hrTime) lang
      }
    ]

searchItemView :: SearchItem -> Search -> P.Html Action
searchItemView item selectedSearch =
    let selected = item.value == selectedSearch
        selectedClass = if selected then " selected" else ""
    in
    P.li
        [ P.className "hero-search-nav__item" ]
        [ P.input
            [ P.type_ "radio"
            , P.id_ $ show item.value
            , P.name $ show item.value
            , P.onChange <<< const $ UpdateSelectedSearch item.value
            , P.checked $ item.value == selectedSearch
            , P.value $ show item.value
            ]
            []
        , P.label
            [ P.className $ "hero-search-nav__item--label" <> selectedClass
            , P.htmlFor $ show item.value ]
            [ P.text item.label ]
        ]
